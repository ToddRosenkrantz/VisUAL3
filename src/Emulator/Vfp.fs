/// VFPv4 (D32) floating point instructions, and MRS/MSR.
/// Arithmetic is done by FpMath, so it is exact and identical on .NET and in JavaScript.
module Vfp

open CommonData
open CommonLex
open Errors
open Helpers
open FpMath

type VReg =
    | SReg of int
    | DReg of int

/// What a VFP load or store accesses, for the editor's pointer tooltips
type VfpAccess = {
    IsLoad : bool
    Regs : VReg list // in memory order, from the start address
    Base : RName option // None for a label
    Start : DataPath -> uint32
    After : (DataPath -> uint32) option // the base register after write-back
    }

/// A ready-to-run VFP instruction
type Instr = { Run : DataPath -> Result<DataPath, ExecuteError>; Access : VfpAccess option }

type private Exec = DataPath -> Result<DataPath, ExecuteError>

let private pErr (msg : string) (found : string) : Result<'a, ParseError> = makeFormatError msg found ""
let private rErr (addr : uint32) (msg : string) : Result<'a, ExecuteError> = ``Run time error`` (addr, msg) |> Error

let private map2 f a b =
    match a, b with
    | Ok x, Ok y -> Ok(f x y)
    | Error e, _ | _, Error e -> Error e

// ---------------------------------------------------------------- register file

let wordOf n (dp : DataPath) = Map.tryFind n dp.Fp.W |> Option.defaultValue 0u
let private setWord n v (dp : DataPath) = { dp with Fp = { dp.Fp with W = Map.add n v dp.Fp.W } }

/// the raw bits of an s or d register
let getV r (dp : DataPath) =
    match r with
    | SReg n -> rawOfWord (wordOf n dp)
    | DReg n -> rawOfWords (wordOf (2 * n) dp) (wordOf (2 * n + 1) dp)

let setV r (raw : bigint) (dp : DataPath) =
    match r with
    | SReg n -> setWord n (wordOfRaw raw) dp
    | DReg n ->
        let lo, hi = wordsOfRaw raw
        dp |> setWord (2 * n) lo |> setWord (2 * n + 1) hi

let fmtOf = function | SReg _ -> F32 | DReg _ -> F64
let regName = function | SReg n -> sprintf "s%d" n | DReg n -> sprintf "d%d" n
/// the words of a register, lowest address first
let private wordsOfReg = function | SReg n -> [ n ] | DReg n -> [ 2 * n; 2 * n + 1 ]
let private orFlags fl (dp : DataPath) = { dp with Fp = { dp.Fp with FPSCR = dp.Fp.FPSCR ||| fl } }
let private ctl (dp : DataPath) = ctlOfFpscr dp.Fp.FPSCR

/// write a result and its cumulative exception flags
let private result dst (r : FpRes) (dp : DataPath) = dp |> setV dst r.Bits |> orFlags r.Flags

// ---------------------------------------------------------------- operand parsing

let private neonMsg = "NEON vector instructions are not supported: this core emulates scalar VFP only"

let (|VREG|_|) (t : string) =
    let t = t.Trim().ToLower()
    if t.Length >= 2 && t.Length <= 3 && (t.[0] = 's' || t.[0] = 'd') && t.Substring(1) |> Seq.forall System.Char.IsDigit then
        let n = int (t.Substring 1)
        if n <= 31 then Some(if t.[0] = 's' then SReg n else DReg n) else None
    else None

let private isQReg (t : string) =
    let t = t.Trim().ToLower()
    t.Length >= 2 && t.[0] = 'q' && t.Substring(1) |> Seq.forall System.Char.IsDigit

/// a core register other than PC
let private coreReg (t : string) =
    match parseRegister t with
    | Ok R15 -> pErr "PC cannot be used here" t
    | r -> r

/// split operands on commas outside [] and {}
let splitOperands (txt : string) =
    let parts = ResizeArray<string>()
    let cur = System.Text.StringBuilder()
    let mutable depth = 0
    for c in txt do
        match c with
        | '[' | '{' -> depth <- depth + 1; cur.Append c |> ignore
        | ']' | '}' -> depth <- depth - 1; cur.Append c |> ignore
        | ',' when depth = 0 -> parts.Add(cur.ToString().Trim()); cur.Clear() |> ignore
        | _ -> cur.Append c |> ignore
    let last = cur.ToString().Trim()
    if last <> "" || parts.Count > 0 then parts.Add last
    List.ofSeq parts

let private isNeonType (dt : string) =
    [ "I8"; "I16"; "I32"; "I64"; "S8"; "U8"; "P8"; "8"; "16"; "64" ] |> List.contains dt

/// the format of a .f32/.f64 instruction; a missing type is taken from the first register
let private fpType (root : string) (dts : string list) (ops : string list) =
    match dts, ops with
    | [ "F32" ], _ -> Ok F32
    | [ "F64" ], _ -> Ok F64
    | [], VREG r :: _ -> Ok(fmtOf r)
    | [ dt ], _ when isNeonType dt -> pErr neonMsg (root + "." + dt)
    | _ -> pErr (sprintf "%s needs a type: .f32 (single, s registers) or .f64 (double, d registers)" (root.ToLower()))
                (String.concat "." (root :: dts))

/// a register of the instruction's format
let private fpReg (fmt : FpFormat) (t : string) =
    match t with
    | VREG r when fmtOf r = fmt -> Ok r
    | VREG(DReg _) -> pErr ("a .f32 instruction on d registers is a " + neonMsg + "; single precision uses s0-s31") t
    | VREG(SReg _) -> pErr "double precision (.f64) uses d registers, d0-d31" t
    | t when isQReg t -> pErr neonMsg t
    | t -> pErr (if fmt = F32 then "an s register, s0-s31" else "a d register, d0-d31") t

let private fpRegs fmt (ops : string list) =
    let rec go = function
        | [] -> Ok []
        | o :: rest -> fpReg fmt o |> Result.bind (fun r -> go rest |> Result.map (fun rs -> r :: rs))
    go ops

/// a #float immediate
let private floatImm (fmt : FpFormat) (t : string) =
    let t = t.Trim()
    if t.StartsWith "#" then parseFloat fmt (t.Substring 1) |> Result.mapError (fun e -> e)
    else Error(sprintf "a #constant, e.g. #1.5: %s" t)

// ---------------------------------------------------------------- arithmetic

/// d := f(n, m)
let private binary (f : FpCtl -> FpFormat -> bigint -> bigint -> FpRes) d n m : Exec =
    fun dp -> Ok(result d (f (ctl dp) (fmtOf d) (getV n dp) (getV m dp)) dp)

/// three operands Rd, Rn, Rm; Rd may be left out (vadd.f32 s0, s1 means vadd.f32 s0, s0, s1)
let private arith3 root dts ops f =
    fpType root dts ops
    |> Result.bind (fun fmt ->
        match ops with
        | [ _; _ ] | [ _; _; _ ] ->
            fpRegs fmt ops |> Result.map (function | [ d; m ] -> binary f d d m | rs -> binary f rs.[0] rs.[1] rs.[2])
        | _ -> pErr (sprintf "%s Rd, Rn, Rm" (root.ToLower())) (String.concat ", " ops))

let private negRes fmt (r : FpRes) = { r with Bits = negBits fmt r.Bits }

/// multiply-accumulate, rounding the product and the sum separately (VMLA, VMLS, VNMLA, VNMLS)
let private mac root negProduct negAcc dts ops =
    fpType root dts ops
    |> Result.bind (fun fmt ->
        match ops with
        | [ _; _; _ ] ->
            fpRegs fmt ops
            |> Result.map (fun rs ->
                let d, n, m = rs.[0], rs.[1], rs.[2]
                fun dp ->
                    let c = ctl dp
                    let p = mul c fmt (getV n dp) (getV m dp)
                    let p' = if negProduct then negRes fmt p else p
                    let acc = if negAcc then negBits fmt (getV d dp) else getV d dp
                    let s = add c fmt acc p'.Bits
                    Ok(dp |> orFlags p.Flags |> result d s))
        | _ -> pErr (sprintf "%s Rd, Rn, Rm" (root.ToLower())) (String.concat ", " ops))

/// fused multiply-add, one rounding (VFMA, VFMS, VFNMA, VFNMS)
let private fused root negN negAcc dts ops =
    fpType root dts ops
    |> Result.bind (fun fmt ->
        match ops with
        | [ _; _; _ ] ->
            fpRegs fmt ops
            |> Result.map (fun rs ->
                let d, n, m = rs.[0], rs.[1], rs.[2]
                fun dp ->
                    let op1 = if negN then negBits fmt (getV n dp) else getV n dp
                    let acc = if negAcc then negBits fmt (getV d dp) else getV d dp
                    Ok(result d (mulAdd (ctl dp) fmt acc op1 (getV m dp)) dp))
        | _ -> pErr (sprintf "%s Rd, Rn, Rm" (root.ToLower())) (String.concat ", " ops))

/// Rd := f(Rm)
let private unary root dts ops (f : FpCtl -> FpFormat -> bigint -> FpRes) =
    fpType root dts ops
    |> Result.bind (fun fmt ->
        match ops with
        | [ _; _ ] -> fpRegs fmt ops |> Result.map (fun rs -> fun dp -> Ok(result rs.[0] (f (ctl dp) fmt (getV rs.[1] dp)) dp))
        | _ -> pErr (sprintf "%s Rd, Rm" (root.ToLower())) (String.concat ", " ops))

let private bitsOnly f = fun (_ : FpCtl) (fmt : FpFormat) (raw : bigint) -> { Bits = f fmt raw; Flags = 0u }

/// VCMP/VCMPE Rd, Rm or Rd, #0: sets the FPSCR N, Z, C and V flags
let private vcmp root dts ops =
    fpType root dts ops
    |> Result.bind (fun fmt ->
        let signal = (root = "VCMPE")
        let run (d : VReg) (other : DataPath -> bigint) : Exec =
            fun dp ->
                let nzcv, fl = compare (ctl dp) fmt signal (getV d dp) (other dp)
                Ok { dp with Fp = { dp.Fp with FPSCR = (dp.Fp.FPSCR &&& 0x0FFFFFFFu) ||| nzcv ||| fl } }
        match ops with
        | [ d; z ] when z.Replace(" ", "") = "#0" || z.Replace(" ", "") = "#0.0" ->
            fpReg fmt d |> Result.map (fun d -> run d (fun _ -> zeroBits fmt false))
        | [ _; _ ] -> fpRegs fmt ops |> Result.map (fun rs -> run rs.[0] (getV rs.[1]))
        | _ -> pErr (sprintf "%s Rd, Rm or %s Rd, #0" (root.ToLower()) (root.ToLower())) (String.concat ", " ops))

// ---------------------------------------------------------------- conversions

let private fmtName = function | DReg _ -> "d" | SReg _ -> "s"

/// VCVT, VCVTR, VCVTB, VCVTT
let private vcvt root (dts : string list) (ops : string list) =
    let intType = function
        | "S32" -> Some(true, 32) | "U32" -> Some(false, 32)
        | "S16" -> Some(true, 16) | "U16" -> Some(false, 16)
        | _ -> None
    let fType = function | "F32" -> Some F32 | "F64" -> Some F64 | _ -> None
    let usage = "vcvt.<to>.<from>: .f64.f32, .f32.f64, .s32/.u32.f32/.f64, .f32/.f64.s32/.u32, or fixed point with #fbits"
    let regOf fmt t = fpReg fmt t
    let fbitsOf size (t : string) =
        let t = t.Trim()
        match (if t.StartsWith "#" then parseNumberExpression Map.empty (t.Substring 1) |> Result.toOption else None) with
        | Some n when (size = 16 && n <= 16u) || (size = 32 && n >= 1u && n <= 32u) -> Ok(int n)
        | _ -> pErr (sprintf "the number of fraction bits: #%d to #%d" (if size = 16 then 0 else 1) size) t
    /// write a 16/32-bit fixed value into an s register, or a d register (extended to 64 bits)
    let writeInt (d : VReg) (signed : bool) (w : uint32) dp =
        match d with
        | SReg _ -> setV d (rawOfWord w) dp
        | DReg _ -> setV d (rawOfWords w (if signed && w >= 0x80000000u then 0xFFFFFFFFu else 0u)) dp
    match root, dts, ops with
    | ("VCVTB" | "VCVTT"), [ "F32"; "F16" ], [ d; m ] ->
        map2 (fun d m ->
            fun dp ->
                let w = wordOfRaw (getV m dp)
                let half = if root = "VCVTB" then w &&& 0xFFFFu else w >>> 16
                Ok(result d (convert (ctl dp) F16 F32 (rawOfWord half)) dp)) (regOf F32 d) (regOf F32 m)
    | ("VCVTB" | "VCVTT"), [ "F16"; "F32" ], [ d; m ] ->
        map2 (fun d m ->
            fun dp ->
                let r = convert (ctl dp) F32 F16 (getV m dp)
                let h = wordOfRaw r.Bits &&& 0xFFFFu
                let old = wordOfRaw (getV d dp)
                let w = if root = "VCVTB" then (old &&& 0xFFFF0000u) ||| h else (old &&& 0xFFFFu) ||| (h <<< 16)
                Ok(dp |> setV d (rawOfWord w) |> orFlags r.Flags)) (regOf F32 d) (regOf F32 m)
    | ("VCVTB" | "VCVTT"), _, _ -> pErr (sprintf "%s.f32.f16 Sd, Sm or %s.f16.f32 Sd, Sm" (root.ToLower()) (root.ToLower())) (String.concat ", " ops)
    | "VCVT", [ "F64"; "F32" ], [ d; m ] ->
        map2 (fun d m -> fun dp -> Ok(result d (convert (ctl dp) F32 F64 (getV m dp)) dp)) (regOf F64 d) (regOf F32 m)
    | "VCVT", [ "F32"; "F64" ], [ d; m ] ->
        map2 (fun d m -> fun dp -> Ok(result d (convert (ctl dp) F64 F32 (getV m dp)) dp)) (regOf F32 d) (regOf F64 m)
    // float to integer: Sd, Sm/Dm
    | ("VCVT" | "VCVTR"), [ it; ft ], [ d; m ] when (intType it).IsSome && snd (intType it).Value = 32 && (fType ft).IsSome ->
        let signed, _ = (intType it).Value
        let fmt = (fType ft).Value
        map2 (fun d m ->
            fun dp ->
                let w, fl = toFixed (ctl dp) fmt signed 32 0 (root = "VCVTR") (getV m dp)
                Ok(dp |> setV d (rawOfWord w) |> orFlags fl)) (regOf F32 d) (regOf fmt m)
    // integer to float: Sd/Dd, Sm
    | "VCVT", [ ft; it ], [ d; m ] when (intType it).IsSome && snd (intType it).Value = 32 && (fType ft).IsSome ->
        let signed, _ = (intType it).Value
        let fmt = (fType ft).Value
        map2 (fun d m -> fun dp -> Ok(result d (ofFixed (ctl dp) fmt signed 32 0 (wordOfRaw (getV m dp))) dp))
            (regOf fmt d) (regOf F32 m)
    // fixed point, in place: Rd, Rd, #fbits
    | "VCVT", [ a; b ], [ d; d2; fb ] when ((intType a).IsSome && (fType b).IsSome) || ((fType a).IsSome && (intType b).IsSome) ->
        let toFixedDir = (intType a).IsSome
        let signed, size = (if toFixedDir then intType a else intType b).Value
        let fmt = (if toFixedDir then fType b else fType a).Value
        match regOf fmt d, regOf fmt d2, fbitsOf size fb with
        | Ok r, Ok r2, Ok fbits when r = r2 ->
            Ok(fun dp ->
                if toFixedDir then
                    let w, fl = toFixed (ctl dp) fmt signed size fbits false (getV r dp)
                    Ok(dp |> writeInt r signed w |> orFlags fl)
                else Ok(result r (ofFixed (ctl dp) fmt signed size fbits (wordOfRaw (getV r dp))) dp))
        | Ok _, Ok _, Ok _ -> pErr "fixed-point vcvt converts a register in place: the two registers must be the same" (String.concat ", " ops)
        | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
    | _, ([ dt ] | [ dt; _ ] | [ _; dt ]), _ when isNeonType dt -> pErr neonMsg (String.concat "." (root :: dts))
    | _ -> pErr usage (String.concat "." (root :: dts) + " " + String.concat ", " ops)

// ---------------------------------------------------------------- moves

/// d0[1]: a 32-bit half of a d register
let private (|SCALAR|_|) (t : string) =
    let t = t.Trim().ToLower()
    match t.Split('[') with
    | [| VREG(DReg n); idx |] when idx = "0]" || idx = "1]" -> Some(2 * n + (if idx = "0]" then 0 else 1))
    | _ -> None

let private (|CORE|_|) (t : string) = if isRegister t then Some t else None

let private vmov dts (ops : string list) : Result<Exec, ParseError> =
    let setCore r v dp = Ok(Helpers.setReg r v dp)
    let core (t : string) = coreReg t
    match ops with
    | [ VREG a; VREG b ] when fmtOf a = fmtOf b ->
        fpType "VMOV" dts ops |> Result.bind (fun fmt -> fpRegs fmt ops)
        |> Result.map (fun rs -> fun dp -> Ok(setV rs.[0] (getV rs.[1] dp) dp))
    | [ VREG a; imm ] when imm.Trim().StartsWith "#" ->
        fpType "VMOV" dts ops
        |> Result.bind (fun fmt ->
            fpReg fmt (List.head ops)
            |> Result.bind (fun d ->
                match floatImm fmt imm with
                | Ok raw when isVfpImmediate fmt raw -> Ok(fun dp -> Ok(setV d raw dp))
                | Ok _ ->
                    pErr "a VFP immediate: ±(16 to 31)/16 × 2^n for n from -3 to 4, e.g. #1.0, #0.5, #-2.25, #10.0; load other values with vldr from a .float or .double" imm
                | Error e -> pErr e imm))
    | [ VREG(SReg s); CORE r ] when dts = [] ->
        core r |> Result.map (fun r -> fun dp -> Ok(setV (SReg s) (rawOfWord dp.Regs.[r]) dp))
    | [ CORE r; VREG(SReg s) ] when dts = [] ->
        core r |> Result.map (fun r -> fun dp -> setCore r (wordOf s dp) dp)
    | [ VREG(SReg s); VREG(SReg s2); CORE r; CORE r2 ] when dts = [] ->
        if s2 <> s + 1 then pErr "the two s registers must be consecutive: s0, s1" (String.concat ", " ops)
        else map2 (fun r r2 -> fun dp -> Ok(dp |> setWord s dp.Regs.[r] |> setWord s2 dp.Regs.[r2])) (core r) (core r2)
    | [ CORE r; CORE r2; VREG(SReg s); VREG(SReg s2) ] when dts = [] ->
        if s2 <> s + 1 then pErr "the two s registers must be consecutive: s0, s1" (String.concat ", " ops)
        else
            map2 (fun r r2 -> r, r2) (core r) (core r2)
            |> Result.bind (fun (r, r2) ->
                if r = r2 then pErr "the two core registers must be different" (String.concat ", " ops)
                else Ok(fun dp -> dp |> Helpers.setReg r (wordOf s dp) |> Helpers.setReg r2 (wordOf s2 dp) |> Ok))
    | [ VREG(DReg d); CORE lo; CORE hi ] when dts = [] ->
        map2 (fun lo hi -> fun dp -> Ok(dp |> setWord (2 * d) dp.Regs.[lo] |> setWord (2 * d + 1) dp.Regs.[hi])) (core lo) (core hi)
    | [ CORE lo; CORE hi; VREG(DReg d) ] when dts = [] ->
        map2 (fun lo hi -> lo, hi) (core lo) (core hi)
        |> Result.bind (fun (lo, hi) ->
            if lo = hi then pErr "the two core registers must be different" (String.concat ", " ops)
            else Ok(fun dp -> dp |> Helpers.setReg lo (wordOf (2 * d) dp) |> Helpers.setReg hi (wordOf (2 * d + 1) dp) |> Ok))
    | [ SCALAR w; CORE r ] when dts = [] || dts = [ "32" ] ->
        core r |> Result.map (fun r -> fun dp -> Ok(setWord w dp.Regs.[r] dp))
    | [ CORE r; SCALAR w ] when dts = [] || dts = [ "32" ] ->
        core r |> Result.map (fun r -> fun dp -> setCore r (wordOf w dp) dp)
    | _ when ops |> List.exists isQReg || (dts |> List.exists isNeonType) -> pErr neonMsg (String.concat ", " ops)
    | [ VREG _; VREG _ ] -> pErr "vmov between an s and a d register: use vcvt to change precision" (String.concat ", " ops)
    | _ ->
        pErr "vmov Sd, Sm | Dd, Dm | Sd/Dd, #imm | Sn, Rt | Rt, Sn | Sm, Sm1, Rt, Rt2 | Rt, Rt2, Sm, Sm1 | Dm, Rt, Rt2 | Rt, Rt2, Dm | Dd[x], Rt | Rt, Dn[x]"
            (String.concat ", " ops)

// ---------------------------------------------------------------- status registers

/// FPSCR bits that can be written: N Z C V QC AHP DN FZ RMode, and the cumulative exception flags
let private fpscrWritable = 0xFFC0009Fu

let private apsrOfFlags (fl : Flags) =
    (if fl.N then 0x80000000u else 0u) ||| (if fl.Z then 0x40000000u else 0u)
    ||| (if fl.C then 0x20000000u else 0u) ||| (if fl.V then 0x10000000u else 0u)

let private flagsOfWord (w : uint32) =
    { N = w &&& 0x80000000u <> 0u; Z = w &&& 0x40000000u <> 0u; C = w &&& 0x20000000u <> 0u; V = w &&& 0x10000000u <> 0u }

let private vmrs (ops : string list) =
    match ops |> List.map (fun o -> o.Trim().ToUpper()) with
    | [ "APSR_NZCV"; "FPSCR" ] -> Ok(fun (dp : DataPath) -> Ok { dp with Fl = flagsOfWord dp.Fp.FPSCR })
    | [ r; "FPSCR" ] -> coreReg r |> Result.map (fun r -> fun dp -> Ok(Helpers.setReg r dp.Fp.FPSCR dp))
    | [ _; ("FPSID" | "FPEXC" | "MVFR0" | "MVFR1") ] -> pErr "only FPSCR can be read in user mode" (List.last ops)
    | _ -> pErr "vmrs Rt, fpscr or vmrs APSR_nzcv, fpscr" (String.concat ", " ops)

let private vmsr (ops : string list) =
    match ops with
    | [ f; r ] when f.Trim().ToUpper() = "FPSCR" ->
        coreReg r
        |> Result.map (fun r ->
            fun (dp : DataPath) ->
                let v = dp.Regs.[r]
                if v &&& 0x00370000u <> 0u then
                    rErr dp.Regs.[R15] (sprintf "FPSCR Len and Stride must be 0 (0x%08X): short vector mode is not supported" v)
                else Ok { dp with Fp = { dp.Fp with FPSCR = v &&& fpscrWritable } })
    | [ f; _ ] when [ "FPSID"; "FPEXC" ] |> List.contains (f.Trim().ToUpper()) -> pErr "only FPSCR can be written in user mode" f
    | _ -> pErr "vmsr fpscr, Rt" (String.concat ", " ops)

let private mrs (ops : string list) =
    match ops with
    | [ r; psr ] when [ "APSR"; "CPSR" ] |> List.contains (psr.Trim().ToUpper()) ->
        coreReg r |> Result.map (fun r -> fun (dp : DataPath) -> Ok(Helpers.setReg r (apsrOfFlags dp.Fl) dp))
    | [ _; psr ] when psr.Trim().ToUpper().StartsWith "SPSR" -> pErr "SPSR does not exist in user mode" psr
    | _ -> pErr "mrs Rd, apsr" (String.concat ", " ops)

/// MSR APSR_nzcvq, Rm/#imm: in user mode only the flags can be written; other fields are ignored
let private msr symTab (ops : string list) =
    let field (f : string) =
        let f = f.Trim().ToUpper()
        match f.Split('_') with
        | [| "APSR"; bits |] when bits <> "" && bits |> Seq.forall (fun c -> "NZCVQG".Contains(string c)) -> Ok(bits.Contains "NZCVQ")
        | [| "CPSR"; bits |] when bits <> "" && bits |> Seq.forall (fun c -> "FSXC".Contains(string c)) -> Ok(bits.Contains "F")
        | _ -> pErr "a status register field: APSR_nzcvq, APSR_g or CPSR_f" f
    match ops with
    | [ f; src ] ->
        field f
        |> Result.bind (fun writesFlags ->
            match DP.parseOp2 DP.NoNegLit symTab [ src ] with
            | Ok(DP.NumberLiteral _ as op2) | Ok(DP.RegisterWithShift(_, DP.LSL, 0u) as op2) ->
                Ok(fun (dp : DataPath) ->
                    let v, _ = DP.evalOp2 op2 dp
                    Ok(if writesFlags then { dp with Fl = flagsOfWord v } else dp))
            | Ok _ -> pErr "msr takes a register or a #constant" src
            | Error e -> Error e)
    | _ -> pErr "msr APSR_nzcvq, Rm or msr APSR_nzcvq, #imm" (String.concat ", " ops)

// ---------------------------------------------------------------- loads and stores

let private alignedOr (addr : uint32) (dp : DataPath) (f : unit -> Result<DataPath, ExecuteError>) =
    if addr % 4u <> 0u then rErr addr (sprintf "VFP loads and stores need a word-aligned address: 0x%X" addr)
    else f ()

/// load the registers' words from consecutive addresses
let private loadRegs (regs : VReg list) (addr : uint32) (dp : DataPath) =
    alignedOr addr dp (fun () ->
        regs
        |> List.collect wordsOfReg
        |> List.indexed
        |> List.fold (fun acc (i, w) ->
            acc |> Result.bind (fun (d : DataPath) ->
                Memory.getDataMemWord (addr + 4u * uint32 i) dp |> Result.map (fun v -> setWord w v d))) (Ok dp))

let private storeRegs (regs : VReg list) (addr : uint32) (dp : DataPath) =
    alignedOr addr dp (fun () ->
        regs
        |> List.collect wordsOfReg
        |> List.indexed
        |> List.fold (fun acc (i, w) ->
            acc |> Result.bind (updateMemData (Dat(wordOf w dp)) (addr + 4u * uint32 i))) (Ok dp))

let private wordCount regs = regs |> List.sumBy (fun r -> (wordsOfReg r).Length)

/// a register list: {d8-d15}, {s0, s1, s2}: consecutive registers of one kind
let private regList (t : string) =
    let t = t.Trim()
    if not (t.StartsWith "{" && t.EndsWith "}") then pErr "a register list in braces: {d8-d15}" t
    else
        let items = t.Substring(1, t.Length - 2).Split(',') |> Array.map (fun s -> s.Trim()) |> List.ofArray
        let expand (item : string) =
            match item.Split('-') with
            | [| VREG a |] -> Ok [ a ]
            | [| VREG(SReg a); VREG(SReg b) |] when b >= a -> Ok [ for i in a .. b -> SReg i ]
            | [| VREG(DReg a); VREG(DReg b) |] when b >= a -> Ok [ for i in a .. b -> DReg i ]
            | _ -> pErr "s or d registers, or a range such as d8-d15" item
        items
        |> List.map expand
        |> List.fold (fun acc r -> map2 (@) acc r) (Ok [])
        |> Result.bind (fun regs ->
            let num = function | SReg n | DReg n -> n
            let sameKind = regs |> List.forall (fun r -> fmtOf r = fmtOf regs.Head)
            let consecutive = regs |> List.pairwise |> List.forall (fun (a, b) -> num b = num a + 1)
            match regs with
            | [] -> pErr "at least one register" t
            | _ when not sameKind -> pErr "a list of all s or all d registers" t
            | _ when not consecutive -> pErr "consecutive registers in increasing order" t
            | DReg _ :: _ when regs.Length > 16 -> pErr "at most 16 d registers" t
            | _ -> Ok regs)

/// VLDR/VSTR Rd, [Rn{, #±imm}] or a label
let private vldrstr (isLoad : bool) (ls : LineData) (dts : string list) (ops : string list) =
    let typeOk r = dts = [] || dts = [ (if fmtOf r = F32 then "32" else "64") ] || dts = [ (if fmtOf r = F32 then "F32" else "F64") ]
    let access r baseReg addrOf =
        Ok((fun (dp : DataPath) ->
                let a = addrOf dp
                if isLoad then loadRegs [ r ] a dp else storeRegs [ r ] a dp),
           Some { IsLoad = isLoad; Regs = [ r ]; Base = baseReg; Start = addrOf; After = None })
    match ops with
    | [ VREG r; _ ] when not (typeOk r) -> pErr "a type matching the register: .32/.f32 for s, .64/.f64 for d" (String.concat "." dts)
    | [ VREG r; addr ] when addr.Trim().StartsWith "[" ->
        let a = addr.Trim()
        if not (a.EndsWith "]") then pErr "[Rn] or [Rn, #offset] (no write-back)" addr
        else
            match a.Substring(1, a.Length - 2).Split(',') |> Array.map (fun s -> s.Trim()) with
            | [| rn |] -> parseRegister rn |> Result.bind (fun rn -> access r (Some rn) (fun dp -> dp.Regs.[rn]))
            | [| rn; off |] when off.StartsWith "#" ->
                parseRegister rn
                |> Result.bind (fun rn ->
                    parseNumberExpression ls.SymTab (off.Substring 1)
                    |> Result.bind (fun o ->
                        let o = int o
                        if o % 4 <> 0 || o < -1020 || o > 1020 then pErr "an offset that is a multiple of 4 from -1020 to 1020" off
                        else access r (Some rn) (fun dp -> uint32 (int64 dp.Regs.[rn] + int64 o))))
            | _ -> pErr "[Rn] or [Rn, #offset]: a VFP address has no register offset or write-back" addr
    | [ VREG r; label ] when isLoad ->
        let (WA here) = ls.LoadAddr
        parseNumberExpression ls.SymTab label
        |> Result.bind (fun target ->
            let off = int64 target - int64 (here + 8u)
            if off % 4L <> 0L || off < -1020L || off > 1020L then
                pErr "a label within 1020 bytes of this instruction; for data further away use ldr r0, =label then vldr with [r0]" label
            else access r None (fun _ -> target))
    | [ VREG _; label ] -> pErr "vstr Rd, [Rn{, #offset}]" label
    | _ -> pErr "vldr/vstr Sd or Dd, [Rn{, #offset}]" (String.concat ", " ops)

/// VLDM/VSTM{IA|DB} Rn{!}, {list}; VPUSH/VPOP {list}
let private vldmstm (isLoad : bool) (decrement : bool) (baseTxt : string) (listTxt : string) =
    let b = baseTxt.Trim()
    let wb = b.EndsWith "!"
    let rnTxt = if wb then b.TrimEnd('!').Trim() else b
    match parseRegister rnTxt, regList listTxt with
    | Error e, _ | _, Error e -> Error e
    | Ok R15, _ when wb -> pErr "write-back to PC is not allowed" baseTxt
    | Ok _, _ when decrement && not wb -> pErr "vldmdb and vstmdb need write-back: Rn!" baseTxt
    | Ok rn, Ok regs ->
        let bytes = 4u * uint32 (wordCount regs)
        let start (dp : DataPath) = if decrement then dp.Regs.[rn] - bytes else dp.Regs.[rn]
        let after (dp : DataPath) = if decrement then start dp else start dp + bytes
        Ok((fun (dp : DataPath) ->
                (if isLoad then loadRegs regs (start dp) dp else storeRegs regs (start dp) dp)
                |> Result.map (fun d -> if wb then Helpers.setReg rn (after dp) d else d)),
           Some { IsLoad = isLoad; Regs = regs; Base = Some rn; Start = start; After = (if wb then Some after else None) })

// ---------------------------------------------------------------- top level

let private roots =
    [ "VADD"; "VSUB"; "VMUL"; "VDIV"; "VNMUL"; "VMLA"; "VMLS"; "VNMLA"; "VNMLS"
      "VFMA"; "VFMS"; "VFNMA"; "VFNMS"; "VABS"; "VNEG"; "VSQRT"; "VCMP"; "VCMPE"
      "VCVT"; "VCVTR"; "VCVTB"; "VCVTT"; "VMOV"; "VMRS"; "VMSR"; "VLDR"; "VSTR"
      "VPUSH"; "VPOP"; "MRS"; "MSR"; "FMSTAT" ]

let private opCodes =
    Map.fold (fun m k v -> Map.add k v m)
        (opCodeExpand { InstrC = MISC; Roots = roots; Suffixes = [ "" ] })
        (opCodeExpand { InstrC = MISC; Roots = [ "VLDM"; "VSTM" ]; Suffixes = [ ""; "IA"; "DB" ] })

/// true if the opcode (without its .type) is one this module handles
let isVfpRoot (opc : string) = Map.containsKey ((opc.ToUpper().Split('.')).[0]) opCodes

let private parseOther (ls : LineData) (root : string) (suffix : string) (dts : string list) (ops : string list) : Result<Exec, ParseError> =
    let noType () = if dts <> [] then pErr (sprintf "%s takes no type" (root.ToLower())) (String.concat "." dts) else Ok()
    match root with
    | "VADD" -> arith3 root dts ops add
    | "VSUB" -> arith3 root dts ops sub
    | "VMUL" -> arith3 root dts ops mul
    | "VDIV" -> arith3 root dts ops div
    | "VNMUL" -> arith3 root dts ops (fun c f a b -> negRes f (mul c f a b))
    | "VMLA" -> mac root false false dts ops
    | "VMLS" -> mac root true false dts ops
    | "VNMLA" -> mac root true true dts ops
    | "VNMLS" -> mac root false true dts ops
    | "VFMA" -> fused root false false dts ops
    | "VFMS" -> fused root true false dts ops
    | "VFNMA" -> fused root true true dts ops
    | "VFNMS" -> fused root false true dts ops
    | "VABS" -> unary root dts ops (bitsOnly absBits)
    | "VNEG" -> unary root dts ops (bitsOnly negBits)
    | "VSQRT" -> unary root dts ops sqrt
    | "VCMP" | "VCMPE" -> vcmp root dts ops
    | "VCVT" | "VCVTR" | "VCVTB" | "VCVTT" -> vcvt root dts ops
    | "VMOV" -> vmov dts ops
    | "VMRS" -> noType () |> Result.bind (fun () -> vmrs ops)
    | "FMSTAT" -> noType () |> Result.bind (fun () -> if ops = [] || ops = [ "" ] then vmrs [ "APSR_nzcv"; "fpscr" ] else pErr "fmstat takes no operands" ls.Operands)
    | "VMSR" -> noType () |> Result.bind (fun () -> vmsr ops)
    | "MRS" -> mrs ops
    | "MSR" -> msr ls.SymTab ops
    | _ -> pErr "a VFP instruction" root

/// loads and stores also describe their memory access
let private parseInstr (ls : LineData) (root : string) (suffix : string) (dts : string list) (ops : string list) =
    match root with
    | "VLDR" -> vldrstr true ls dts ops
    | "VSTR" -> vldrstr false ls dts ops
    | "VLDM" | "VSTM" ->
        match ops with
        | [ b; l ] -> vldmstm (root = "VLDM") (suffix = "DB") b l
        | _ -> pErr (sprintf "%s%s Rn{!}, {register list}" (root.ToLower()) (suffix.ToLower())) ls.Operands
    | "VPUSH" | "VPOP" ->
        match ops with
        | [ l ] -> vldmstm (root = "VPOP") (root = "VPUSH") "sp!" l
        | _ -> pErr (sprintf "%s {register list}" (root.ToLower())) ls.Operands
    | _ -> parseOther ls root suffix dts ops |> Result.map (fun run -> run, None)

/// parse a VFP instruction, or MRS/MSR
let parse (ls : LineData) : Parse<Instr> option =
    let parts = ls.OpCode.ToUpper().Split('.')
    Map.tryFind parts.[0] opCodes
    |> Option.map (fun (_, (root, suffix, cond)) ->
        let dts = parts |> Array.skip 1 |> List.ofArray
        let ops = splitOperands ls.Operands |> List.filter (fun o -> o <> "")
        let res = parseInstr ls root suffix dts ops |> Result.map (fun (run, access) -> { Run = run; Access = access })
        copyParse ls res cond)

let (|IMatch|_|) = parse
