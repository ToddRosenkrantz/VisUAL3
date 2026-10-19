(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.Misc
    Description: Implement ARM miscellaneous instructions
*)

/// emulate DCD, DCB, FILL, EQU, ADR instructions
module Misc
    open EEExtensions
    open CommonData
    open CommonLex
    open Expressions
    open Helpers
    open Errors
    open DP


    type FILLInstr = { NumBytes : uint32; FillValue : uint32 }

    type ADRInstr = { AReg : RName; AVal : uint32 }

    type Instr =
        | DCD of uint32 list
        | DCB of uint32 list
        | FILL of FILLInstr
        | EQU of uint32
        | ADR of ADRInstr
        | BYTES of uint32 list // GNU data directives: bytes stored from the load address

    let miscRoots = [ "DCD"; "DCB"; "FILL"; "EQU"; "ADR" ]


    /// Errors which can occur during parsing
    // type ErrInstr =
    //     | InvalidExp of string
    //     | InvalidExpList of string
    //     | InvalidFill of string
    //     | LabelRequired
    /// Error types for parsing.

    let commaSplit (x : string) =
        x.Split([| ',' |])
        |> Array.toList
        |> List.map String.trim

    let mergeResults (lst : Result<'T, 'E> list) =
        let folder (st : Result<'T list, 'E>) (r : Result<'T, 'E>) =
            match st with
            | Error st -> Error st
            | Ok st -> Result.map (fun r -> r :: st) r
        List.fold folder (Ok []) lst
        |> Result.map List.rev


    let parseNumExpr ls s =
            Ok((), s)
            |> ResExpr(fun _ e -> e)
            |> ResCheckDone
            |> Result.bind (fun exp ->
                eval ls.SymTab exp)

    let parseExprList symTab lst =
        List.map (parseNumExpr symTab) lst
        |> mergeResults



    /// Largest block a single data directive may allocate
    let maxDataBytes = 0x100000u

    /// GNU as data and alignment directives (.word, .byte, .ascii, .space, .align, ...).
    /// The load address is the current data address; data is stored little-endian.
    let parseGnuData (ls : LineData) : Parse<Instr> =
        let (WA la) = ls.LoadAddr
        let op = ls.OpCode
        let lop = op.ToLower()
        let args = GnuLex.splitArgs ls.Operands
        let evalArg a = parseNumExpr ls a
        let directive dSize res =
            { copyDefault ls Cal with PInstr = Result.map BYTES res; ISize = 0u; DSize = dSize }
        let err dSize msg = directive dSize (makeInstructionError msg)
        let le size (v : uint32) = List.init size (fun i -> (v >>> (8 * i)) &&& 0xFFu)
        /// value fits in size bytes, signed or unsigned
        let fits size (v : uint32) =
            match size with
            | 1 -> v <= 0xFFu || v >= 0xFFFFFF80u
            | 2 -> v <= 0xFFFFu || v >= 0xFFFF8000u
            | _ -> true
        let values size =
            let dSize = Some(uint32 (args.Length * size))
            args
            |> List.map (fun a ->
                evalArg a |> Result.bind (fun v ->
                    if fits size v then Ok v
                    else makeInstructionError (sprintf "%s value '%s' (= %d) does not fit in %d byte(s)" lop a (int v) size)))
            |> mergeResults
            |> Result.map (List.collect (le size))
            |> directive dSize
        let byteArg (a : string option) =
            match a with
            | None -> Ok 0u
            | Some a ->
                evalArg a |> Result.bind (fun f ->
                    if fits 1 f then Ok(f &&& 0xFFu) else makeInstructionError (sprintf "Fill value '%s' must be a byte" a))
        match op with
        | ".WORD" | ".LONG" | ".INT" | ".4BYTE" -> values 4
        | ".HWORD" | ".SHORT" | ".2BYTE" -> values 2
        | ".BYTE" -> values 1
        | ".ASCII" | ".ASCIZ" | ".STRING" ->
            let terminator = if op = ".ASCII" then [] else [ 0u ]
            match args |> List.map GnuLex.parseStringLiteral |> mergeResults with
            | _ when args.IsEmpty -> err (Some 0u) (sprintf "%s needs a string operand" lop)
            | Ok strs ->
                let bytes = strs |> List.collect (fun b -> b @ terminator)
                directive (Some(uint32 bytes.Length)) (Ok bytes)
            | Error msg -> err (Some 0u) msg
        | ".SPACE" | ".SKIP" | ".ZERO" ->
            match args with
            | [ n ] | [ n; _ ] when op <> ".ZERO" || args.Length = 1 ->
                let fill = byteArg (List.tryItem 1 args)
                match evalArg n, fill with
                | Ok n, _ when n > maxDataBytes -> err (Some 0u) (sprintf "%s %d is too large (max %d bytes)" lop n maxDataBytes)
                | Ok n, Ok f -> directive (Some n) (Ok(List.replicate (int n) f))
                | Ok n, Error e -> directive (Some n) (Error e)
                | Error e, _ -> directive None (Error e)
            | _ -> err (Some 0u) (if op = ".ZERO" then ".zero needs one operand: .zero size" else sprintf "Usage: %s size[, fill]" lop)
        | ".FILL" ->
            let get i dflt = match List.tryItem i args with | Some a -> evalArg a | None -> Ok dflt
            match get 0 0u, get 1 1u, get 2 0u with
            | _ when args.IsEmpty || args.Length > 3 -> err (Some 0u) "Usage: .fill repeat[, size[, value]]"
            | Ok _, Ok sz, _ when sz <> 1u && sz <> 2u && sz <> 4u -> err (Some 0u) ".fill size must be 1, 2 or 4"
            | Ok r, Ok sz, _ when r * sz > maxDataBytes -> err (Some 0u) (sprintf ".fill is too large (max %d bytes)" maxDataBytes)
            | Ok r, Ok sz, Ok v -> directive (Some(r * sz)) (Ok(List.replicate (int r) (le (int sz) v) |> List.concat))
            | Ok r, Ok sz, Error e -> directive (Some(r * sz)) (Error e)
            | Error e, _, _ | _, Error e, _ -> directive None (Error e)
        | _ -> // .align / .p2align / .balign
            match GnuLex.alignBytes op ls.Operands, byteArg (List.tryItem 1 args) with
            | Ok a, Ok f ->
                let pad = (a - la % a) % a
                directive (Some pad) (Ok(List.replicate (int pad) f))
            | Ok _, Error e -> directive (Some 0u) (Error e)
            | Error msg, _ -> err (Some 0u) msg

    let parse (ls : LineData) : Parse<Instr> =
        let (WA la) = ls.LoadAddr // address this instruction is loaded into memory
        let opLst = commaSplit ls.Operands
        let resolvedOpLst = parseExprList ls opLst
        let (|PARSE|_|) op = parseNumExpr ls op |> Some
        let (|RESOLVEALL|_|) lst = match parseExprList ls lst with | Ok ops -> Some ops | _ -> None
        let opCode = ls.OpCode
        let offsetError wb b1 b2 ofs =
            let msg = sprintf "Valid %s offset in range %d..%d. Use 'LDR Rx, =SYMBOL' when offset is larger than this" wb b1 b2
            makeParseError msg (sprintf "Offset of %d" ofs) ""
        let checkAddrOffset (ofs : int) =
            match ofs - 8 with
            | x when x % 4 <> 0 && (x > 264 || x < -248) -> offsetError "byte" -248 264 (ofs - 8)
            | x when (x > 1032 || x < -1016) -> offsetError "word" -1016 1032 (ofs - 8)
            | x -> Ok()

        let labelBinder f =
            match ls.Label with
            | Some lab -> f lab
            | None -> ``Label required`` ("'" + ls.Operands + "' requires a label.")


        let makeDataInstr dataInstrCode = Result.map dataInstrCode resolvedOpLst

        let makeDataDirective dSizeOpt dataInstr =
            { copyDefault ls Cal with
                PInstr = dataInstr
                ISize = 0u
                DSize = dSizeOpt
            }



        let opNum = List.length opLst |> uint32

        let makeFILL ops =
            match resolvedOpLst with
            | Ok [ nBytes ] when nBytes % 4u = 0u ->
                makeDataDirective (Some nBytes) (FILL { NumBytes = nBytes; FillValue = 0u } |> Ok)
            | Ok [ nBytes; fillVal ] when nBytes % 4u = 0u ->
                makeDataDirective (Some nBytes) (FILL { NumBytes = nBytes; FillValue = fillVal } |> Ok)
            | Ok [ nBytes ]
            | Ok [ nBytes; _ ] ->
                let msg = sprintf "%d FILL bytes is invalid. Fill must have a number of bytes divisible by 4" nBytes
                makeDataDirective (Some 0u) (makeInstructionError msg)

            | _ -> makeDataDirective (Some 0u) <|
                       makeInstructionError ("Invalid operands '" + ls.Operands + "'. Fill must have 1 or 2 operands")

        let makeEQU (op : Resolvable) =
            match op with
            | Ok addr -> makeDataDirective (Some 0u) (EQU addr |> Ok)
            | Error e -> makeDataDirective (Some 0u) (Error e)
            |> fun pa -> { pa with PLabel = Option.map (fun lab -> lab, op) ls.Label }

        let pa = copyDefault ls Cal
        match opCode.ToUpper(), opLst with
        | "DCD", RESOLVEALL ops -> makeDataDirective (Some(opNum * 4u)) (makeDataInstr DCD)
        | "DCB", RESOLVEALL ops when ops.Length % 4 = 0 -> makeDataDirective (Some opNum) (makeDataInstr DCB)
        | "DCB", _ ->
            let msg = "Invalid operands: '" + ls.Operands + "'. DCB must have a number of parameters divisible by 4"
            makeInstructionError msg
            |> makeDataDirective (Some 0u)
        | "FILL", RESOLVEALL [ op ] -> makeFILL [ op, 0u ]
        | "FILL", _ -> makeDataDirective None <| makeInstructionError ("Invalid operands for FILL: unresolved symbols")
        | "ADR", RegMatch(Ok rn) :: [ PARSE(Ok addr) ] ->
            match checkAddrOffset (int addr - int la) with
            | Ok _ -> { pa with PInstr = ADR { AReg = rn; AVal = addr } |> Ok; PStall = (if rn = R15 then 2 else 0) }
            | Error e -> { pa with PInstr = Error e }
        | "ADR", _ops ->
            let msg = "Invalid operands: '" + ls.Operands +
                      "'. ADR must have a register followed by a numeric expression operand."
            { pa with PInstr = makeInstructionError msg  }
        | "EQU", [ PARSE op ] when opLst <> [] -> makeEQU op
        | "EQU", x -> { pa with PInstr = makeInstructionError (sprintf "'%A' is an invalid expression for EQU" x) }
        | _, ops -> makeInstructionError ("Invalid instruction: '" + ls.OpCode + " " + ls.Operands + "'")
                    |> makeDataDirective (Some 0u)
        | _ -> failwithf "What? unrecognised Misc opcode %s" opCode



    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) (ls : LineData) =
        if GnuLex.dataDirectives.Contains ls.OpCode || GnuLex.alignDirectives.Contains ls.OpCode
        then parseGnuData ls |> Some
        elif List.contains ls.OpCode miscRoots
        then
            //printfn "IMISC Parsing '%s'" ls.OpCode
            parse ls |> Some
        else
            //printfn "IMISC Not parsing '%s'" ls.OpCode
            None

