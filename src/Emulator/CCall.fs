(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.CCall
    Description: Call an assembly function as C would, from a C prototype
*)

/// Call an assembly function the way compiled C does (AAPCS), from a prototype
/// written in the source as a comment:
///     @ C: extern int sum_array(const int *a, int n);
/// Arguments go in r0-r3 then on the stack; pointer arguments are buffers the
/// emulator builds in a caller area of memory. After the run the return value,
/// buffers and calling-convention checks are decoded according to the prototype.
module CCall

open CommonData
open Errors
open Expressions
open ExecutionTop

// ///////////////////////////// C types ///////////////////////////////////////

/// Integer types the emulator can pass. Plain char is unsigned on ARM (AAPCS).
type CInt =
    | CChar of signed : bool * plain : bool // plain: written as just "char" (unsigned on ARM)
    | CShort of signed : bool
    | CInt32 of signed : bool
    | CBool

type CType =
    | CVoid
    | CIntT of CInt
    | CPtr of pointee : CType * isConst : bool

type CParam = { PName : string; PType : CType }

type Prototype = {
    FName : string
    Ret : CType
    Params : CParam list
    Line : int // source line of the "@ C:" comment
}

let rec typeName t =
    match t with
    | CVoid -> "void"
    | CIntT(CChar(true, _)) -> "signed char"
    | CIntT(CChar(false, true)) -> "char"
    | CIntT(CChar(false, false)) -> "unsigned char"
    | CIntT(CShort s) -> if s then "short" else "unsigned short"
    | CIntT(CInt32 s) -> if s then "int" else "unsigned int"
    | CIntT CBool -> "_Bool"
    | CPtr(p, c) -> (if c then "const " else "") + typeName p + " *"

/// size in bytes of a value of type t in memory
let sizeOf t =
    match t with
    | CIntT(CChar _) | CIntT CBool -> 1
    | CIntT(CShort _) -> 2
    | CIntT(CInt32 _) | CPtr _ -> 4
    | CVoid -> 1 // void * buffers are bytes

/// Range of values allowed for an integer type
let rangeOf t =
    match t with
    | CIntT(CChar(true, _)) -> -128L, 127L
    | CIntT(CChar(false, _)) | CVoid -> 0L, 255L
    | CIntT(CShort true) -> -32768L, 32767L
    | CIntT(CShort false) -> 0L, 65535L
    | CIntT(CInt32 true) -> -2147483648L, 2147483647L
    | CIntT(CInt32 false) | CPtr _ -> 0L, 4294967295L
    | CIntT CBool -> 0L, 1L

let isSigned t =
    match t with
    | CIntT(CChar(s, _)) | CIntT(CShort s) | CIntT(CInt32 s) -> s
    | _ -> false

/// Interpret the low (sizeOf t) bytes of a word as a value of type t
let decodeValue t (w : uint32) : int64 =
    match sizeOf t, isSigned t with
    | 1, true -> int64 (sbyte (w &&& 0xFFu))
    | 1, false -> int64 (w &&& 0xFFu)
    | 2, true -> int64 (int16 (w &&& 0xFFFFu))
    | 2, false -> int64 (w &&& 0xFFFFu)
    | _, true -> int64 (int32 w)
    | _ -> int64 w

/// The register value AAPCS requires for value v of type t (sign or zero extended)
let extendValue t (v : int64) = uint32 (decodeValue t (uint32 (v &&& 0xFFFFFFFFL)) &&& 0xFFFFFFFFL)

// ///////////////////////////// prototype parsing ///////////////////////////////

let private isIdent (s : string) =
    s.Length > 0 && (System.Char.IsLetter s.[0] || s.[0] = '_') && Seq.forall (fun c -> System.Char.IsLetterOrDigit c || c = '_') s

/// split C declaration text into tokens: identifiers, numbers and single punctuation
let private tokenize (txt : string) =
    let toks = ResizeArray<string>()
    let mutable i = 0
    while i < txt.Length do
        let c = txt.[i]
        if System.Char.IsWhiteSpace c then i <- i + 1
        elif System.Char.IsLetterOrDigit c || c = '_' then
            let j = i
            while i < txt.Length && (System.Char.IsLetterOrDigit txt.[i] || txt.[i] = '_') do i <- i + 1
            toks.Add(txt.Substring(j, i - j))
        elif c = '.' && i + 2 < txt.Length && txt.Substring(i, 3) = "..." then
            toks.Add "..."
            i <- i + 3
        else
            toks.Add(string c)
            i <- i + 1
    List.ofSeq toks

/// Parse type specifier words (e.g. ["unsigned"; "short"; "int"]) into a base type
let private baseType (words : string list) : Result<CType, string> =
    let ws = words |> List.filter (fun w -> w <> "const" && w <> "volatile" && w <> "extern" && w <> "static" && w <> "register")
    match ws with
    | [ "void" ] -> Ok CVoid
    | [ "char" ] -> Ok(CIntT(CChar(false, true)))
    | [ "unsigned"; "char" ] | [ "uint8_t" ] -> Ok(CIntT(CChar(false, false)))
    | [ "signed"; "char" ] | [ "int8_t" ] -> Ok(CIntT(CChar(true, false)))
    | [ "short" ] | [ "short"; "int" ] | [ "signed"; "short" ] | [ "signed"; "short"; "int" ] | [ "int16_t" ] -> Ok(CIntT(CShort true))
    | [ "unsigned"; "short" ] | [ "unsigned"; "short"; "int" ] | [ "uint16_t" ] -> Ok(CIntT(CShort false))
    | [ "int" ] | [ "signed" ] | [ "signed"; "int" ] | [ "long" ] | [ "long"; "int" ] | [ "signed"; "long" ]
    | [ "int32_t" ] | [ "intptr_t" ] | [ "ptrdiff_t" ] | [ "ssize_t" ] -> Ok(CIntT(CInt32 true))
    | [ "unsigned" ] | [ "unsigned"; "int" ] | [ "unsigned"; "long" ] | [ "unsigned"; "long"; "int" ]
    | [ "uint32_t" ] | [ "uintptr_t" ] | [ "size_t" ] -> Ok(CIntT(CInt32 false))
    | [ "_Bool" ] | [ "bool" ] -> Ok(CIntT CBool)
    | ws when List.contains "float" ws || List.contains "double" ws ->
        Error "float and double are not supported (the emulated core has no FPU)"
    | ws when (ws |> List.filter ((=) "long")).Length = 2 || List.contains "int64_t" ws || List.contains "uint64_t" ws ->
        Error "64-bit types (long long, int64_t) are not supported yet"
    | "struct" :: _ | "union" :: _ | "enum" :: _ -> Error "struct, union and enum types are not supported"
    | [] -> Error "missing type"
    | ws -> Error(sprintf "unknown type '%s'" (String.concat " " ws))

/// Parse one declaration "type-words [*...] [name] [[]]" into (name option, type)
let private parseDecl (toks : string list) : Result<string option * CType, string> =
    if List.contains "(" toks then Error "function pointer parameters are not supported"
    else
        // array suffix: int a[] or int a[10] is a pointer parameter
        let toks, isArray =
            match List.tryFindIndex ((=) "[") toks with
            | Some i -> List.take i toks, true
            | None -> toks, false
        // leading words up to the first '*' (or the name) are the base type
        let rec split (acc : string list) rest =
            match rest with
            | [] -> List.rev acc, []
            | "*" :: _ -> List.rev acc, rest
            | w :: rest' -> split (w :: acc) rest'
        let typeWords, ptrPart = split [] toks
        let nameFromWords, typeWords =
            // "int n": last word is a name if it isn't a type word
            let typeKeywords = set [ "void"; "char"; "short"; "int"; "long"; "signed"; "unsigned"; "const"; "volatile"
                                     "_Bool"; "bool"; "float"; "double"; "struct"; "union"; "enum"
                                     "int8_t"; "uint8_t"; "int16_t"; "uint16_t"; "int32_t"; "uint32_t"; "int64_t"; "uint64_t"
                                     "size_t"; "ssize_t"; "intptr_t"; "uintptr_t"; "ptrdiff_t" ]
            match ptrPart, List.rev typeWords with
            | [], last :: before when not (typeKeywords.Contains last) && isIdent last && before <> [] -> Some last, List.rev before
            | _ -> None, typeWords
        let baseConst = List.contains "const" typeWords
        // pointer part: "*", "const", "*", ..., [name]
        let rec ptrs t pendingConst rest =
            match rest with
            | "*" :: rest' -> ptrs (CPtr(t, pendingConst)) false rest'
            | "const" :: rest' | "volatile" :: rest' -> ptrs t pendingConst rest' // qualifies the pointer itself
            | [ name ] when isIdent name -> Ok(Some name, t)
            | [] -> Ok(None, t)
            | x :: _ -> Error(sprintf "unexpected '%s' in declaration" x)
        baseType typeWords
        |> Result.bind (fun bt ->
            match ptrPart with
            | [] -> Ok(nameFromWords, bt)
            | "*" :: rest -> ptrs (CPtr(bt, baseConst)) false rest
            | _ -> Error "bad declaration")
        |> Result.map (fun (name, t) -> name, (if isArray then CPtr(t, baseConst) else t))

/// Parse a prototype such as "extern int sum_array(const int *a, int n);"
let parsePrototype (line : int) (txt : string) : Result<Prototype, string> =
    let toks = tokenize (txt.Trim().TrimEnd(';'))
    match List.tryFindIndex ((=) "(") toks with
    | None -> Error "expected a function prototype, e.g. extern int f(int x);"
    | Some op ->
        let head = List.take op toks
        let inside, after =
            let rest = List.skip (op + 1) toks
            // the ')' matching the opening '(' of the parameter list
            let rec close i depth =
                match List.tryItem i rest with
                | None -> None
                | Some ")" when depth = 0 -> Some i
                | Some ")" -> close (i + 1) (depth - 1)
                | Some "(" -> close (i + 1) (depth + 1)
                | Some _ -> close (i + 1) depth
            match close 0 0 with
            | Some cp -> List.take cp rest, List.skip (cp + 1) rest
            | None -> rest, [ "<missing )>" ]
        if after <> [] then Error "unexpected text after the parameter list"
        else
            match parseDecl head with
            | Error e -> Error e
            | Ok(None, _) -> Error "missing function name"
            | Ok(Some fname, ret) ->
                let paramToks =
                    let rec splitCommas cur acc rest =
                        match rest with
                        | [] -> List.rev (List.rev cur :: acc)
                        | "," :: rest' -> splitCommas [] (List.rev cur :: acc) rest'
                        | t :: rest' -> splitCommas (t :: cur) acc rest'
                    match inside with
                    | [] | [ "void" ] -> []
                    | _ -> splitCommas [] [] inside
                let rec parseParams i ps acc =
                    match ps with
                    | [] -> Ok(List.rev acc)
                    | [ "..." ] :: _ -> Error "variadic functions (...) are not supported"
                    | p :: rest ->
                        match parseDecl p with
                        | Ok(_, CVoid) -> Error "a parameter cannot have type void"
                        | Ok(name, t) ->
                            let name = defaultArg name (sprintf "arg%d" (i + 1))
                            parseParams (i + 1) rest ({ PName = name; PType = t } :: acc)
                        | Error e -> Error(sprintf "parameter %d: %s" (i + 1) e)
                parseParams 0 paramToks []
                |> Result.map (fun ps -> { FName = fname; Ret = ret; Params = ps; Line = line })

/// Find "@ C: <prototype>" comments in the source
let findPrototypes (lines : string list) : Result<Prototype, int * string> list =
    lines
    |> List.indexed
    |> List.choose (fun (i, l) ->
        let t = l.TrimStart()
        if t.StartsWith "@" then
            let body = t.Substring(1).TrimStart()
            if body.StartsWith "C:" || body.StartsWith "c:" then
                Some(parsePrototype (i + 1) (body.Substring 2) |> Result.mapError (fun e -> i + 1, e))
            else None
        else None)

/// Names declared .global / .globl in the source
let globalSymbols (lines : string list) =
    let segs, _ = GnuLex.splitStatements lines
    segs
    |> List.collect (fun (_, s) ->
        let words = s.Split([| ' '; '\t'; ',' |], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
        match words with
        | d :: names when d.ToLower() = ".global" || d.ToLower() = ".globl" -> names
        | _ -> [])
    |> Set.ofList

// ///////////////////////////// arguments ///////////////////////////////////////

/// What the user supplies for one parameter
type ArgInput =
    | Value of string // integer: 42, -1, 0x10, 'A'
    | Elements of string list // pointer to initialised buffer: {10, 20, 30}
    | Str of string // char pointer to "text" (with terminating 0)
    | OutBuffer of int // pointer to n zeroed elements
    | Address of string // pointer given directly: NULL, a label, or an address

/// Parse the text typed for a parameter of type t:
/// numbers for values; for pointers {a, b}, "text", [n] (zeroed), NULL, label or address
let parseArgText (t : CType) (txt : string) : Result<ArgInput, string> =
    let s = txt.Trim()
    match t with
    | CPtr _ when s.StartsWith "{" ->
        if s.EndsWith "}" then Ok(Elements(GnuLex.splitArgs (s.Substring(1, s.Length - 2))))
        else Error "missing '}'"
    | CPtr _ when s.StartsWith "\"" -> Ok(Str s)
    | CPtr _ when s.StartsWith "[" ->
        match System.Int32.TryParse(s.TrimStart('[').TrimEnd(']').Trim()) with
        | true, n when s.EndsWith "]" && n >= 0 -> Ok(OutBuffer n)
        | _ -> Error "use [n] for a buffer of n zeroed elements"
    | CPtr _ -> if s = "" then Error "missing pointer argument" else Ok(Address s)
    | _ -> if s = "" then Error "missing value" else Ok(Value s)

/// A buffer the emulator built in the caller area for a pointer argument
type Buffer = {
    BParam : string
    BAddr : uint32
    BElem : CType
    BCount : int
    BConst : bool
    BInitial : int64 list
}

/// Base of the memory area where argument buffers are placed
let callerArea = 0x10000000u

/// Distinct values in r4-r11 at the call, so callee-saved violations are visible
let calleeSavedInit = [ R4; R5; R6; R7; R8; R9; R10; R11 ] |> List.mapi (fun i r -> r, 0xC0DE0004u + uint32 i)

type CallSetup = {
    Proto : Prototype
    Run : RunInfo
    Buffers : Buffer list
    ArgWords : uint32 list // argument words in order: r0-r3 then stack
    SPAtCall : uint32
    FuncAddr : uint32
    Warnings : string list
}

let private evalValue (syms : Map<string, uint32>) (t : CType) (txt : string) : Result<int64, string> =
    match txt.Trim() with
    | Expr(ast, "") ->
        match evalI64 syms ast with
        | Ok v ->
            let lo, hi = rangeOf t
            if v >= lo && v <= hi then Ok v
            elif not (isSigned t) && v < 0L && v >= -(hi + 1L) / 2L - 1L then
                Error(sprintf "%d is negative but %s is unsigned" v (typeName t))
            else Error(sprintf "%s does not fit in %s (%d..%d)" txt (typeName t) lo hi)
        | Error(``Undefined symbol`` [ (s, _) ]) -> Error(sprintf "'%s' is not a number or a label in the program" s)
        | Error e -> Error(sprintf "%A" e)
    | _ -> Error(sprintf "'%s' is not a valid number" txt)

/// Build the machine state for calling proto with args, using the program in lim
let prepareCall (lim : LoadImage) (sourceLines : string list) (proto : Prototype) (args : ArgInput list) : Result<CallSetup, string list> =
    let syms = lim.SymInf.SymTab
    let fail msgs = Error msgs
    if lim.Errors <> [] then fail [ "the program has assembly errors" ]
    elif args.Length <> proto.Params.Length then
        fail [ sprintf "%s takes %d argument(s) but %d were given" proto.FName proto.Params.Length args.Length ]
    else
        match Map.tryFind proto.FName syms, Map.tryFind proto.FName lim.SymInf.SymTypeTab with
        | None, _ -> fail [ sprintf "undefined reference to `%s': no label %s: in the assembly code" proto.FName proto.FName ]
        | Some _, Some typ when typ <> CodeSymbol -> fail [ sprintf "`%s' is a data label, not a function" proto.FName ]
        | Some faddr, _ ->
            let warnings =
                [ if not (globalSymbols sourceLines |> Set.contains proto.FName) then
                      yield sprintf "%s is not declared .global: a C program could not link to it" proto.FName ]
            if lim.Mem |> Map.exists (fun (WA a) _ -> a >= callerArea && a < callerArea + 0x1000000u) then
                fail [ sprintf "program data overlaps the argument area at 0x%X" callerArea ]
            else
            // allocate buffers and compute argument words
            let mutable next = callerArea
            let mutable mem = lim.Mem
            let mutable buffers = []
            let alloc (p : CParam) elem isConst (values : int64 list) =
                let size = sizeOf elem
                let addr = next
                let bytes = values |> List.collect (fun v -> List.init size (fun i -> uint32 ((v >>> (8 * i)) &&& 0xFFL)))
                mem <- addBytesToMem addr mem bytes
                next <- (next + uint32 (max 1 bytes.Length) + 7u) &&& 0xFFFFFFF8u
                buffers <- buffers @ [ { BParam = p.PName; BAddr = addr; BElem = elem; BCount = values.Length; BConst = isConst; BInitial = values } ]
                addr
            let argWord (p : CParam) (a : ArgInput) : Result<uint32, string> =
                let where msg = sprintf "%s: %s" p.PName msg
                match p.PType, a with
                | CPtr _, Value _ | (CVoid | CIntT _), (Elements _ | Str _ | OutBuffer _ | Address _) ->
                    Error(where "wrong kind of argument for this type")
                | (CVoid | CIntT _) as t, Value v -> evalValue syms t v |> Result.map (extendValue t) |> Result.mapError where
                | CPtr(elem, isConst), Elements es ->
                    let rec evalAll acc es =
                        match es with
                        | [] -> Ok(List.rev acc)
                        | e :: rest -> evalValue syms elem e |> Result.bind (fun v -> evalAll (v :: acc) rest)
                    evalAll [] es |> Result.map (alloc p elem isConst) |> Result.mapError where
                | CPtr((CIntT(CChar _) | CVoid) as elem, isConst), Str s ->
                    GnuLex.parseStringLiteral s
                    |> Result.map (fun bytes -> alloc p elem isConst ((bytes |> List.map int64) @ [ 0L ]))
                    |> Result.mapError where
                | CPtr _, Str _ -> Error(where "a string can only be passed to a char * parameter")
                | CPtr(elem, isConst), OutBuffer n -> Ok(alloc p elem isConst (List.replicate n 0L))
                | CPtr _, Address s when s.Trim() = "NULL" -> Ok 0u
                | CPtr(elem, _), Address s -> evalValue syms (CIntT(CInt32 false)) s |> Result.map uint32 |> Result.mapError where
            let results = List.map2 argWord proto.Params args
            match results |> List.choose (function | Error e -> Some e | _ -> None) with
            | errs when errs <> [] -> fail errs
            | _ ->
                let words = results |> List.map (function | Ok w -> w | Error _ -> 0u)
                let regArgs = List.truncate 4 words
                let stackArgs = if words.Length > 4 then List.skip 4 words else []
                // AAPCS: stacked arguments start at SP, which is 8-byte aligned at the call
                let sp = (initStackPointer - 4u * uint32 stackArgs.Length) &&& 0xFFFFFFF8u
                mem <- addBytesToMem sp mem (stackArgs |> List.collect (fun w -> List.init 4 (fun i -> (w >>> (8 * i)) &&& 0xFFu)))
                let regs =
                    initialRegMap
                    |> Map.add R13 sp
                    |> Map.add R14 returnToCaller
                    |> Map.add R15 faddr
                    |> fun m -> List.fold (fun m (r, v) -> Map.add r v m) m calleeSavedInit
                    |> fun m -> List.fold (fun m (i, w) -> Map.add (register i) w m) m (List.indexed regArgs)
                let flags = { N = false; Z = false; C = false; V = false }
                let ri = getRunInfoFromImageWithInits NoBreak lim regs flags Map.empty mem
                Ok { Proto = proto; Run = ri; Buffers = buffers; ArgWords = words
                     SPAtCall = sp; FuncAddr = faddr; Warnings = warnings }

// ///////////////////////////// results ///////////////////////////////////////

type CallOutcome =
    | Returned
    | RanOffEnd // reached the end of the code without returning
    | StepLimit of int64 // still running: possibly an infinite loop
    | RunError of string

type BufferResult = {
    Buffer : Buffer
    After : int64 list
    Changed : int list // indices of changed elements
}

type CallResult = {
    Outcome : CallOutcome
    R0 : uint32
    ReturnValue : int64 option // decoded by return type (None for void)
    ReturnText : string
    BufferResults : BufferResult list
    Problems : string list // calling convention violations and warnings
}

let readByte (mm : DataMemory) (a : uint32) =
    match Map.tryFind (WA(a &&& 0xFFFFFFFCu)) mm with
    | Some(Dat w) -> (w >>> (8 * int (a &&& 3u))) &&& 0xFFu
    | _ -> 0u

let readValue (mm : DataMemory) (t : CType) (a : uint32) =
    let n = sizeOf t
    let w = List.init n (fun i -> readByte mm (a + uint32 i) <<< (8 * i)) |> List.fold (|||) 0u
    decodeValue t w

/// Describe an address: inside an argument buffer, or relative to a program label
let describeAddress (setup : CallSetup) (a : uint32) =
    let inBuffer =
        setup.Buffers |> List.tryFind (fun b -> a >= b.BAddr && a < b.BAddr + uint32 (max 1 (b.BCount * sizeOf b.BElem)))
    match inBuffer with
    | _ when a = 0u -> "NULL"
    | Some b ->
        let off = int (a - b.BAddr)
        let size = sizeOf b.BElem
        if off % size = 0 then sprintf "&%s[%d]" b.BParam (off / size) else sprintf "%s + %d bytes" b.BParam off
    | None ->
        let st = setup.Run.st |> Map.toList |> List.filter (fun (_, (v, _)) -> v <= a) |> List.sortBy (fun (_, (v, _)) -> a - v)
        match st with
        | (name, (v, _)) :: _ when a - v < 0x1000u -> if a = v then name else sprintf "%s + %d" name (a - v)
        | _ -> sprintf "0x%X" a

let private formatValue t (v : int64) =
    match t with
    | CIntT(CChar _) when v >= 32L && v < 127L -> sprintf "%d '%c'" v (char (int v))
    | CIntT CBool -> if v = 0L then "false" else "true"
    | _ when v < 0L -> sprintf "%d (0x%X)" v (uint32 (v &&& 0xFFFFFFFFL))
    | _ -> sprintf "%d (0x%X)" v v

/// Decode the result of a call after it has been run
let decodeResult (setup : CallSetup) (ri : RunInfo) : CallResult =
    let dp = fst ri.dpCurrent
    let r0 = dp.Regs.[R0]
    let outcome =
        match ri.State with
        | PSError TBEXIT -> Returned
        | PSExit -> RanOffEnd
        | PSRunning | PSBreak -> StepLimit ri.StepsDone
        | PSError(``Run time error``(a, msg)) -> RunError(sprintf "at 0x%X: %s" a msg)
        | PSError(NotInstrMem a) -> RunError(sprintf "jumped to 0x%X, which is not code" a)
        | PSError e -> RunError(sprintf "%A" e)
    let ret = setup.Proto.Ret
    let retVal = match ret with | CVoid -> None | t -> Some(decodeValue t r0)
    let returnText =
        match ret, retVal with
        | CVoid, _ -> "void"
        | CPtr(elem, _), Some v ->
            let a = uint32 v
            let target = describeAddress setup a
            if a = 0u || elem = CVoid then target
            else sprintf "%s -> %s" target (formatValue elem (readValue dp.MM elem a))
        | t, Some v -> formatValue t v
        | _, None -> ""
    let bufferResults =
        setup.Buffers |> List.map (fun b ->
            let size = sizeOf b.BElem
            let after = List.init b.BCount (fun i -> readValue dp.MM b.BElem (b.BAddr + uint32 (i * size)))
            let changed = List.zip b.BInitial after |> List.indexed |> List.filter (fun (_, (x, y)) -> x <> y) |> List.map fst
            { Buffer = b; After = after; Changed = changed })
    let problems =
        [ yield! setup.Warnings
          if outcome = Returned then
              for (r, v) in calleeSavedInit do
                  if dp.Regs.[r] <> v then
                      yield sprintf "%A was not preserved (0x%X at call, 0x%X at return): r4-r11 are callee-saved" r v dp.Regs.[r]
              if dp.Regs.[R13] <> setup.SPAtCall then
                  yield sprintf "SP not restored (0x%X at call, 0x%X at return)" setup.SPAtCall dp.Regs.[R13]
              match ret with
              | CIntT t when sizeOf ret < 4 && r0 <> extendValue ret (decodeValue ret r0) ->
                  yield sprintf "r0 = 0x%X is not a properly %s-extended %s: AAPCS requires the callee to extend narrow results"
                                r0 (if isSigned ret then "sign" else "zero") (typeName ret)
              | CIntT CBool when r0 > 1u -> yield sprintf "r0 = 0x%X is not a valid _Bool (0 or 1)" r0
              | _ -> ()
          for br in bufferResults do
              if br.Buffer.BConst && br.Changed <> [] then
                  yield sprintf "%s points to const data but the function changed it" br.Buffer.BParam ]
    { Outcome = outcome; R0 = r0; ReturnValue = retVal; ReturnText = returnText
      BufferResults = bufferResults; Problems = problems }

/// Prepare, run (up to maxSteps instructions) and decode a call
let callFunction (lim : LoadImage) (sourceLines : string list) (proto : Prototype) (args : ArgInput list) (maxSteps : int64) =
    prepareCall lim sourceLines proto args
    |> Result.map (fun setup ->
        let ri = asmStep maxSteps setup.Run
        setup, decodeResult setup ri)

/// Text lines summarising a call, e.g. for a call history panel
let formatCall (setup : CallSetup) (res : CallResult) =
    let p = setup.Proto
    let argText =
        List.zip p.Params setup.ArgWords
        |> List.map (fun (prm, w) ->
            match prm.PType with
            | CPtr _ -> sprintf "%s=%s" prm.PName (describeAddress setup w)
            | t -> sprintf "%s=%d" prm.PName (decodeValue t w))
        |> String.concat ", "
    [ yield sprintf "%s(%s)" p.FName argText
      match res.Outcome with
      | Returned -> yield sprintf "  returned %s" res.ReturnText
      | RanOffEnd -> yield "  did not return: execution reached the end of the code"
      | StepLimit n -> yield sprintf "  did not return after %d instructions (infinite loop?)" n
      | RunError e -> yield sprintf "  run time error %s" e
      for br in res.BufferResults do
          let show i v = (if List.contains i br.Changed then "*" else "") + string v
          yield sprintf "  %s[%d]: {%s}" br.Buffer.BParam br.Buffer.BCount (br.After |> List.mapi show |> String.concat ", ")
      for prob in res.Problems do
          yield sprintf "  warning: %s" prob ]
