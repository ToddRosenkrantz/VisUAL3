(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.GnuLex
    Description: GNU as (ARM, unified syntax) source front end
*)

/// Turn GNU as source text into a list of statements for the loader.
/// Handles comments, ';' statement separators, 'label:' definitions,
/// numeric local labels (1: ... b 1b), sections and directives.
/// Code statements are emitted first, then data statements grouped by section.
module GnuLex

open System.Text
open Expressions

type Section =
    | Text
    | Data of string // section name, e.g. ".data", ".rodata", ".bss"

/// One assembler statement. Op is upper case: a mnemonic, a data/alignment directive,
/// "EQU" (from .equ/.set/sym = expr), or "" for a label with no statement.
type Stmt = {
    LineNum : int
    Label : string option
    Op : string
    Operands : string
    LexError : string option
}

let makeStmt lineNum label op operands = { LineNum = lineNum; Label = label; Op = op; Operands = operands; LexError = None }

let errorStmt lineNum msg = { makeStmt lineNum None "" "" with LexError = Some msg }

/// directives that put bytes into data memory
let dataDirectives =
    set [ ".WORD"; ".LONG"; ".INT"; ".4BYTE"; ".HWORD"; ".SHORT"; ".2BYTE"; ".BYTE"
          ".ASCII"; ".ASCIZ"; ".STRING"; ".SPACE"; ".SKIP"; ".ZERO"; ".FILL" ]

let stringDirectives = set [ ".ASCII"; ".ASCIZ"; ".STRING" ]

let alignDirectives = set [ ".ALIGN"; ".P2ALIGN"; ".BALIGN" ]

/// directives accepted and ignored: they only matter to the linker, debugger or tools
let ignoredDirectives =
    set [ ".GLOBAL"; ".GLOBL"; ".TYPE"; ".SIZE"; ".ARM"; ".CPU"; ".ARCH"; ".ARCH_EXTENSION"; ".FPU"
          ".EABI_ATTRIBUTE"; ".FILE"; ".IDENT"; ".LTORG"; ".POOL"; ".FUNC"; ".ENDFUNC"; ".WEAK"
          ".HIDDEN"; ".LOCAL"; ".FNSTART"; ".FNEND"; ".CANTUNWIND"; ".SAVE"; ".SETFP"; ".PAD"
          ".PERSONALITY"; ".PERSONALITYINDEX"; ".HANDLERDATA"; ".MOVSP"; ".VSAVE"; ".UNWIND_RAW" ]

/// armasm / VisUAL mnemonics that look like instructions but are not GNU as
let armasmOnly =
    Map.ofList [
        "DCD", ".word"; "DCW", ".hword"; "DCB", ".byte or .ascii"; "FILL", ".space"; "SPACE", ".space"
        "EQU", ".equ"; "END", "nothing (or .end)"; "ALIGN", ".align"; "AREA", ".text or .data"
        "ENTRY", "nothing"; "EXPORT", ".global"; "IMPORT", "nothing"; "PRESERVE8", "nothing" ]

let isSymbol (s : string) = s.Length > 0 && isSymbolStart s.[0] && Seq.forall isSymbolChar s

let isNumericLabel (s : string) = s.Length > 0 && Seq.forall System.Char.IsDigit s

/// Split operands at top-level commas, ignoring commas inside "strings" and 'c' character literals
let splitArgs (txt : string) : string list =
    let parts = ResizeArray<string>()
    let cur = StringBuilder()
    let mutable i = 0
    let n = txt.Length
    while i < n do
        match txt.[i] with
        | '"' ->
            cur.Append '"' |> ignore
            i <- i + 1
            while i < n && txt.[i] <> '"' do
                if txt.[i] = '\\' && i + 1 < n then
                    cur.Append(txt.[i]).Append(txt.[i + 1]) |> ignore
                    i <- i + 2
                else
                    cur.Append txt.[i] |> ignore
                    i <- i + 1
            if i < n then
                cur.Append '"' |> ignore
                i <- i + 1
        | '\'' ->
            let len = if i + 1 < n && txt.[i + 1] = '\\' then 3 else 2
            cur.Append(txt.Substring(i, min len (n - i))) |> ignore
            i <- i + len
        | ',' ->
            parts.Add(cur.ToString().Trim())
            cur.Clear() |> ignore
            i <- i + 1
        | c ->
            cur.Append c |> ignore
            i <- i + 1
    let last = cur.ToString().Trim()
    if parts.Count = 0 && last = "" then [] else List.ofSeq parts @ [ last ]

/// Parse a "string literal" (with GNU escapes) into byte values
let parseStringLiteral (txt : string) : Result<uint32 list, string> =
    let t = txt.Trim()
    if t.Length < 2 || t.[0] <> '"' || t.[t.Length - 1] <> '"' then
        Error(sprintf "Expected a string in double quotes, found '%s'" txt)
    else
        let body = t.Substring(1, t.Length - 2)
        let rec loop i acc =
            if i >= body.Length then Ok(List.rev acc)
            elif body.[i] = '\\' then
                match parseEscape body (i + 1) with
                | Some(v, j) -> loop j (v :: acc)
                | None -> Error(sprintf "Invalid escape sequence in %s" t)
            else loop (i + 1) ((uint32 body.[i] &&& 0xFFu) :: acc)
        loop 0 []

/// Split source into (line number, statement text) segments:
/// removes comments (@, //, C-style block comments, '#' at start of line) and splits at ';'.
/// Returns segments and errors.
let splitStatements (lines : string list) : (int * string) list * Stmt list =
    let segs = ResizeArray<int * string>()
    let errs = ResizeArray<Stmt>()
    let mutable inBlock = false
    let mutable blockStart = 0
    lines |> List.iteri (fun idx line ->
        let lineNum = idx + 1
        let n = line.Length
        let cur = StringBuilder()
        let flush () =
            let s = cur.ToString()
            if s.Trim() <> "" then segs.Add((lineNum, s.Trim()))
            cur.Clear() |> ignore
        let mutable i = 0
        // '#' as the first non-blank character makes the whole line a comment
        if not inBlock && line.TrimStart().StartsWith "#" then i <- n
        while i < n do
            if inBlock then
                if i + 1 < n && line.[i] = '*' && line.[i + 1] = '/' then
                    inBlock <- false
                    i <- i + 2
                else i <- i + 1
            else
                match line.[i] with
                | '@' -> i <- n
                | '/' when i + 1 < n && line.[i + 1] = '/' -> i <- n
                | '/' when i + 1 < n && line.[i + 1] = '*' ->
                    inBlock <- true
                    blockStart <- lineNum
                    cur.Append ' ' |> ignore
                    i <- i + 2
                | ';' ->
                    flush ()
                    i <- i + 1
                | '"' ->
                    let start = i
                    i <- i + 1
                    while i < n && line.[i] <> '"' do
                        i <- i + (if line.[i] = '\\' then 2 else 1)
                    if i >= n then
                        errs.Add(errorStmt lineNum "Unterminated string: missing closing '\"'")
                        i <- n
                    else
                        i <- i + 1
                        cur.Append(line.Substring(start, i - start)) |> ignore
                | '\'' ->
                    // character literal: 'c, 'c' or '\n'
                    let len = if i + 1 < n && line.[i + 1] = '\\' then 3 else 2
                    let len = min len (n - i)
                    let len = if i + len < n && line.[i + len] = '\'' then len + 1 else len
                    cur.Append(line.Substring(i, len)) |> ignore
                    i <- i + len
                | c ->
                    cur.Append c |> ignore
                    i <- i + 1
        flush ())
    if inBlock then errs.Add(errorStmt blockStart "Unterminated /* comment")
    List.ofSeq segs, List.ofSeq errs

/// A source statement before label attachment and section ordering
type RawItem =
    | RLabel of int * string
    | RStmt of int * string * string // line, op (upper case), operands
    | RErr of Stmt

/// Split one statement segment into labels and an optional op + operands
let parseSegment (lineNum : int, text : string) : RawItem list =
    let rec labels (t : string) acc =
        let t = t.TrimStart()
        let mutable j = 0
        while j < t.Length && isSymbolChar t.[j] do j <- j + 1
        let name = t.Substring(0, j)
        let k =
            let mutable k = j
            while k < t.Length && (t.[k] = ' ' || t.[k] = '\t') do k <- k + 1
            k
        if j > 0 && k < t.Length && t.[k] = ':' && (isSymbol name || isNumericLabel name) then
            labels (t.Substring(k + 1)) (RLabel(lineNum, name) :: acc)
        elif j > 0 && k < t.Length && t.[k] = ':' then
            List.rev (RErr(errorStmt lineNum (sprintf "Invalid label name '%s'" name)) :: acc), ""
        else List.rev acc, t.Trim()
    let labs, rest = labels text []
    if rest = "" then labs
    else
        let opEnd =
            let mutable j = 0
            while j < rest.Length && not (System.Char.IsWhiteSpace rest.[j]) && rest.[j] <> '=' do j <- j + 1
            j
        let op = rest.Substring(0, opEnd)
        let operands = rest.Substring(opEnd).Trim()
        if isSymbol op && operands.StartsWith "=" && not (operands.StartsWith "==") then
            // sym = expr
            labs @ [ RStmt(lineNum, ".SET", op + ", " + operands.Substring(1).Trim()) ]
        else
            labs @ [ RStmt(lineNum, op.ToUpper(), operands) ]

/// Rename numeric local labels (N:) to unique symbols and resolve Nb / Nf references.
let resolveLocalLabels (items : RawItem list) : RawItem list =
    let totals =
        items
        |> List.choose (function | RLabel(_, n) when isNumericLabel n -> Some n | _ -> None)
        |> List.countBy id |> Map.ofList
    let localName n k = sprintf ".L%s$%d" n k
    let mutable seen : Map<string, int> = Map.empty
    let count n = Map.tryFind n seen |> Option.defaultValue 0
    /// replace Nb / Nf references in operand text; returns text or an error
    let replaceRefs (txt : string) : Result<string, string> =
        let sb = StringBuilder()
        let n = txt.Length
        let mutable i = 0
        let mutable err = None
        while i < n do
            let c = txt.[i]
            if c = '\'' then
                // skip character literals
                let len = min (if i + 1 < n && txt.[i + 1] = '\\' then 3 else 2) (n - i)
                sb.Append(txt.Substring(i, len)) |> ignore
                i <- i + len
            elif System.Char.IsDigit c && (i = 0 || not (isSymbolChar txt.[i - 1])) then
                let mutable j = i
                while j < n && System.Char.IsDigit txt.[j] do j <- j + 1
                let num = txt.Substring(i, j - i)
                let isRef = j < n && (txt.[j] = 'b' || txt.[j] = 'f') && (j + 1 = n || not (isSymbolChar txt.[j + 1]))
                if isRef then
                    let k = count num
                    let target =
                        if txt.[j] = 'b' then
                            if k = 0 then Error(sprintf "No earlier local label '%s:' for '%sb'" num num) else Ok(localName num k)
                        else
                            if k + 1 > (Map.tryFind num totals |> Option.defaultValue 0)
                            then Error(sprintf "No later local label '%s:' for '%sf'" num num)
                            else Ok(localName num (k + 1))
                    match target with
                    | Ok name -> sb.Append name |> ignore
                    | Error e -> err <- Some e
                    i <- j + 1
                else
                    sb.Append num |> ignore
                    i <- j
            else
                sb.Append c |> ignore
                i <- i + 1
        match err with
        | Some e -> Error e
        | None -> Ok(sb.ToString())
    items
    |> List.map (function
        | RLabel(line, n) when isNumericLabel n ->
            seen <- Map.add n (count n + 1) seen
            RLabel(line, localName n (count n))
        | RStmt(line, op, operands) when not (stringDirectives.Contains op) ->
            match replaceRefs operands with
            | Ok ops -> RStmt(line, op, ops)
            | Error e -> RErr(errorStmt line e)
        | item -> item)

/// Number of bytes for an alignment directive (.align/.p2align n = 2^n, .balign n = n)
let alignBytes op (operands : string) : Result<uint32, string> =
    match splitArgs operands with
    | [] -> Error(sprintf "%s needs an alignment operand" ((op : string).ToLower()))
    | a :: _ ->
        match parseEvalNumericExpression Map.empty a with
        | Ok n when op = ".BALIGN" ->
            if n > 0u && n &&& (n - 1u) = 0u && n <= 0x10000u then Ok n
            else Error(sprintf ".balign %d: alignment must be a power of 2" n)
        | Ok n when n <= 16u -> Ok(1u <<< int n)
        | Ok n -> Error(sprintf "%s %d: alignment too large" (op.ToLower()) n)
        | Error _ -> Error(sprintf "%s: alignment must be a constant" (op.ToLower()))

/// Turn GNU as source lines into loader statements.
/// Code statements (and .equ, errors) come first, in source order;
/// then data statements grouped by section in order of first appearance
/// (data written in .text comes first). Labels are attached to the next
/// statement that occupies memory, so a label before .word gets a data address.
let lexProgram (lines : string list) : Stmt list =
    let segs, splitErrs = splitStatements lines
    let items = segs |> List.collect parseSegment |> resolveLocalLabels
    let code = ResizeArray<Stmt>(splitErrs)
    let dataSections = ResizeArray<string * ResizeArray<Stmt>>()
    let dataStream name =
        match Seq.tryFind (fun (n, _) -> n = name) dataSections with
        | Some(_, s) -> s
        | None ->
            // like a linker, start each section word aligned
            let s = ResizeArray<Stmt>([ makeStmt 0 None ".BALIGN" "4" ])
            dataSections.Add((name, s))
            s
    dataStream ".text" |> ignore // data written in .text goes first
    let mutable section = Text
    let mutable pending : (int * string) list = [] // labels waiting for the next statement (reverse order)
    let mutable pendingAlign : (int * string * string) option = None
    let mutable codeWords = 0u
    let mutable stop = false
    let streamOf sec = match sec with | Text -> dataStream ".text" | Data name -> dataStream name
    /// labels with no following statement in this section
    let flushLabels () =
        for (line, lab) in List.rev pending do
            match section with
            | Text -> code.Add(makeStmt line (Some lab) "" "")
            | Data name -> (dataStream name).Add(makeStmt line (Some lab) ".SPACE" "0")
        pending <- []
        pendingAlign <- None
    /// attach pending labels: all but the last get zero-size statements of the same kind
    let attach (stream : ResizeArray<Stmt>) (st : Stmt) isData =
        match List.rev pending with
        | [] -> stream.Add st
        | labs ->
            let extra, (_, last) = List.take (labs.Length - 1) labs, List.last labs
            for (line, lab) in extra do
                stream.Add(if isData then makeStmt line (Some lab) ".SPACE" "0" else makeStmt line (Some lab) "" "")
            stream.Add { st with Label = Some last }
        pending <- []
    let addCode (st : Stmt) =
        match pendingAlign with
        | Some(line, op, operands) ->
            pendingAlign <- None
            match alignBytes op operands with
            | Ok a when a > 4u ->
                let pad = ((a - (codeWords * 4u) % a) % a) / 4u
                for _ in 1u .. pad do
                    code.Add(makeStmt line None "MOV" "r0, r0") // ARM nop
                    codeWords <- codeWords + 1u
            | Ok _ -> ()
            | Error e -> code.Add(errorStmt line e)
        | None -> ()
        attach code st false
        codeWords <- codeWords + 1u
    let addData (st : Stmt) =
        let stream = streamOf section
        match pendingAlign with
        | Some(line, op, operands) ->
            pendingAlign <- None
            stream.Add(makeStmt line None op operands)
        | None -> ()
        attach stream st true
    let setSection sec =
        flushLabels ()
        section <- sec
    let sectionOfName (name : string) =
        let nm = name.Trim().ToLower()
        if nm.StartsWith ".text" then Ok Text
        elif nm.StartsWith ".data" || nm.StartsWith ".rodata" then Ok(Data(if nm.StartsWith ".data" then ".data" else ".rodata"))
        elif nm.StartsWith ".bss" then Ok(Data ".bss")
        elif nm.StartsWith ".note" || nm.StartsWith ".comment" || nm.StartsWith ".arm." then Error None // tool metadata: ignore
        else Error(Some(sprintf "Section '%s' is not supported: use .text, .data, .rodata or .bss" name))
    for item in items do
        if not stop then
            match item with
            | RErr e -> code.Add e
            | RLabel(line, lab) -> pending <- (line, lab) :: pending
            | RStmt(line, op, operands) ->
                let err msg = code.Add(errorStmt line msg)
                match op with
                | ".TEXT" -> setSection Text
                | ".DATA" -> setSection(Data ".data")
                | ".BSS" -> setSection(Data ".bss")
                | ".SECTION" | ".PUSHSECTION" ->
                    match splitArgs operands with
                    | name :: _ when name <> "" ->
                        match sectionOfName name with
                        | Ok sec -> setSection sec
                        | Error None -> ()
                        | Error(Some msg) -> err msg
                    | _ -> err ".section needs a section name"
                | ".END" ->
                    flushLabels ()
                    stop <- true
                | ".SYNTAX" ->
                    if operands.Trim().ToLower() <> "unified" then err "Only '.syntax unified' is supported"
                | ".CODE" ->
                    if operands.Trim() <> "32" then err "Only ARM code (.code 32) is supported, not Thumb"
                | ".THUMB" | ".THUMB_FUNC" | ".FORCE_THUMB" -> err "Thumb code is not supported: use .arm"
                | ".EQU" | ".SET" | ".EQUIV" ->
                    match splitArgs operands with
                    | [ sym; expr ] when isSymbol sym && expr <> "" -> code.Add(makeStmt line (Some sym) "EQU" expr)
                    | _ -> err (sprintf "%s needs a symbol and a value: %s symbol, expression" (op.ToLower()) (op.ToLower()))
                | _ when alignDirectives.Contains op -> pendingAlign <- Some(line, op, operands)
                | _ when dataDirectives.Contains op -> addData(makeStmt line None op operands)
                | _ when ignoredDirectives.Contains op || op.StartsWith ".CFI_" -> ()
                | ".INCLUDE" | ".MACRO" | ".ENDM" | ".IF" | ".IFDEF" | ".IFNDEF" | ".ELSE" | ".ENDIF" | ".REPT" | ".IRP" | ".ENDR" ->
                    err (sprintf "Directive %s is not supported" (op.ToLower()))
                | _ when op.StartsWith "." -> err (sprintf "Unknown directive %s" (op.ToLower()))
                | _ when armasmOnly.ContainsKey op ->
                    err (sprintf "'%s' is armasm syntax, not GNU as: use %s" op armasmOnly.[op])
                | _ ->
                    match section with
                    | Text -> addCode(makeStmt line None op operands)
                    | Data name -> err (sprintf "Instruction '%s' in %s section: instructions must be in .text" (op.ToLower()) name)
    flushLabels ()
    List.ofSeq code @ (dataSections |> Seq.collect snd |> List.ofSeq)
