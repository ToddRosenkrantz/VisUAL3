(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.CCallView
    Description: Dashboard panel to call an assembly function as C would, from an "@ C:" prototype
*)

/// "C Call" dashboard panel.
/// Lists the "@ C: <prototype>" comments in the current file, builds an input form for the
/// selected function's parameters, starts the call (via Integration, so stepping works)
/// and shows the decoded result, buffers and calling-convention checks.
module CCallView

open Fable.Core.JsInterop
open Browser
open Browser.Types
open Browser.Dom
open Refs
open CCall

/// How a pointer argument is given
type PtrKind =
    | PArray // {1, 2, 3}
    | PString // "text"
    | POut // n zeroed elements
    | PAddr // label, address or NULL

type ParamInput = { Kind : PtrKind; Text : string }

/// user input remembered per (function, parameter) while the panel is re-rendered
let mutable private inputs : Map<string * string, ParamInput> = Map.empty
let mutable private selected : string option = None
/// call currently running in the emulator
let mutable private activeCall : CallSetup option = None
let mutable private lastResult : (CallSetup * CallResult) option = None
let mutable private lastErrors : string list = []
let mutable private history : string list list = []

// ///////////////////////////// DOM helpers ///////////////////////////////////

let private el (tag : string) (cls : string) (children : HTMLElement list) =
    let e = document.createElement tag
    if cls <> "" then e.className <- cls
    children |> List.iter (fun c -> e.appendChild c |> ignore)
    e

let private txt tag cls (s : string) =
    let e = el tag cls []
    e.textContent <- s
    e

// ///////////////////////////// parameters ///////////////////////////////////

/// AAPCS location of argument i (words: r0-r3 then the stack)
let argLocation i = if i < 4 then sprintf "r%d" i else sprintf "[sp, #%d]" ((i - 4) * 4)

let private isCharLike t = match t with | CIntT(CChar _) | CVoid -> true | _ -> false

let private ptrKinds elem =
    [ yield PArray, "array"
      if isCharLike elem then yield PString, "string"
      yield POut, "output buffer"
      yield PAddr, "address / label / NULL" ]

let private defaultInput (p : CParam) =
    match p.PType with
    | CPtr(elem, _) when isCharLike elem -> { Kind = PString; Text = "" }
    | CPtr _ -> { Kind = PArray; Text = "" }
    | CIntT CBool -> { Kind = PArray; Text = "0" }
    | _ -> { Kind = PArray; Text = "" }

let private inputOf (f : Prototype) (p : CParam) =
    Map.tryFind (f.FName, p.PName) inputs |> Option.defaultValue (defaultInput p)

let private setInput (f : Prototype) (p : CParam) v = inputs <- Map.add (f.FName, p.PName) v inputs

let private valueHint t =
    match t with
    | CIntT(CChar(false, true)) -> "0..255 or 'A' (plain char is unsigned on ARM)"
    | CIntT(CChar(false, false)) -> "0..255 or 'A'"
    | CIntT(CChar(true, _)) -> "-128..127 or 'A'"
    | CIntT(CShort true) -> "-32768..32767"
    | CIntT(CShort false) -> "0..65535"
    | CIntT(CInt32 true) -> "e.g. 42, -1, 0x1F, 'A'"
    | CIntT(CInt32 false) -> "e.g. 42, 0xFFFFFFFF"
    | _ -> ""

let private ptrHint kind elem =
    match kind with
    | PArray -> sprintf "elements, e.g. {10, -2, 0x30} (%s, %d byte%s each)" (typeName elem) (sizeOf elem) (if sizeOf elem = 1 then "" else "s")
    | PString -> "text, e.g. hello\\n (a 0 byte is added)"
    | POut -> "number of zeroed elements, e.g. 4"
    | PAddr -> "a label from the program, an address, or NULL"

/// Turn a remembered input into the argument for CCall
let private argOf (p : CParam) (inp : ParamInput) : Result<ArgInput, string> =
    let t = inp.Text.Trim()
    let need what = Error(sprintf "%s: enter %s" p.PName what)
    match p.PType, inp.Kind with
    | CIntT CBool, _ -> Ok(Value(if t = "1" then "1" else "0"))
    | (CIntT _ | CVoid), _ -> if t = "" then need "a value" else Ok(Value t)
    | CPtr _, PArray ->
        let inner = if t.StartsWith "{" && t.EndsWith "}" then t.Substring(1, t.Length - 2) else t
        Ok(Elements(GnuLex.splitArgs inner))
    | CPtr _, PString ->
        if t.Length >= 2 && t.StartsWith "\"" && t.EndsWith "\"" then Ok(Str t)
        else Ok(Str("\"" + inp.Text.Replace("\"", "\\\"") + "\""))
    | CPtr _, POut ->
        match System.Int32.TryParse t with
        | true, n when n >= 0 -> Ok(OutBuffer n)
        | _ -> need "a number of elements"
    | CPtr _, PAddr -> if t = "" then need "a label, address or NULL" else Ok(Address t)

// ///////////////////////////// calling ///////////////////////////////////////

let rec private startCall (proto : Prototype) (stepInto : bool) =
    let args = proto.Params |> List.map (fun p -> argOf p (inputOf proto p))
    match args |> List.choose (function | Error e -> Some e | _ -> None) with
    | errs when errs <> [] -> showErrors errs
    | _ ->
        let args = args |> List.choose (function | Ok a -> Some a | _ -> None)
        match Integration.tryParseAndIndentCode currentFileTabId with
        | None -> showErrors [ "The program has assembly errors (marked in the editor): fix them first" ]
        | Some(lim, _) ->
            match prepareCall lim (textOfTId currentFileTabId) proto args with
            | Error errs -> showErrors errs
            | Ok setup ->
                activeCall <- Some setup
                lastErrors <- []
                render ()
                Integration.startCCall setup.Run stepInto

and private showErrors errs =
    lastErrors <- errs
    render ()

/// Called by Integration whenever a run stops: decode the result if it is our call
and onRunEnd (ri : ExecutionTop.RunInfo) =
    match activeCall with
    | Some setup when ri.dpInit.Regs = setup.Run.dpInit.Regs ->
        match ri.State with
        | ExecutionTop.PSBreak -> () // paused at a breakpoint: the call continues
        | state ->
            let res = decodeResult setup ri
            lastResult <- Some(setup, res)
            if state <> ExecutionTop.PSRunning then
                activeCall <- None
                history <- (formatCall setup res |> List.truncate 3) :: history |> List.truncate 10
            render ()
    | _ -> ()

// ///////////////////////////// rendering /////////////////////////////////////

and private paramRow (proto : Prototype) (i : int) (p : CParam) =
    let inp = inputOf proto p
    let loc = txt "td" "ccall-loc" (argLocation i)
    let name = txt "td" "ccall-param" (sprintf "%s %s" (typeName p.PType) p.PName)
    let control =
        match p.PType with
        | CIntT CBool ->
            let sel = document.createElement_select()
            for (v, label) in [ "0", "false"; "1", "true" ] do
                let o = document.createElement_option()
                o.value <- v
                o.text <- label
                sel.appendChild o |> ignore
            sel.value <- (if inp.Text = "1" then "1" else "0")
            sel.onchange <- fun _ -> setInput proto p { inp with Text = sel.value }
            el "div" "" [ sel ]
        | CPtr(elem, _) ->
            let input = document.createElement_input()
            input.value <- inp.Text
            input.placeholder <- ptrHint inp.Kind elem
            input.className <- "ccall-input"
            let kinds = ptrKinds elem
            let sel = document.createElement_select()
            for (k, label) in kinds do
                let o = document.createElement_option()
                o.value <- label
                o.text <- label
                sel.appendChild o |> ignore
            sel.value <- (kinds |> List.find (fst >> (=) inp.Kind) |> snd)
            sel.onchange <- fun _ ->
                let k = kinds |> List.find (snd >> (=) sel.value) |> fst
                setInput proto p { (inputOf proto p) with Kind = k }
                render ()
            input.oninput <- fun _ -> setInput proto p { (inputOf proto p) with Text = input.value }
            el "div" "ccall-ptr" [ sel; input ]
        | t ->
            let input = document.createElement_input()
            input.value <- inp.Text
            input.placeholder <- valueHint t
            input.className <- "ccall-input"
            input.oninput <- fun _ -> setInput proto p { (inputOf proto p) with Text = input.value }
            el "div" "" [ input ]
    el "tr" "" [ loc; name; el "td" "" [ control ] ]

and private formatElem (t : CType) (v : int64) =
    match t with
    | CPtr _ -> sprintf "0x%X" v
    | CIntT(CChar _) when v >= 32L && v < 127L -> sprintf "%d '%c'" v (char (int v))
    | CIntT CBool -> if v = 0L then "false" else "true"
    | _ -> string v

and private resultView (setup : CallSetup) (res : CallResult) =
    let p = setup.Proto
    let outcomeCls, outcome =
        match res.Outcome with
        | Returned -> "ccall-ok", sprintf "%s returned" p.FName
        | RanOffEnd -> "ccall-err", "Did not return: execution reached the end of the code (missing bx lr?)"
        | StepLimit n -> "ccall-warn", sprintf "Not returned after %d instructions (infinite loop?). Continue with Run or Step." n
        | RunError e -> "ccall-err", sprintf "Run time error %s" e
    let retLines =
        match res.Outcome, p.Ret with
        | Returned, CVoid -> [ txt "div" "" "void function: r0 is not a result" ]
        | Returned, t ->
            [ txt "div" "ccall-return" (sprintf "%s result: %s" (typeName t) res.ReturnText)
              txt "div" "ccall-raw" (sprintf "r0 = 0x%08X" res.R0) ]
        | _ -> []
    let buffers =
        res.BufferResults |> List.map (fun br ->
            let b = br.Buffer
            let head =
                txt "div" "ccall-buf-head"
                    (sprintf "%s: %s%s[%d] at 0x%X" b.BParam (if b.BConst then "const " else "") (typeName b.BElem) b.BCount b.BAddr)
            let rows =
                List.zip b.BInitial br.After
                |> List.mapi (fun i (before, after) ->
                    let changed = List.contains i br.Changed
                    el "tr" (if changed then "ccall-changed" else "") [
                        txt "td" "" (string i)
                        txt "td" "" (formatElem b.BElem before)
                        txt "td" "" (formatElem b.BElem after) ])
            let table =
                el "table" "ccall-buf" (el "tr" "" [ txt "th" "" "#"; txt "th" "" "before"; txt "th" "" "after" ] :: rows)
            el "div" "" [ head; (if b.BCount = 0 then txt "div" "ccall-raw" "(empty)" else table) ])
    let problems = res.Problems |> List.map (fun pr -> txt "div" "ccall-problem" ("⚠ " + pr))
    el "div" "ccall-result" ([ txt "div" outcomeCls outcome ] @ retLines @ buffers @ problems)

and private helpView () =
    el "div" "ccall-help" [
        txt "p" "" "Declare a function with a C prototype in a comment, then call it from here:"
        txt "pre" "" "@ C: extern int sum_array(const int *a, int n);\n        .global sum_array\nsum_array:\n        ...\n        bx lr"
        txt "p" "" "Types: char, signed/unsigned char, short, int, long, unsigned, int8_t..uint32_t, size_t, _Bool, pointers to these or void (const allowed), and arrays as pointers. Returns: void, the integer types, _Bool and pointers."
        txt "p" "" "Arguments are passed as AAPCS requires: the first four in r0-r3, the rest on the stack. Pointer arguments are built in memory at 0x10000000." ]

and render () =
    let root = getHtml "view-call"
    root.innerHTML <- ""
    let lines = if currentFileTabId < 0 then [] else textOfTId currentFileTabId
    let found = findPrototypes lines
    let protos = found |> List.choose (function | Ok p -> Some p | _ -> None)
    let protoErrors =
        found |> List.choose (function
            | Error(line, e) -> Some(txt "div" "ccall-problem" (sprintf "line %d: @ C: %s" line e))
            | _ -> None)
    let refresh = txt "button" "btn btn-default ccall-refresh" "↻ Refresh"
    refresh.addEventListener ("click", fun _ -> render ())
    let header = el "div" "ccall-header" [ txt "span" "ccall-title" "Call a function from C"; refresh ]
    let body =
        match protos with
        | [] -> [ helpView () ]
        | _ ->
            let proto =
                match selected |> Option.bind (fun n -> protos |> List.tryFind (fun p -> p.FName = n)) with
                | Some p -> p
                | None -> protos.Head
            selected <- Some proto.FName
            let chooser =
                let sel = document.createElement_select()
                for p in protos do
                    let o = document.createElement_option()
                    o.value <- p.FName
                    o.text <- p.FName
                    sel.appendChild o |> ignore
                sel.value <- proto.FName
                sel.onchange <- fun _ -> selected <- Some sel.value; render ()
                el "div" "ccall-chooser" [ txt "span" "" "Function: "; sel ]
            let signature =
                let ps = proto.Params |> List.map (fun p -> typeName p.PType + " " + p.PName) |> String.concat ", "
                txt "div" "ccall-signature" (sprintf "%s %s(%s)" (typeName proto.Ret) proto.FName (if ps = "" then "void" else ps))
            let table =
                match proto.Params with
                | [] -> txt "div" "ccall-raw" "no parameters"
                | ps ->
                    el "table" "ccall-params"
                        (el "tr" "" [ txt "th" "" "in"; txt "th" "" "parameter"; txt "th" "" "argument" ]
                         :: List.mapi (paramRow proto) ps)
            let callBtn = txt "button" "btn btn-default ccall-call" "Call"
            callBtn.addEventListener ("click", fun _ -> startCall proto false)
            let stepBtn = txt "button" "btn btn-default" "Step into"
            stepBtn.addEventListener ("click", fun _ -> startCall proto true)
            let buttons = el "div" "ccall-buttons" [ callBtn; stepBtn ]
            let status =
                match activeCall with
                | Some s -> [ txt "div" "ccall-warn" (sprintf "Call to %s in progress: use Run or Step to continue" s.Proto.FName) ]
                | None -> []
            let errors = lastErrors |> List.map (fun e -> txt "div" "ccall-err" e)
            let result =
                match lastResult with
                | Some(setup, res) -> [ resultView setup res ]
                | None -> []
            let hist =
                match history with
                | [] -> []
                | h ->
                    [ txt "div" "ccall-buf-head" "History"
                      txt "pre" "ccall-history" (h |> List.map (String.concat "\n") |> String.concat "\n") ]
            [ chooser; signature; table; buttons ] @ status @ errors @ result @ hist
    (header :: protoErrors @ body) |> List.iter (fun e -> root.appendChild e |> ignore)
