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
open CallCases
/// call currently running in the emulator
let mutable private activeCall : CallSetup option = None
let mutable private lastResult : (CallSetup * CallResult) option = None
/// what the case expected when the running call started, and the checks of the last result
let mutable private activeExpect : Expectation = { Return = ""; After = Map.empty }
let mutable private lastChecks : CheckResult list = []
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

/// AAPCS (hard-float) location of argument i of proto: r0-r3, s0-s15, d0-d7 or the stack
let argLocation (proto : Prototype) i =
    let locs, _ = argLocations (proto.Params |> List.map (fun p -> p.PType))
    argLocText locs.[i]

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
    (currentCase (funcCases currentFileTabId f.FName)).Args |> Map.tryFind p.PName |> Option.defaultValue (defaultInput p)

/// change an argument of the current case; the source tab becomes unsaved (cases are saved with it)
let private setInput (f : Prototype) (p : CParam) v =
    let fcs = funcCases currentFileTabId f.FName
    let cur = currentCase fcs
    if Map.tryFind p.PName cur.Args <> Some v then
        let cur' = { cur with Args = Map.add p.PName v cur.Args }
        let cases = fcs.Cases |> List.map (fun c -> if c.Name = cur.Name then cur' else c)
        setFuncCases currentFileTabId f.FName { Cases = cases; Current = cur.Name }
        Tabs.setTabUnsaved currentFileTabId

/// an expectation of the current case: "return", or a pointer parameter's name
let private expectOf (f : Prototype) (key : string) =
    (currentCase (funcCases currentFileTabId f.FName)).Expect |> Map.tryFind key |> Option.defaultValue ""

let private setExpect (f : Prototype) (key : string) (v : string) =
    let fcs = funcCases currentFileTabId f.FName
    let cur = currentCase fcs
    if expectOf f key <> v then
        let cur' = { cur with Expect = Map.add key v cur.Expect }
        let cases = fcs.Cases |> List.map (fun c -> if c.Name = cur.Name then cur' else c)
        setFuncCases currentFileTabId f.FName { Cases = cases; Current = cur.Name }
        Tabs.setTabUnsaved currentFileTabId

/// the function shown in the panel for the current file
let private selected () = (CallCases.get currentFileTabId).Selected
let private select name = CallCases.set currentFileTabId { CallCases.get currentFileTabId with Selected = Some name }

let private valueHint t =
    match t with
    | CIntT(CChar(false, true)) -> "0..255 or 'A' (plain char is unsigned on ARM)"
    | CIntT(CChar(false, false)) -> "0..255 or 'A'"
    | CIntT(CChar(true, _)) -> "-128..127 or 'A'"
    | CIntT(CShort true) -> "-32768..32767"
    | CIntT(CShort false) -> "0..65535"
    | CIntT(CInt32 true) -> "e.g. 42, -1, 0x1F, 'A'"
    | CIntT(CInt32 false) -> "e.g. 42, 0xFFFFFFFF"
    | CIntT(CInt64 true) -> "e.g. -5, 3000000000, 0x7FFFFFFFFFFFFFFF"
    | CIntT(CInt64 false) -> "e.g. 42, 0xFFFFFFFFFFFFFFFF"
    | CFloat _ -> "e.g. 1.5, -0.25, 3e8"
    | _ -> ""

let private ptrHint kind elem =
    match kind with
    | PArray ->
        let example = match elem with | CFloat _ -> "{1.5, -2, 0.25}" | _ -> "{10, -2, 0x30}"
        sprintf "elements, e.g. %s (%s, %d byte%s each)" example (typeName elem) (sizeOf elem) (if sizeOf elem = 1 then "" else "s")
    | PString -> "text, e.g. hello\\n (a 0 byte is added)"
    | POut -> "number of zeroed elements, e.g. 4"
    | PAddr -> "a label from the program, an address, or NULL"

/// Turn a remembered input into the argument for CCall
let private argOf (p : CParam) (inp : ParamInput) : Result<ArgInput, string> =
    let t = inp.Text.Trim()
    let need what = Error(sprintf "%s: enter %s" p.PName what)
    match p.PType, inp.Kind with
    | CIntT CBool, _ -> Ok(Value(if t = "1" then "1" else "0"))
    | (CIntT _ | CVoid | CFloat _), _ -> if t = "" then need "a value" else Ok(Value t)
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
                activeExpect <-
                    { Return = expectOf proto "return"
                      After = proto.Params |> List.map (fun p -> p.PName, expectOf proto p.PName) |> Map.ofList }
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
            lastChecks <- checkExpectations setup res activeExpect
            if state <> ExecutionTop.PSRunning then
                activeCall <- None
                let checked =
                    match lastChecks with
                    | [] -> []
                    | cs ->
                        let passed = cs |> List.filter (fun c -> c.Passed) |> List.length
                        [ sprintf "  checks: %d passed, %d failed" passed (cs.Length - passed) ]
                history <- ((formatCall setup res |> List.truncate 3) @ checked) :: history |> List.truncate 10
            render ()
    | _ -> ()

// ///////////////////////////// rendering /////////////////////////////////////

and private paramRow (proto : Prototype) (i : int) (p : CParam) =
    let inp = inputOf proto p
    let loc = txt "td" "ccall-loc" (argLocation proto i)
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
            let expected =
                match p.PType with
                | CPtr(_, false) ->
                    let e = document.createElement_input()
                    e.value <- expectOf proto p.PName
                    e.placeholder <- (if isCharLike elem then "expected after the call: {…} or \"text\" (optional)" else "expected after the call, e.g. {0, 1, 4} (optional)")
                    e.className <- "ccall-input ccall-expect"
                    e.oninput <- fun _ -> setExpect proto p.PName e.value
                    [ e :> HTMLElement ]
                | _ -> []
            el "div" "ccall-ptr" ([ sel :> HTMLElement; input :> HTMLElement ] @ expected)
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
    | CFloat d -> formatFloat d v
    | CIntT(CInt64 false) -> string (uint64 v)
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
            let raw =
                match t, res.ReturnValue with
                | CFloat false, Some v -> sprintf "s0 = 0x%08X" (uint32 v)
                | CFloat true, Some v -> sprintf "d0 = 0x%08X_%08X" (uint32 ((v >>> 32) &&& 0xFFFFFFFFL)) (uint32 (v &&& 0xFFFFFFFFL))
                | _ -> sprintf "r0 = 0x%08X" res.R0
            [ txt "div" "ccall-return" (sprintf "%s result: %s" (typeName t) res.ReturnText)
              txt "div" "ccall-raw" raw ]
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
    let checks =
        lastChecks |> List.map (fun c ->
            txt "div" (if c.Passed then "ccall-check-pass" else "ccall-check-fail")
                (sprintf "%s %s: %s" (if c.Passed then "✔" else "✘") c.What c.Detail))
    el "div" "ccall-result" ([ txt "div" outcomeCls outcome ] @ checks @ retLines @ buffers @ problems)

and private helpView () =
    el "div" "ccall-help" [
        txt "p" "" "Declare a function with a C prototype in a comment, then call it from here:"
        txt "pre" "" "@ C: extern int sum_array(const int *a, int n);\n        .global sum_array\nsum_array:\n        ...\n        bx lr"
        txt "p" "" "Types: char, signed/unsigned char, short, int, long, unsigned, int8_t..uint32_t, size_t, _Bool, float, double, pointers to these or void (const allowed), and arrays as pointers. Returns: void, the integer types, _Bool, float, double and pointers."
        txt "p" "" "Arguments are passed as the hard-float AAPCS (armv7hf) requires: integers and pointers in r0-r3, float in s0-s15 and double in d0-d7, the rest on the stack. Results come back in r0, s0 or d0. Pointer arguments are built in memory at 0x10000000." ]

/// Case row: pick a saved set of arguments, save the current ones under a new name, or delete one
and private caseChooser (proto : Prototype) =
    let tab = currentFileTabId
    let fcs = funcCases tab proto.FName
    let cur = currentCase fcs
    let changed (f : FuncCases) =
        setFuncCases tab proto.FName f
        Tabs.setTabUnsaved tab
        render ()
    let sel = document.createElement_select()
    sel.className <- "ccall-case-select"
    for c in fcs.Cases do
        let o = document.createElement_option()
        o.value <- c.Name
        o.text <- c.Name
        sel.appendChild o |> ignore
    sel.value <- cur.Name
    // handlers read the cases afresh: arguments typed since this row was drawn are in the store
    let now () = funcCases tab proto.FName
    sel.onchange <- fun _ ->
        setFuncCases tab proto.FName { now () with Current = sel.value }
        render ()
    let name = document.createElement_input()
    name.className <- "ccall-case-name"
    name.placeholder <- "new case name"
    let saveAs = txt "button" "btn btn-default ccall-case-btn" "Save as"
    saveAs.title <- "Save the arguments below as a new case with this name"
    let addCase () =
        let n = name.value.Trim()
        let fcs = now ()
        if n = "" then name.focus ()
        elif fcs.Cases |> List.exists (fun c -> c.Name = n) then showErrors [ sprintf "There is already a case named '%s'" n ]
        else
            lastErrors <- []
            let c = currentCase fcs
            changed { Cases = fcs.Cases @ [ { Name = n; Args = c.Args; Expect = c.Expect } ]; Current = n }
    saveAs.addEventListener ("click", fun _ -> addCase ())
    name.addEventListener ("keydown", fun (e : Event) -> if (e :?> KeyboardEvent).key = "Enter" then addCase ())
    let delete = txt "button" "btn btn-default ccall-case-btn" "Delete"
    delete.title <- "Delete this case"
    if fcs.Cases.Length < 2 then delete.setAttribute ("disabled", "true")
    delete.addEventListener ("click", fun _ ->
        let fcs = now ()
        let rest = fcs.Cases |> List.filter (fun c -> c.Name <> (currentCase fcs).Name)
        if not rest.IsEmpty then changed { Cases = rest; Current = rest.Head.Name })
    el "div" "ccall-chooser ccall-cases" [ txt "span" "" "Case: "; sel; name; saveAs; delete ]

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
                match selected () |> Option.bind (fun n -> protos |> List.tryFind (fun p -> p.FName = n)) with
                | Some p -> p
                | None -> protos.Head
            select proto.FName
            let chooser =
                let sel = document.createElement_select()
                for p in protos do
                    let o = document.createElement_option()
                    o.value <- p.FName
                    o.text <- p.FName
                    sel.appendChild o |> ignore
                sel.value <- proto.FName
                sel.onchange <- fun _ -> select sel.value; render ()
                el "div" "ccall-chooser" [ txt "span" "" "Function: "; sel ]
            let caseRow = caseChooser proto
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
            let expectedResult =
                match proto.Ret with
                | CVoid -> []
                | t ->
                    let e = document.createElement_input()
                    e.value <- expectOf proto "return"
                    e.placeholder <-
                        (match t with
                         | CFloat _ -> "optional, e.g. 2.5 (matched within rounding)"
                         | CPtr _ -> "optional: a label, an address or NULL"
                         | _ -> "optional, e.g. 6")
                    e.className <- "ccall-input ccall-expect"
                    e.oninput <- fun _ -> setExpect proto "return" e.value
                    [ el "div" "ccall-expected-row" [ txt "span" "" (sprintf "Expected %s result: " (typeName t)); e ] ]
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
            [ chooser; caseRow; signature; table ] @ expectedResult @ [ buttons ] @ status @ errors @ result @ hist
    (header :: protoErrors @ body) |> List.iter (fun e -> root.appendChild e |> ignore)
