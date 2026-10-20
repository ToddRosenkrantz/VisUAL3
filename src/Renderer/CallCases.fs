(*
    VisUAL3
    Module: Renderer.CallCases
    Description: Named argument cases for the C Call panel, saved beside the source file
*)

/// Named argument cases for the C Call panel, kept per file tab.
/// They are saved beside the source, prog.s -> prog.ccall (JSON), when the .s is saved,
/// and loaded when the .s is opened.
module CallCases

open Fable.Core
open Fable.Core.JsInterop
open Interop

/// How a pointer argument is given
type PtrKind =
    | PArray // {1, 2, 3}
    | PString // "text"
    | POut // n zeroed elements
    | PAddr // label, address or NULL

type ParamInput = { Kind : PtrKind; Text : string }

/// One named set of arguments: parameter name -> input
type Case = { Name : string; Args : Map<string, ParamInput> }

/// The cases of one function, and the one shown
type FuncCases = { Cases : Case list; Current : string }

/// Everything kept for one source file
type FileCases = { Selected : string option; Funcs : Map<string, FuncCases> }

let private empty = { Selected = None; Funcs = Map.empty }
let mutable private byTab : Map<int, FileCases> = Map.empty

let get tabId = Map.tryFind tabId byTab |> Option.defaultValue empty
let set tabId (fc : FileCases) = byTab <- Map.add tabId fc byTab
let forget tabId = byTab <- Map.remove tabId byTab

let defaultCaseName = "case 1"

/// The cases of function fname in a tab: always at least one
let funcCases tabId fname =
    match Map.tryFind fname (get tabId).Funcs with
    | Some f when not f.Cases.IsEmpty -> f
    | _ -> { Cases = [ { Name = defaultCaseName; Args = Map.empty } ]; Current = defaultCaseName }

let currentCase (f : FuncCases) =
    f.Cases |> List.tryFind (fun c -> c.Name = f.Current) |> Option.defaultValue f.Cases.Head

let setFuncCases tabId fname (f : FuncCases) =
    let fc = get tabId
    set tabId { fc with Funcs = Map.add fname f fc.Funcs }

/// The cases file beside a source file: prog.s -> prog.ccall
let casesPath (sourcePath : string) =
    let file = path.basename sourcePath
    let stem = match file.LastIndexOf '.' with | i when i > 0 -> file.Substring(0, i) | _ -> file
    path.join (path.dirname sourcePath, stem + ".ccall")

// ///////////////////////////// JSON ///////////////////////////////////////

[<Emit("JSON.stringify($0, null, 2)")>]
let private stringify (o : obj) : string = jsNative

[<Emit("Object.keys($0)")>]
let private keys (o : obj) : string array = jsNative

let private kindName = function | PArray -> "array" | PString -> "string" | POut -> "output" | PAddr -> "address"
let private kindOf = function | "string" -> PString | "output" -> POut | "address" -> PAddr | _ -> PArray

let private present (x : obj) = not (isNullOrUndefined x)

/// true if any case holds an argument worth saving
let private hasArgs (fc : FileCases) =
    fc.Funcs |> Map.exists (fun _ f -> f.Cases |> List.exists (fun c -> c.Args |> Map.exists (fun _ a -> a.Text.Trim() <> "")))

let toJson (fc : FileCases) =
    let funcs = createEmpty<obj>
    for KeyValue(fname, f) in fc.Funcs do
        let cases =
            f.Cases
            |> List.map (fun c ->
                let args = createEmpty<obj>
                for KeyValue(p, a) in c.Args do
                    args?(p) <- createObj [ "kind" ==> kindName a.Kind; "text" ==> a.Text ]
                createObj [ "name" ==> c.Name; "args" ==> args ])
            |> List.toArray
        funcs?(fname) <- createObj [ "current" ==> f.Current; "cases" ==> cases ]
    stringify (createObj [
        "format" ==> "VisUAL3 C Call cases"
        "version" ==> 1
        "selected" ==> (match fc.Selected with | Some s -> box s | None -> null)
        "functions" ==> funcs ])

/// Read a cases file. An argument may be an object {kind, text} or just its text.
let ofJson (text : string) : Result<FileCases, string> =
    try
        let o = JS.JSON.parse text
        let str (x : obj) = if present x then string x else ""
        let funcsObj = o?functions
        let funcs =
            if not (present funcsObj) then Map.empty
            else
                keys funcsObj
                |> Array.map (fun fname ->
                    let f = funcsObj?(fname)
                    let cases =
                        (if present f?cases then (f?cases : obj array) else [||])
                        |> Array.map (fun c ->
                            let argsObj = c?args
                            let args =
                                if not (present argsObj) then Map.empty
                                else
                                    keys argsObj
                                    |> Array.map (fun p ->
                                        let a = argsObj?(p)
                                        p, (if jsTypeof a = "object" && present a then { Kind = kindOf (str a?kind); Text = str a?text }
                                            else { Kind = PArray; Text = str a }))
                                    |> Map.ofArray
                            { Name = (str c?name).Trim(); Args = args })
                        |> Array.filter (fun c -> c.Name <> "")
                        |> List.ofArray
                    fname, { Cases = cases; Current = str f?current })
                |> Map.ofArray
        let sel = str o?selected
        Ok { Selected = (if sel = "" then None else Some sel); Funcs = funcs }
    with e -> Error e.Message

// ///////////////////////////// files ///////////////////////////////////////

/// Write the cases of a tab beside its source file, if it has any arguments to keep
let save tabId (sourcePath : string) =
    let fc = get tabId
    if hasArgs fc then
        let p = casesPath sourcePath
        try fs.writeFileSync (p, toJson fc)
        with e -> Refs.showVexAlert (sprintf "Could not save the C Call cases to %s: %s" p e.Message)

/// Load the cases saved beside a source file into a tab; with no cases file the tab starts empty
let load tabId (sourcePath : string) =
    forget tabId
    let p = casesPath sourcePath
    if fs.existsSync p then
        match (try ofJson (fs.readTextSync p) with e -> Error e.Message) with
        | Ok fc -> set tabId fc
        | Error e -> Refs.showVexAlert (sprintf "Could not read the C Call cases in %s: %s" p e)
