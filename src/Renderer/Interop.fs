(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Interop
    Description: Minimal typed bindings for the node, electron and @electron/remote APIs the renderer uses
*)

/// Minimal typed bindings for node (fs, path, os), electron and @electron/remote.
/// The renderer runs with nodeIntegration, so these are plain require()s.
module Interop

open Fable.Core
open Fable.Core.JsInterop

type IStats =
    abstract isDirectory : unit -> bool
    abstract isFile : unit -> bool
    abstract mtimeMs : float

type IFs =
    abstract existsSync : path : string -> bool
    [<Emit("$0.readFileSync($1, 'utf8')")>]
    abstract readTextSync : path : string -> string
    abstract writeFileSync : path : string * data : string -> unit
    abstract readdirSync : path : string -> string array
    abstract statSync : path : string -> IStats
    abstract mkdirSync : path : string -> unit
    abstract unlinkSync : path : string -> unit
    abstract watch : path : string * listener : (string -> string -> unit) -> obj

type IPath =
    abstract join : [<System.ParamArray>] parts : string array -> string
    abstract dirname : path : string -> string
    abstract basename : path : string -> string
    abstract sep : string

type IOs =
    abstract homedir : unit -> string
    abstract hostname : unit -> string

let fs : IFs = importAll "fs"
let path : IPath = importAll "path"
let os : IOs = importAll "os"

[<Emit("process.platform")>]
let platform : string = jsNative

[<Emit("__dirname")>]
let dirname : string = jsNative

/// Window icon for windows opened by the renderer: Electron only reads .ico files on Windows
let appIcon () = path.join (dirname, (if platform = "win32" then "visual.ico" else "visual.png"))

/// electron APIs available in the renderer (ipcRenderer, shell)
let electron : obj = importAll "electron"

/// main-process APIs (dialog, Menu, BrowserWindow, app...) via @electron/remote
let remote : obj = importAll "@electron/remote"

let currentWindow () : obj = remote?getCurrentWindow ()

let currentWebContents () : obj = remote?getCurrentWebContents ()

let openExternal (url : string) = electron?shell?openExternal (url) |> ignore

let ipcSend (channel : string) = electron?ipcRenderer?send (channel) |> ignore

let ipcOn (channel : string) (handler : obj -> unit) = electron?ipcRenderer?on (channel, fun e _ -> handler e) |> ignore

/// command line of the main process
let mainArgv () : string array = remote?``process``?argv

/// Open-file dialog: returns the chosen paths ([] if cancelled)
let showOpenDialog (options : obj) : string list =
    match remote?dialog?showOpenDialogSync (currentWindow (), options) with
    | null -> []
    | paths -> paths |> unbox<string array> |> List.ofArray

/// Save-file dialog: returns the chosen path, or None if cancelled
let showSaveDialog (options : obj) : string option =
    match remote?dialog?showSaveDialogSync (currentWindow (), options) with
    | null -> None
    | p -> Some(unbox<string> p)

/// Message box: returns the index of the button pressed
let showMessageBox (options : obj) : int =
    remote?dialog?showMessageBoxSync (currentWindow (), options)

// ///////////////// persistent settings ///////////////////////////////
// A JSON object in <userData>/Settings, the file the old electron-settings library used,
// so settings from earlier versions are kept.

let settingsFile () : string = path.join (remote?app?getPath ("userData"), "Settings")

let private readSettings () : obj =
    try
        let f = settingsFile ()
        if fs.existsSync f then JS.JSON.parse (fs.readTextSync f) else createObj []
    with _ -> createObj []

/// Read a setting (defaultValue if it is not set)
let settingsGet (name : string) (defaultValue : obj) : obj =
    let s = readSettings ()
    if isNull (s?(name)) then defaultValue else s?(name)

/// Write a setting
let settingsSet (name : string) (value : obj) =
    let s = readSettings ()
    s?(name) <- value
    try fs.writeFileSync (settingsFile (), JS.JSON.stringify s)
    with e -> printfn "Could not save settings: %s" e.Message
