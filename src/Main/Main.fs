(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Main.Main
    Description: Electron Main Process
*)

/// This single module is compiled to JS to make the electron main process that runs node directly on the host PC and starts up the app.
module Main

open Fable.Core
open Fable.Core.JsInterop

let electron : obj = importAll "electron"
let remoteMain : obj = importAll "@electron/remote/main"
let path : obj = importAll "path"

[<Emit("process.argv")>]
let argv : string array = jsNative

[<Emit("__dirname")>]
let dirname : string = jsNative

let printHelpMessage() =
    printfn """
Visual2 command line options
----------------------------
-h, --help      - print this help message
-d, --debug     - run with browser dev tools initially open, for startup debug info logged to console
                - <F12> to open or close dev tools after startup
"""

let args = argv |> Seq.toList |> List.map (fun s -> s.ToLower())

/// returns true if any of flags are present as command line argument
let argFlagIsOn (flags : string list) =
    List.exists (fun (flag : string) -> List.contains (flag.ToLower()) args) flags

/// returns true if fl is recognised as valid
let isValidFlag (fl : string) =
    List.contains fl [ "-h"; "--help"; "-d"; "--debug"; "." ] || fl.EndsWith "electron" || fl.EndsWith ".js"

let hasHelpArgs() = argFlagIsOn [ "--help"; "-h" ]

let hasDebugArgs() = argFlagIsOn [ "--debug"; "-d" ]

let dTrace fmt s = if hasDebugArgs() then printfn fmt s

let app : obj = electron?app

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mutable mainWindow : obj option = None

remoteMain?initialize () |> ignore

/// create main renderer window of app
let createMainWindow () =
    if hasHelpArgs() then
        printHelpMessage()
        app?quit () |> ignore
    else
        dTrace "%s" "Starting to create app window..."
        let options =
            createObj [
                "width" ==> 1200
                "height" ==> 800
                // smallest size at which the toolbar fits on one row beside the widest dashboard view
                "minWidth" ==> 860
                "minHeight" ==> 600
                "show" ==> false
                "frame" ==> true
                "hasShadow" ==> true
                "backgroundColor" ==> "#5F9EA0"
                "icon" ==> path?join (dirname, "..", "app", "visual.ico")
                "webPreferences" ==> createObj [
                    // The renderer uses node (fs, path) directly and only ever loads local app pages
                    "nodeIntegration" ==> true
                    "contextIsolation" ==> false
                    "sandbox" ==> false
                    "devTools" ==> true
                ]
            ]
        let window = createNew electron?BrowserWindow options
        remoteMain?enable (window?webContents) |> ignore
        if hasDebugArgs() then
            // detached, so the dev tools do not take width from the app
            window?webContents?openDevTools (createObj [ "mode" ==> "detach" ]) |> ignore
            // echo renderer console output and failures to the terminal
            window?webContents?on ("console-message", fun (e : obj) (level : obj) (msg : obj) (line : obj) (src : obj) ->
                // Electron 35+: details are properties of the event; older: positional arguments
                let msg, line, src = if isNull msg then e?message, e?lineNumber, e?sourceId else msg, line, src
                printfn "[renderer] %O (%O:%O)" msg src line) |> ignore
            window?webContents?on ("did-fail-load", fun _ (code : obj) (desc : obj) (url : obj) ->
                printfn "[renderer] failed to load %O: %O %O" url code desc) |> ignore
            window?webContents?on ("render-process-gone", fun _ (details : obj) ->
                printfn "[renderer] process gone: %O" (details?reason)) |> ignore
        let indexHtml = path?join (dirname, "..", "app", "index.html")
        dTrace "Loading HTML: %s" indexHtml
        window?loadFile (indexHtml) |> ignore

        let mutable closeAfterSave = false
        window?on ("close", fun (e : obj) ->
            if not closeAfterSave then
                dTrace "%s" "Close event received!"
                e?preventDefault () |> ignore
                window?webContents?send ("closingWindow") |> ignore) |> ignore

        window?on ("closed", fun () -> mainWindow <- None) |> ignore

        window?on ("resize", fun () -> window?webContents?send ("resizeWindow") |> ignore) |> ignore

        // links that would open a new window open in the system browser instead
        window?webContents?setWindowOpenHandler (fun (details : obj) ->
            electron?shell?openExternal (details?url) |> ignore
            createObj [ "action" ==> "deny" ]) |> ignore

        electron?ipcMain?on ("doClose", fun () ->
            closeAfterSave <- true
            dTrace "%s" "Closing window NOW!"
            window?close () |> ignore) |> ignore

        // No menu until the renderer process installs the real one.
        // (A placeholder menu with an empty item stopped the renderer starting on Linux.)
        electron?Menu?setApplicationMenu (null) |> ignore

        window?on ("ready-to-show", fun () ->
            window?show () |> ignore
            window?focus () |> ignore
            dTrace "%s" "Window on!") |> ignore

        mainWindow <- Some window

/// ensure that if app is started again the first instance is focussed and the second quits
if not (app?requestSingleInstanceLock () : bool) then
    app?quit () |> ignore
else
    app?on ("second-instance", fun () ->
        match mainWindow with
        | Some win ->
            if win?isMinimized () then win?restore () |> ignore
            win?focus () |> ignore
        | None -> ()) |> ignore

    // This method will be called when Electron has finished
    // initialization and is ready to create browser windows.
    app?on ("ready", createMainWindow) |> ignore

    // Quit when all windows are closed.
    app?on ("window-all-closed", fun () -> app?quit () |> ignore) |> ignore

    app?on ("activate", fun () -> if mainWindow.IsNone then createMainWindow ()) |> ignore
