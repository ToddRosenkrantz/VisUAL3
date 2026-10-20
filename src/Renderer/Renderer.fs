(*
    High Level Programming @ Imperial College London # Spring 2018
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Renderer
    Description: Top-level Electron Renderer Process
*)

/// Top-level renderer process function: calls everything else

module Renderer

open Interop
open Fable.Core
open Fable.Core.JsInterop
open Browser
open Browser.Types
open Browser.Dom
// open DevTools to see the message
// Menu -> View -> Toggle Developer Tools
console.log "Hi from renderer.fs" |> ignore

open Refs
open MenuBar

/// Hack to provide a constant global variable
/// set from command line arguments of main process.
/// 0 => production. 1 => dev. 2 => debug.
let setDebugLevel() =
    let argV =
        mainArgv ()
        |> Seq.toList
        |> List.tail
        |> List.map (fun s -> s.ToLower())
    let isArg s = List.contains s argV
    debugLevel <-
        if isArg "--debug" || isArg "-d" then 2
        elif isArg "-w" then 1
        else 0




/// Attach a click event on each of the map elements to a function f
/// which accepts the map element as an argument
let mapClickAttacher map (refFinder : 'a -> HTMLElement) f =
    let attachRep ref = (refFinder ref).addEventListener_click(fun _ -> f ref)
    map
    |> Map.toList
    |> List.map (fst >> attachRep)
    |> ignore

/// Called via IPC message from main process whenever main window is resized.
/// Work out any CSS dimensions that must change in response and set
/// them. Note use of CSS custom variables to control multiple
/// CSS properties
let resizeGUI() =
    let headerHeight = (getHtml "vis-header").offsetHeight
    setCustomCSS "--header-height" (sprintf "%.1fpx" headerHeight)

/// Initialization after `index.html` is loaded.
/// Equivalent of main() function
let init() =

    setDebugLevel()
    printfn "Debug level = %d" debugLevel
    // Show the body once we are ready to go!
    document.getElementById("vis-body").classList.remove("invisible")

    // Set up window close interlock using IPC from/to main process
    ipcOn "closingWindow" (fun _ -> MenuBar.ExitIfOK())

    ipcOn "resizeWindow" (fun _ -> resizeGUI())




    // Actions for the buttons
    Refs.openFileBtn.addEventListener_click (fun _ -> MenuBar.interlock "open file" MenuBar.openFile)

    Refs.saveFileBtn.addEventListener_click (fun _ -> MenuBar.interlock "save file" Files.saveFile)

    Refs.runSimulationBtn.addEventListener_click (fun _ ->
        Integration.runCode ExecutionTop.NoBreak () :> obj
    )
    stepForwardBtn.addEventListener_click (fun _ ->
        Integration.stepCode() :> obj
    )
    stepBackBtn.addEventListener_click (fun _ ->
        Integration.stepCodeBack() :> obj
    )

    resetSimulationBtn.addEventListener_click (fun _ ->
        let tabId = Refs.currentFileTabId
        if tabId >= 0 then
            Refs.editors.[tabId]?focus ()
        Integration.resetEmulator() :> obj
    )

    mapClickAttacher repToId Refs.representation (fun rep ->
        console.log (sprintf "Representation changed to %A" rep) |> ignore
        Views.setRepresentation rep |> ignore
        Views.updateMemory()
        Views.updateSymTable() :> obj
    )

    mapClickAttacher viewToIdTab Refs.viewTab (fun view ->
        console.log (sprintf "View changed to %A" view) |> ignore
        Views.setView view
        if view = CCall then CCallView.render ()
        null
    )

    (Refs.byteViewBtn).addEventListener_click(fun _ ->
        console.log "Toggling byte view" |> ignore
        Views.toggleByteView()
        Views.updateMemory() :> obj
    )

    (Refs.reverseViewBtn).addEventListener_click(fun _ ->
        console.log "Toggling reverse view" |> ignore
        Views.toggleReverseView()
        Views.updateMemory() :> obj
    )

    (Refs.newFileTab).addEventListener_click(fun _ ->
        console.log "Creating a new file tab" |> ignore
        MenuBar.interlock "create a new tab" (fun () -> Tabs.createFileTab() |> ignore))

    // create electron menus
    MenuBar.mainMenu()

    // Create an empty tab to start with
    Tabs.createFileTab() |> ignore
    printfn "Ending renderer init"
    vSettings <- checkSettings (getJSONSettings())
    Editors.updateAllEditors false

    Tooltips.addFixedToolTips()
    // replace the placeholder values in index.html with the initial register state
    updateRegisters()
    Integration.runEndHook <- CCallView.onRunEnd


/// top-level function that runs the renderer code
let handleMonacoReady (_ : Event) = init()

let handlePreventDefault (e : Event) = e.preventDefault()

let handleDrop (e : Event) =
    e.preventDefault()
    let files = (e :?> DragEvent).dataTransfer.files
    let num = (files.length |> int)
    let paths =
            [ 0..num - 1 ]
            // File.path was removed in Electron 32: webUtils.getPathForFile replaces it
            |> List.map (fun s -> electron?webUtils?getPathForFile (files.[s]) : string)
    interlock "open files" (fun () -> openListOfFiles paths |> ignore) |> ignore

document.addEventListener ("monaco-ready", handleMonacoReady)
document.addEventListener ("dragover", handlePreventDefault)
document.addEventListener ("drop", handleDrop)

