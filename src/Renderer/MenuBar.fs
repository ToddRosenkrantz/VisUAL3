(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.MenuBar
    Description: Code for the app menus
*)

/// implement menu functions
module MenuBar
open EEExtensions
open Fable.Core
open Fable.Core.JsInterop
open Interop
open Browser.Dom
open Refs
open Settings
open Tabs




let display runMode =
    match runMode with
    | ExecutionTop.ResetMode -> "ResetMode"
    | ExecutionTop.FinishedMode _ -> "FinishedMode"
    | ExecutionTop.ActiveMode _ -> "ActiveMode"
    | ExecutionTop.ParseErrorMode -> "ParseErrorMode"
    | ExecutionTop.RunErrorMode _ -> "RunErrorMode"



/// Wrap an action so that it can only happen if simulator is stopped.
/// Converts unit -> unit into obj. Must be called as fun () -> interlock actionName action.
/// Suitable for use as JS callback.
let interlock (actionName : string) (action : Unit -> Unit) = (
        if debugLevel > 0 then printf "Interlock : runMode=%A" (display runMode)
        let actIfConfirmed buttonNum =
            printf "button %A" buttonNum
            match buttonNum with
            | false -> ()
            | _ -> Integration.resetEmulator(); action()
        match Refs.runMode with
        | ExecutionTop.ResetMode
        | ExecutionTop.ParseErrorMode -> action() :> obj
        | _ -> showVexConfirm (sprintf "Can't %s while simulator is running <br> <br>Reset and %s<br>" actionName actionName) actIfConfirmed :> obj
    )
 /// Wrap an action so that it can only happen if simulator is stopped.
 /// Operates on (Unit->Unit) to make (Unit->Unit).
 /// Suitable for use as action in menu.
let interlockAction (actionName : string) (action : Unit -> Unit) = (fun () ->
    interlock actionName action |> ignore
    )

(****************************************************************************************************
 *
 *                                      MENU OPERATIONS
 *
 ****************************************************************************************************)


 /// Load file text into the specified tab
let loadFileIntoTab tId (fileData : string) =
    if currentFileTabId = tId then
        Integration.resetEmulator()
    let editor = editors.[tId]
    editor?setValue (fileData) |> ignore
    setTabSaved tId


let openListOfFiles (fLst : string list) =
    let makeTab path =
        let tId = createNamedFileTab (Files.baseFilePath path) path
        (fun tId -> Files.setTabFilePath tId path) |> ignore
        (path, tId)
    let readPath (path, tId) =
        try
            loadFileIntoTab tId (fs.readTextSync path)
            Files.setTabFilePath tId path
        with e -> showVexAlert (sprintf "Could not read %s: %s" path e.Message)
        [ tId ]
    if debugLevel > 0 then printfn "File list to open is: %A" fLst
    fLst
    |> Files.resultUndefined()
    |> Result.map Files.updateCurrentPathFromList
    |> Result.map (List.map (fun p -> Files.filterBadName false p) >> List.concat)
    |> Result.map (List.map (makeTab >> readPath))
    |> Result.map List.concat
    |> Result.map (function | tId :: _ -> selectFileTab tId; () | [] -> ())


let openFile() =
    showOpenDialog (createObj [
                        "properties" ==> [| "openFile"; "multiSelections" |]
                        "filters" ==> Files.fileFilterOpts
                        "defaultPath" ==> vSettings.CurrentFilePath ])
    |> openListOfFiles
    |> ignore





let loadDemo() =
    Tabs.createFileTab()
    |> fun tId ->
        let sampleFileName = Tests.sampleDir + "karatsuba.s"
        printfn "Reading sample file: %s" sampleFileName
        try loadFileIntoTab tId (fs.readTextSync sampleFileName)
        with e -> showVexAlert (sprintf "Could not read %s: %s" sampleFileName e.Message)
        Tabs.setTabSaved tId



let showQuitMessage (callBack : bool -> unit) =
    let mess = "You have unsaved changes. Are you sure you want to exit and lose changes?"
    let buttons = [ "Save"; "Exit without saving" ]
    Refs.showVexConfirm mess callBack


/// Check if there is any unsaved info. Display dialog asking for confirmation if there is.
/// Otherwise exit.
let ExitIfOK() =
    let close() = ipcSend "doClose"
    let callback (result : bool) =
        match result with
        | false -> ()
        | true -> close()
    let tabL = Tabs.unsavedTabs()
    if tabL <> [] then
        showQuitMessage callback
    else close()

(****************************************************************************************************
 *
 *                                  MENU HELPER FUNCTIONS
 *
 ****************************************************************************************************)


/// Electron MenuItem constructor options: https://www.electronjs.org/docs/latest/api/menu-item
type MenuItemOptions = obj

/// Electron menu item roles used here
[<RequireQualifiedAccess>]
module MenuItemRole =
    let Cut = "cut"
    let Copy = "copy"
    let Paste = "paste"
    let Togglefullscreen = "togglefullscreen"
    let Zoomin = "zoomIn"
    let Zoomout = "zoomOut"
    let Resetzoom = "resetZoom"

let menuSeparator : MenuItemOptions = createObj [ "type" ==> "separator" ]

/// Make action menu item from name, opt key to trigger, and action.
let makeItem (label : string) (accelerator : string option) (iAction : unit -> unit) : MenuItemOptions =
    let item = createObj [ "label" ==> label; "click" ==> (fun () -> iAction()) ]
    accelerator |> Option.iter (fun a -> item?accelerator <- a)
    item

/// Make role menu from name, opt key to trigger, and action.
let makeRoleItem label accelerator (role : string) =
    let item = makeItem label accelerator id
    item?role <- role
    item

/// make conditional menu item from condition, name, opt key to trigger, and role
let makeCondRoleItem cond label accelerator role =
    let item = makeRoleItem label accelerator role
    item?visible <- cond
    item

/// make conditional menu item from condition, name, opt key to trigger, and action
let makeCondItem cond label accelerator action =
    let item = makeItem label accelerator action
    item?visible <- cond
    item

/// Make a new menu from a a list of menu items
let makeMenu (name : string) (table : MenuItemOptions list) : MenuItemOptions =
    createObj [ "label" ==> name; "submenu" ==> List.toArray table ]


(****************************************************************************************************
 *
 *                                         MENUS
 *
 ****************************************************************************************************)
let fileMenu() =
    makeMenu "File" [
            makeItem "New" (Some "CmdOrCtrl+N") (interlockAction "make new file tab" (createFileTab >> ignore))
            menuSeparator
            makeItem "Save" (Some "CmdOrCtrl+S") (interlockAction "save file" Files.saveFile)
            makeItem "Save As" (Some "CmdOrCtrl+Shift+S") (interlockAction "save file" Files.saveFileAs)
            makeItem "Open" (Some "CmdOrCtrl+O") (interlockAction "open file" (openFile >> ignore))
            menuSeparator
            makeItem "Close" (Some "CmdOrCtrl+W") (interlockAction "close file" deleteCurrentTab)
            menuSeparator
            makeItem "Quit" (Some "CmdOrCtrl+Q") ExitIfOK
        ]



let editMenu() =
    makeMenu "Edit" [
        makeItem "Undo" (Some "CmdOrCtrl+Z") Files.editorUndo
        makeItem "Redo" (Some "CmdOrCtrl+Shift+Z") Files.editorRedo
        menuSeparator
        makeRoleItem "Cut" (Some "CmdOrCtrl+X") MenuItemRole.Cut
        makeRoleItem "Copy" (Some "CmdOrCtrl+C") MenuItemRole.Copy
        makeRoleItem "Paste" (Some "CmdOrCtrl+V") MenuItemRole.Paste
        menuSeparator
        makeItem "Select All" (Some "CmdOrCtrl+A") Files.editorSelectAll
        menuSeparator
        makeItem "Find" (Some "CmdOrCtrl+F") Files.editorFind
        makeItem "Replace" (Some "CmdOrCtrl+H") Files.editorFindReplace
        menuSeparator
        makeItem "Increase Font Size" (Some "CmdOrCtrl+.") (fun () -> Settings.alterFontSize 2)
        makeItem "Decrease Font Size" (Some "CmdOrCtrl+,") (fun () -> Settings.alterFontSize -2)
        makeItem "Preferences" Core.Option.None (interlockAction "show preferences tab" createSettingsTab)
    ]

let viewMenu() =
        let devToolsKey = if platform = "darwin" then "Alt+Command+I" else "Ctrl+Shift+I"
        makeMenu "View" [
            makeRoleItem "Toggle Fullscreen" (Some "F11") MenuItemRole.Togglefullscreen
            menuSeparator
            makeRoleItem "Zoom In" (Some "CmdOrCtrl+Plus") MenuItemRole.Zoomin
            makeRoleItem "Zoom Out" (Some "CmdOrCtrl+-") MenuItemRole.Zoomout
            makeRoleItem "Reset Zoom" (Some "CmdOrCtrl+0") MenuItemRole.Resetzoom
            menuSeparator
            makeCondItem (debugLevel > 0) "Toggle Dev Tools" (Some devToolsKey) (fun () -> currentWebContents()?toggleDevTools () |> ignore)
        ]






let popupMenu (items : MenuItemOptions list) =
    let menu = remote?Menu?buildFromTemplate (List.toArray items)
    menu?popup (createObj [ "window" ==> currentWindow() ]) |> ignore

let testMenu() =
        let runToBranch() = ()
        let runSteps() =
            showVexValidatedPrompt "steps forward" validPosInt (int64 >> (Integration.runEditorTab ExecutionTop.NoBreak)) "Number of steps forward"
        let runStepsBack() =
            showVexValidatedPrompt "steps back" validPosInt (int64 >> (Integration.stepCodeBackBy)) "Number of steps back"
        let runSingleTest() =
            match Testbench.getTestList() with
            | [] -> showVexAlert "Can't find any tests. Have you loaded a valid testbench?"
            | lst -> popupMenu (List.map (fun (test : ExecutionTop.Test) ->
                        let name = sprintf "Step code with initial data from Test %d" test.TNum
                        let actFun = fun () -> Integration.startTest test
                        makeItem name Core.None actFun) lst)
        let runTo cond () = Integration.runEditorTab cond System.Int64.MaxValue
        makeMenu "Test" [
            makeItem "Step <-" (Some "F3") Integration.stepCodeBack
            makeItem "Step ->" (Some "F4") Integration.stepCode
            makeItem "Step to next call" (Some "F5") (runTo ExecutionTop.ToSubroutine)
            makeItem "Step to next return" (Some "F6") (runTo ExecutionTop.ToReturn)
            makeItem "Step forward by" Core.Option.None runSteps
            makeItem "Step back by" Core.Option.None runStepsBack
            menuSeparator
            makeItem "Step into test" Core.Option.None (interlockAction "Test" runSingleTest)
            makeItem "Run all tests" Core.Option.None (interlockAction "Testbench" Integration.runTestbenchOnCode)
        ]


/// GPL "Appropriate Legal Notices": copyright, no warranty, and how to see the licence
let licenseNoticeHtml () =
    sprintf "<h4>VisUAL3 v%s</h4>" Refs.appVersion +
    "<p>Copyright &copy; 2026 Todd Rosenkrantz.<br>Based on VisUAL2, copyright &copy; 2018&ndash;2019 " +
    "Dr T. Clarke, Imperial College London, and the VisUAL2 contributors.</p>" +
    "<p>VisUAL3 is free software: you can redistribute it and/or modify it under the terms of " +
    "the GNU General Public License version 3.</p>" +
    "<p>It is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even " +
    "the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.</p>"

/// copy of a vex dialog button with a new caption
[<Emit("Object.assign({}, $0, {text: $1})")>]
let private relabel (button : obj) (text : string) : obj = jsNative

/// Help > License: the notice, with a button that opens the full licence text
let showLicense () =
    vex?dialog?confirm (createObj [
        "unsafeMessage" ==> licenseNoticeHtml ()
        "buttons" ==> [| relabel vex?dialog?buttons?YES "View full licence"; relabel vex?dialog?buttons?NO "Close" |]
        // LICENSE ships at the top of the app, beside the app folder
        "callback" ==> fun (viewText : bool) -> if viewText then runLocalPage (path.join ("..", "LICENSE")) "" () ])
    |> ignore

let helpMenu() =
        makeMenu "Help" (
            [
                makeItem "Assembly language guide" (Some "F1") (runLocalPage "guide.html" "")
                makeItem "Calling functions from C" Core.Option.None (runLocalPage "guide.html" "c-calls")
                menuSeparator
                makeItem "GNU as reference: ARM features" Core.Option.None (runExtPage "https://sourceware.org/binutils/docs/as/ARM_002dDependent.html")
                makeItem "Arm Architecture Reference Manual (ARMv7-A)" Core.Option.None (runExtPage "https://developer.arm.com/documentation/ddi0406/latest/")
                makeItem "Testbench file format (original VisUAL2 site)" Core.Option.None (runExtPage <| visualDocsPage "testbench")
                menuSeparator
                makeItem "Load demo program" Core.Option.None (interlockAction "load code" loadDemo)
                makeCondItem (debugLevel > 0) "Run dev tools FABLE checks" Core.Option.None (interlockAction "FABLE checks" Integration.runTestbench)
                makeCondItem (debugLevel > 0) "Run Emulator Tests" Core.Option.None (interlockAction "run tests" Tests.runAllEmulatorTests)
                menuSeparator
                makeItem "License (GNU GPL v3)" Core.Option.None showLicense
                makeItem "About" Core.option.None (fun () ->
                    printfn "Directory is:%s" (Stats.dirOfSettings())
                    showVexAlert (sprintf "<h4>VisUAL3 ARMv7 emulator v%s</h4>" Refs.appVersion +
                                "GNU as syntax, VFPv4 floating point, C function calls (AAPCS).<br><br>" +
                                "Copyright (c) 2026 Todd Rosenkrantz. Based on VisUAL2, copyright (c) 2018-2019 " +
                                "Dr T. Clarke, Imperial College London, and the 2018 HLP students who wrote and tested it, " +
                                "including Angelos Filos, Lorenzo Silvestri, Thomas Carotti, Douglas Brion and Christopher Macca. " +
                                "VisUAL2 was inspired by Salman Arif's VisUAL.<br><br>" +
                                "VisUAL3 is free software under the GNU General Public License version 3, " +
                                "with no warranty. See LICENSE and CREDITS.md in the source repository."))
            ])


/// Make all app menus
let mainMenu() =
    let template = [| fileMenu(); editMenu(); viewMenu(); helpMenu(); testMenu() |]
    remote?Menu?setApplicationMenu (remote?Menu?buildFromTemplate (template)) |> ignore
