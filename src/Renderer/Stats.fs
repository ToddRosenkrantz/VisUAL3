(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Stats
    Description: Location of local app data. (The original online check of an Imperial College
                 server for new-version and exam-time notices has been removed.)
*)

module Stats

open Interop

/// Directory holding the settings file (and any local logs)
let dirOfSettings() = path.dirname (settingsFile())
