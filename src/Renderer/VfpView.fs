(*
    VisUAL3
    Module: Renderer.VfpView
    Description: Dashboard panel showing the VFP registers and FPSCR
*)

/// "VFP" dashboard panel: s0-s31 or d0-d31 as decimal values and hex bits, and the FPSCR.
/// Registers changed by the last step or run are highlighted.
module VfpView

open Browser
open Browser.Types
open Browser.Dom
open CommonData
open FpMath

/// true: show d0-d31; false: show s0-s31
let mutable private showDouble = false
let mutable private current = emptyFp
let mutable private previous = emptyFp

let private el (tag : string) (cls : string) (children : HTMLElement list) =
    let e = document.createElement tag
    if cls <> "" then e.className <- cls
    children |> List.iter (fun c -> e.appendChild c |> ignore)
    e

let private txt tag cls (s : string) =
    let e = el tag cls []
    e.textContent <- s
    e

let private word (fp : FpState) n = Map.tryFind n fp.W |> Option.defaultValue 0u

/// the FPSCR fields as text
let private fpscrText (f : uint32) =
    let bit n = f &&& (1u <<< n) <> 0u
    let flags = [ "N", 31; "Z", 30; "C", 29; "V", 28 ] |> List.map (fun (s, n) -> if bit n then s else "-") |> String.concat ""
    let exc = [ "IOC", 0; "DZC", 1; "OFC", 2; "UFC", 3; "IXC", 4; "IDC", 7 ] |> List.filter (snd >> bit) |> List.map fst
    let mode = [| "nearest"; "+inf"; "-inf"; "zero" |].[int ((f >>> 22) &&& 3u)]
    let ctl = [ "FZ", 24; "DN", 25 ] |> List.filter (snd >> bit) |> List.map fst
    flags, (if exc.IsEmpty then "none" else String.concat " " exc), "round to " + mode + (if ctl.IsEmpty then "" else ", " + String.concat ", " ctl)

let rec render () =
    let view = document.getElementById "view-vfp"
    if not (isNull view) then
        view.innerHTML <- ""
        let toggle =
            let b label isD =
                let e = txt "button" ("btn btn-default vfp-toggle" + (if isD = showDouble then " vfp-toggle-on" else "")) label
                e.addEventListener ("click", fun _ -> showDouble <- isD; render ())
                e
            el "div" "vfp-header" [ txt "span" "vfp-title" "VFP registers"; el "div" "btn-group" [ b "s (single)" false; b "d (double)" true ] ]
        let flags, exc, ctl = fpscrText current.FPSCR
        let fpscr =
            el "table" "vfp-fpscr" [
                el "tr" "" [ txt "td" "vfp-name" "FPSCR"; txt "td" "vfp-hex" (sprintf "0x%08X" current.FPSCR) ]
                el "tr" "" [ txt "td" "vfp-label" "NZCV"; txt "td" "vfp-value" flags ]
                el "tr" "" [ txt "td" "vfp-label" "exceptions"; txt "td" "vfp-value" exc ]
                el "tr" "" [ txt "td" "vfp-label" "mode"; txt "td" "vfp-value" ctl ] ]
        if current.FPSCR <> previous.FPSCR then fpscr.classList.add "vfp-changed"
        let row n =
            let words = if showDouble then [ 2 * n; 2 * n + 1 ] else [ n ]
            let changed = words |> List.exists (fun w -> word current w <> word previous w)
            let name, value, hex =
                if showDouble then
                    let lo, hi = word current (2 * n), word current (2 * n + 1)
                    sprintf "d%d" n, format F64 (rawOfWords lo hi), sprintf "0x%08X_%08X" hi lo
                else
                    let w = word current n
                    sprintf "s%d" n, format F32 (rawOfWord w), sprintf "0x%08X" w
            el "tr" (if changed then "vfp-changed" else "") [ txt "td" "vfp-name" name; txt "td" "vfp-value" value; txt "td" "vfp-hex" hex ]
        let regs = el "table" "vfp-regs" [ for n in 0 .. 31 -> row n ]
        let note =
            txt "div" "vfp-note"
                (if showDouble then "d0-d15 share storage with s0-s31: d<n> is s<2n> (low half) and s<2n+1> (high half)."
                 else "s<2n> and s<2n+1> are the low and high halves of d<n>.")
        [ toggle; fpscr; regs; note ] |> List.iter (fun e -> view.appendChild e |> ignore)

/// show a new VFP state; registers that differ from the last one shown are highlighted
let setState (fp : FpState) =
    if fp <> current then
        previous <- current
        current <- fp
    render ()

/// back to all zeros, nothing highlighted
let reset () =
    previous <- emptyFp
    current <- emptyFp
    render ()
