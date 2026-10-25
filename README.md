# VisUAL3: ARMv7 assembly emulator

A desktop emulator for learning ARM assembly. You write ARMv7-A (A32) code for a Cortex-A7-class core with a VFPv4 floating-point unit in **GNU assembler syntax**, step through it, and watch registers, flags, memory and the stack. A **C Call** panel calls your assembly functions the way compiled C would, so you can test a function against the C prototype it implements.

VisUAL3 is a modernised fork of [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2). VisUAL2 used its own armasm-like syntax and a 2019 toolchain; this version uses GNU syntax and current tools (Electron 43, Fable 4, .NET 10).

## Running it

Download the zip for your platform from the [Releases page](https://github.com/ToddRosenkrantz/VisUAL3/releases) (the current one is [v3.0.0-dev.8](https://github.com/ToddRosenkrantz/VisUAL3/releases/tag/v3.0.0-dev.8)), or [build it](#packaging). Unzip it anywhere and run:

| Platform | Run |
|---|---|
| Windows | `VisUAL3-win32-x64\VisUAL3.exe` |
| Linux | `VisUAL3-linux-x64/VisUAL3` |
| macOS | `VisUAL3-darwin-arm64/VisUAL3.app` (Apple silicon) or `VisUAL3-darwin-x64/VisUAL3.app` (Intel) |

[CHANGELOG.md](CHANGELOG.md) lists every significant change since VisUAL2.

Instructors: [docs/writing-cases.md](docs/writing-cases.md) explains how to write C Call cases for exercises and check them with `npm run check-cases`.

The builds are not code-signed. On Windows choose **More info → Run anyway** at the SmartScreen warning; on macOS right-click the app and choose **Open** the first time.

## Writing code

```asm
@ C: extern int sum_array(const int *a, int n);
        .text
        .global sum_array
sum_array:                      @ r0 = a, r1 = n
        mov     r2, #0
1:      ldr     r3, [r0], #4    @ load a word, step to the next
        add     r2, r2, r3
        subs    r1, r1, #1
        bne     1b              @ back to the local label 1:
        mov     r0, r2
        bx      lr

        .data
table:  .word   10, 20, 30, -5
```

- **Syntax:** GNU `as`, unified ARM syntax. Labels end with `:`, comments are `@`, `//` or `/* */`, and `;` separates statements. Directives include `.text`/`.data`/`.bss`, `.word`/`.hword`/`.byte`, `.ascii`/`.asciz`, `.space`, `.align` and `.equ`. gcc boilerplate such as `.type`, `.size`, `.syntax unified` and `.cfi_*` is accepted.
- **Instructions:** data processing, shifts, `movw`/`movt`, `neg`, `nop`, `mul`/`mla`/`mls`, `sdiv`/`udiv`, the long multiplies `umull`/`smull`/`umlal`/`smlal`, `uxtb`/`uxth`/`sxtb`/`sxth`, the bit-field instructions `ubfx`/`sbfx`/`bfi`/`bfc`, `clz`, `rbit`, `rev`/`rev16`/`revsh`, `ldr`/`str` with `b`, `h` and `d` forms, `ldrsb`/`ldrsh`, `ldm`/`stm`, `push`/`pop`, `b`/`bl`/`bx`, `adr` and `mrs`/`msr`, all with condition codes.
- **Floating point:** VFPv4 with s0–s31 and d0–d31: loads and stores, `vmov`, arithmetic, multiply-accumulate and fused multiply-add, `vsqrt`, compares with `vmrs`, and conversions, with exact IEEE rounding and FPSCR flags. `.float` and `.double` store values. The **VFP** tab shows the registers.
- **Running:** execution starts at the first instruction in `.text`. It ends when execution runs past the last instruction, or when the code returns with `bx lr`. SP starts at `0xFF000000`, and LR at a return address that stops the run.
- **Not supported:** NEON vector instructions, Thumb, `svc`/system calls, `blx`, exclusive loads and stores (`ldrex`/`strex`), `mrs`/`msr`, and macros.

**Help → Assembly language guide** (F1) in the app is the full reference. Pointing at a line marked as an error shows help for that instruction or directive.

### Calling functions from C

Put a C prototype in a comment starting with `@ C:` and open the **C Call** tab. Enter arguments; arrays and strings are built in memory for you. Then press **Call** to run, or **Step into** to step through. The panel shows the return value, arrays before and after the call, and calling-convention (AAPCS) problems. `float` and `double` use the hard-float convention, as on armv7hf Linux: they are passed in s0–s15/d0–d7 and returned in s0/d0. It flags r4–r11, s16–s31 or SP not preserved, results not extended to 32 bits, and writes through `const` pointers. Arguments can be kept as named cases per function; they are saved with the source, as `prog.ccall` beside `prog.s`, and loaded when it is opened.

### Testbenches

A tab whose first line is `##TESTBENCH` holds automated tests. Run them with **Test → Run all tests**:

```
##TESTBENCH
#TEST 1 sums three words
IN R0 PTR 10, 20, 30
IN R1 IS 3
OUT R0 IS 60
STACKPROTECT
PERSISTENTREGS R4, R5, R6, R7, R8, R9, R10, R11
BRANCHTOSUB sum_array
```

Results are written back into the tab: `>>;` lines for tests that passed, and `>>-` lines with actual and expected values for tests that failed.

## Building from source

Requirements:

- [.NET 10 SDK](https://dotnet.microsoft.com/download/dotnet/10.0) (pinned to 10.0 by `global.json`)
- [Node.js](https://nodejs.org/) 22 LTS or later (npm comes with it)

```sh
dotnet tool restore     # installs the Fable compiler (pinned in .config/dotnet-tools.json)
npm install             # Electron, esbuild, Monaco and the other JavaScript dependencies
npm run build           # F# → JavaScript with Fable, bundled with esbuild
npx electron .          # run the app (the first run downloads the Electron binary)
```

`npm start` builds and runs in one step.

Development options:

- `npx electron . -d` opens the developer tools in their own window and copies the app's console output to the terminal. Use `-d`, not `--debug`: Electron treats `--debug` as a Node option.
- Under WSL or in some Linux containers, add `--no-sandbox` if Electron reports that its sandbox helper is not configured.

### Tests

```sh
./run-tests.sh          # or: npm test
```

This runs the emulator tests twice: on .NET, and as Fable-compiled JavaScript under Node. The app runs the JavaScript, and some bugs only appear there. Examples are 32-bit arithmetic precision, regular expressions, and 64-bit integers, which are BigInts in JavaScript. Run both after changing the emulator.

### Packaging

| Command | Output in `dist/` |
|---|---|
| `npm run pack-linux` | `VisUAL3-linux-x64-<version>.zip` |
| `npm run pack-win` | `VisUAL3-win32-x64-<version>.zip` |
| `npm run pack-mac` | `VisUAL3-darwin-x64-<version>.zip`, `VisUAL3-darwin-arm64-<version>.zip` |
| `npm run pack-all` | all of the above |

Each command builds the app, packages it with `@electron/packager`, and zips the result with `zip-dist.mjs`. All platforms can be packaged from Linux: no Wine is needed for the Windows icon. The zips keep the macOS bundle's symbolic links and the executables' permissions. On a Windows build machine, zipping falls back to PowerShell.

## Project structure

| Path | Contents |
|---|---|
| `src/Emulator` | The assembler and emulator, pure F#. `GnuLex.fs` reads GNU syntax; `ParseTop.fs`, `DP.fs`, `Memory.fs`, `Branch.fs` and `Misc.fs` parse instructions and directives; `ExecutionTop.fs` loads and runs programs; `CCall.fs` does C calls and AAPCS checks; `Testlib.fs` runs testbenches. |
| `src/Renderer` | The user interface, F# compiled with Fable. `Interop.fs` holds the bindings for Node, Electron and `@electron/remote`; `Integration.fs` runs programs; `CCallView.fs` is the C Call panel; `ErrorDocs.fs` is the error-hover help. |
| `src/Main` | The Electron main process. |
| `app/` | `index.html`, CSS, fonts, `guide.html`, the demo program in `samples/`, and `js/monaco-init.js` (Monaco setup and GNU syntax highlighting). |
| `tests/EmulatorTests` | Emulator tests, run on .NET and Node. |
| `build.mjs` | esbuild bundling into `out/main.js` and `app/js/renderer.js`; copies Monaco, tippy and popper into `app/js`. |
| `zip-dist.mjs` | Zips packaged apps. |

Generated, not committed: `build/` (Fable output), `out/`, `app/js/renderer.js`, `app/js/vs/`, `dist/`.

The renderer runs with Node integration on (`nodeIntegration: true`, `contextIsolation: false`) because it reads and writes files directly. It only loads the app's own local pages; links to web sites open in the system browser.

## Credits

VisUAL3 is built on [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2), created by Dr T. Clarke and the 2018 third-year HLP students at Imperial College London, who wrote and tested it. VisUAL2 was inspired by Salman Arif's [VisUAL](https://salmanarif.bitbucket.io/visual/). [CREDITS.md](CREDITS.md) names the contributors and lists the third-party components and their licences.

## Licence

VisUAL3 is free software, released under the [GNU General Public License version 3](LICENSE) (GPL-3.0-only), with the permission of Dr T. Clarke for the VisUAL2 code it is built on. You may use, study, share and modify it under the terms of that licence. Anyone who distributes a modified version must release its source under the same licence. It comes with no warranty.

- Copyright © 2018–2019 Dr T. Clarke, Imperial College London, and the VisUAL2 contributors
- Copyright © 2026 Todd Rosenkrantz (VisUAL3)
