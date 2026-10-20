# VisUAL2: ARMv7 assembly emulator

A desktop emulator for learning ARM assembly. You write ARMv7 (A32) code in **GNU assembler syntax**, step through it, and watch registers, flags, memory and the stack. A **C Call** panel calls your assembly functions the way compiled C would, so you can test a function against the C prototype it implements.

This is a modernised fork of [ImperialCollegeLondon/Visual2](https://github.com/ImperialCollegeLondon/Visual2). The original used its own armasm-like syntax and a 2019 toolchain; this version uses GNU syntax and current tools (Electron 43, Fable 4, .NET 8).

## Running it

Download the zip for your platform from a release (or [build it](#packaging)), unzip it anywhere and run:

| Platform | Run |
|---|---|
| Windows | `VisUAL2-win32-x64\VisUAL2.exe` |
| Linux | `VisUAL2-linux-x64/VisUAL2` |
| macOS | `VisUAL2-darwin-arm64/VisUAL2.app` (Apple silicon) or `VisUAL2-darwin-x64/VisUAL2.app` (Intel) |

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
- **Instructions:** data processing, shifts, `mul`/`mla`, `ldr`/`str`(`b`), `ldm`/`stm`, `push`/`pop`, `b`/`bl`/`bx` and `adr`, all with condition codes.
- **Running:** execution starts at the first instruction in `.text`. It ends when execution runs past the last instruction, or when the code returns with `bx lr`. SP starts at `0xFF000000`, and LR at a return address that stops the run.
- **Not supported:** floating point (the core has no FPU), Thumb, `svc`/system calls, `blx`, halfword loads and stores, and macros.

**Help → Assembly language guide** (F1) in the app is the full reference. Pointing at a line marked as an error shows help for that instruction or directive.

### Calling functions from C

Put a C prototype in a comment starting with `@ C:` and open the **C Call** tab. Enter arguments; arrays and strings are built in memory for you. Then press **Call** to run, or **Step into** to step through. The panel shows the return value, arrays before and after the call, and calling-convention (AAPCS) problems. It flags r4–r11 or SP not preserved, results not extended to 32 bits, and writes through `const` pointers.

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

- [.NET 8 SDK](https://dotnet.microsoft.com/download/dotnet/8.0)
- [Node.js](https://nodejs.org/) 20 or later (npm comes with it)

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
| `npm run pack-linux` | `VisUAL2-linux-x64-<version>.zip` |
| `npm run pack-win` | `VisUAL2-win32-x64-<version>.zip` |
| `npm run pack-mac` | `VisUAL2-darwin-x64-<version>.zip`, `VisUAL2-darwin-arm64-<version>.zip` |
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

Based on [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2) (Imperial College London), itself a reimplementation of [VisUAL](https://salmanarif.bitbucket.io/visual/) by Salman Arif. See the original project's [acknowledgements](https://github.com/ImperialCollegeLondon/Visual2/wiki/Acknowledgements).

## Licence

The repository has no licence file. The original project's README said it had no licence while it was closed source at Imperial College, and planned GPLv3 on publication. Check the original project's terms before redistributing.
