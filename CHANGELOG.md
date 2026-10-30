# Changes from VisUAL2 to VisUAL3

VisUAL3 is a fork of [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2) 1.06.10 (commit `6a36478`, December 2019). This document lists the significant ways VisUAL3 differs from it. It brings together everything from the development builds 3.0.0-dev to 3.0.0-dev.9.0.2 (October 2026), and will be the change list for the 3.0.0 release. Builds are on the [Releases page](https://github.com/ToddRosenkrantz/VisUAL3/releases).

In short:

- Programs are written in **GNU assembler syntax** (as used by gcc, clang and Linux) instead of VisUAL's own syntax.
- The emulated core is a **Cortex-A7-class ARMv7-A** processor, with many more instructions, unaligned loads and stores, and a **VFPv4 floating-point unit**.
- A new **C Call** panel calls assembly functions as compiled C would, from a C prototype in a comment, and checks the results and the calling convention.
- The app runs on current Electron, Monaco and .NET, and is released under the **GNU GPL version 3**.

## Assembler syntax

- **GNU assembler syntax** (ARM, unified) replaces VisUAL's armasm-like syntax, which is no longer accepted:
  - labels end with `:`, including single-letter labels and local labels such as `1:` with `1b`/`1f`
  - comments are `@`, `//` or `/* */`, and `;` separates statements
  - directives include `.text`, `.data`, `.bss`, `.word`, `.hword`, `.byte`, `.ascii`, `.asciz`, `.space`, `.align`, `.equ`, `.float`/`.single` and `.double`, and gcc boilerplate such as `.global`, `.type`, `.size` and `.cfi_*` is accepted
  - a mnemonic such as `b` is never taken as a label
- ARMv7 shift forms such as `lsl r0, r1, #2`, `lsl r0, #2` and `asl`; `ldm`/`stm` without a suffix.
- `#:lower16:symbol` and `#:upper16:symbol` as gcc emits them. Like GNU `as`, `mov Rd, #imm` becomes `movw` when the value fits in 16 bits but is not an ARM immediate.
- `b`/`bl` targets are checked against the A32 rules.

## The emulated processor

The core is a Cortex-A7-class ARMv7-A processor running ARM (A32) code.

### New instructions

- Multiply and divide: `mul`, `mla`, `mls`; 64-bit `umull`, `smull`, `umlal`, `smlal` (with `s`, N and Z come from the 64-bit result); `sdiv` and `udiv`, which round toward zero and, as on the Cortex-A7, give 0 when dividing by zero.
- Moves and data processing: `movw`, `movt`, `neg` and `nop`.
- Extend: `uxtb`, `uxth`, `sxtb`, `sxth`, with an optional `ror #8`, `#16` or `#24`.
- Bits and bytes: `ubfx`, `sbfx`, `bfi`, `bfc`, `clz`, `rbit`, `rev`, `rev16` and `revsh`.
- Loads and stores: `ldrh`, `ldrsh`, `ldrsb`, `strh`, and `ldrd`/`strd` (an even/odd register pair, second register optional, word-aligned address).
- Stack and branches: `push`, `pop` and `bx`.
- Status: `mrs` and `msr` for the core flags.

### Loads and stores

- Addressing matches ARMv7: immediate offsets up to ±4095 (±255 for halfword, signed and doubleword forms), negative register offsets such as `[r1, -r2]`, and `lsl`, `lsr`, `asr`, `ror` and `rrx` on register offsets.
- Unaligned `ldr`, `str`, `ldrh`, `ldrsh` and `strh` work as on ARMv7 Linux, in little-endian byte order, with one extra cycle. VisUAL2 stopped with an error. `ldm`, `stm`, `push`, `pop`, `ldrd` and `strd` still need a word-aligned address.
- **Edit → Preferences → Strict Alignment** (off by default) makes unaligned accesses stop the run, as on a core that traps alignment faults.
- `push` pre-decrements: the stack is full-descending.

### Floating point (VFPv4)

- 32 double registers (d0–d31, with s0–s31 overlapping d0–d15) and the FPSCR.
- `vldr`/`vstr`, `vldm`/`vstm` (`ia`, `db`), `vpush`/`vpop`, and `vmov` in all its forms, including VFP constants.
- `vadd`, `vsub`, `vmul`, `vdiv`, `vnmul`, `vabs`, `vneg`, `vsqrt`; `vmla`, `vmls`, `vnmla`, `vnmls`; fused `vfma`, `vfms`, `vfnma`, `vfnms`.
- `vcmp`/`vcmpe` with `vmrs APSR_nzcv, fpscr` (or `fmstat`), and `vmrs`/`vmsr` for the FPSCR.
- `vcvt`/`vcvtr` between float, double, integers and fixed point; `vcvtb`/`vcvtt` for half precision.
- Results are exact IEEE 754, identical on .NET and in the app, with the FPSCR rounding modes, the flush-to-zero (FZ) and default-NaN (DN) modes, the cumulative exception flags, and ARM's default NaN.

### Running programs

- A program ends when it returns from the top level with `bx lr`: LR starts at a return address that stops the run. It can also run off the end of its code.
- SP starts at `0xFF000000`.
- The cycle count follows a simple three-stage pipeline model that students can work out by hand. Every instruction takes 1 cycle except branches and memory operations. `b`, `bl` and `bx` take 2, or 3 when a conditional branch is taken (1 when it is not). Loads and stores take 2 for each word moved, plus 1 if unaligned. The step that ends a program is no longer counted as a cycle. The built-in guide has the table.
- Testbenches keep the case of labels, so GNU labels work.

## C Call panel

New in VisUAL3. Write a C prototype in an `@ C:` comment, such as `@ C: extern int sum_array(const int *a, int n);`, then open the **C Call** tab, enter the arguments and press **Call** (or **Step into** to stop at the first instruction).

### Types and the calling convention

- Parameters and results: the integer types (`char` … `long long`, `int8_t` … `uint64_t`, `size_t`), `_Bool`, `float`, `double`, and pointers to these, to `void` or to structs, with `const`.
- Arguments are passed as the AAPCS requires, with the hard-float convention of armv7hf Linux: integers and pointers in r0–r3, `long long` in an even/odd register pair, `float` and `double` in s0–s15/d0–d7 with back-filling, the rest on the stack. The panel shows where each argument goes.
- After the call it reports calling-convention mistakes: r4–r11, SP or s16–s31 not preserved, the FPSCR rounding mode changed, `char`/`short`/`_Bool` results not extended to 32 bits, and writes through `const` pointers.

### Arguments

- Numbers in decimal, hex or binary, or characters such as `'A'`, checked against the type's range.
- Pointers can be given as an **array** (`{10, 20, 30}`), a **string** for `char *` (`hello\n`, with a 0 byte added), an **output buffer** of n zeroed elements, or an **address** (a label, a number or `NULL`).
- A size after an array or a quoted string gives a buffer with room to spare, counted in elements: `{1, 2, 3} [10]`, or `"Hello, " [32]` for a function such as `strcat`. The rest is zeroed.
- **Structs:** declared in `@ C:` comments (`struct`, `typedef struct`, `__attribute__((packed))`), laid out as gcc does on ARM, with the layout shown in the panel. Arrays of them are entered as C initializers, such as `{{"Ann", 90, 3.5}, {"Bob", 75, 2.8}}`.
- **C variables** declared in `@ C:` comments (`struct student s[] = {...};`, `int counts[4];`, `char msg[] = "hi";`) are placed in memory, freshly initialised for each call. Pointer arguments can be addresses into them, as C code would pass them: `s`, `&s[1]`, `&s[1].name`, `&counts[2]`.
- The input hints give an example for each parameter's type, and a **How to enter arrays, strings and buffer sizes** section under the parameters gives worked examples for the function's own types.

### Results and checks

- The panel shows the return value, and every array, string buffer, struct array and C variable before and after the call, with changes highlighted. Char buffers and `char *` results are also shown as text; struct arrays as a table with a column per member.
- **Expected results:** a case can give the expected return value and the expected contents of each changeable array or C variable. After each call every check shows ✔ or ✘ with the values compared. Only the elements and members given are checked; `float` and `double` match within rounding. A pointer result can be expected as a label, an address, `NULL` or a place such as `&s[1]`.
- A history of recent calls, with how many checks passed.

### Cases

- Each function can keep named sets of arguments and expectations, chosen in the **Case** row.
- They are saved with the source as `prog.ccall` beside `prog.s`, in readable JSON, and loaded when `prog.s` is opened.
- **Help → Load floating-point example** opens five VFP functions with ready-made cases and expected results.
- **For instructors:** [docs/writing-cases.md](docs/writing-cases.md) explains how to write cases for exercises, and `npm run check-cases -- <cases> [solutions]` runs every case in a folder against reference solutions, with the panel's checks, from the command line. `npm run grade-cases -- <submissions.zip> <cases>` grades a Canvas bulk download with the same cases: credit by the fraction of cases passed, a feedback file per student, and a gradebook file for Canvas's import.

## User interface

- A **VFP** tab shows s0–s31 or d0–d31 as decimal values and hex, and the FPSCR, with changed registers highlighted.
- Memory tab: a **Floats** button adds a column showing each word as a float, or each 8-byte-aligned pair as a double.
- Pointer and stack tooltips cover the VFP loads and stores, showing each register's value as a float and in hex.
- Hover help, the Help menu, the demo program and the built-in assembly language guide (F1) cover GNU syntax, the new instructions, floating point and the C Call panel.
- **Help → License (GNU GPL v3)** shows the licence; **Help → About** gives the credits.
- The first **Step** loads the program and pauses before its first instruction, with memory, symbols and registers shown; stepping back to the start returns there.
- **View → Show Addresses** shows each line's address (instruction, data or label) in place of its line number.
- When two open files have the same name, such as a solution and a starter, their tabs show as much of each folder as tells them apart (`HW4_2c.s — VisUAL3/HW4`, `HW4_2c.s — VisUAL3_solutions/HW4`). A tab's tooltip is its file's full path.
- Fixed: the Memory tab's **Reverse Direction** listed each word's bytes in increasing order in Byte View, so the addresses did not run in sequence.
- Fixed: the layout at small window sizes, the register display at startup and after reset, tooltips, the editor cursor drifting away from the text, and the Preferences checkboxes showing fixed defaults instead of the saved settings.
- A PNG window icon on Linux. The app's settings are kept separately from VisUAL2's.

## Build and packaging

- Ported to Electron 43, Monaco 0.57, tippy.js 6 and esbuild, from Electron 2.0, Monaco 0.12, tippy.js 2 and webpack 3.
- The emulator and UI are compiled with Fable 4.29 on .NET 10 (pinned by `global.json`), replacing Fable 2 (netstandard2.0) and Paket. Building needs Node.js 22 LTS. Emulator tests run on both .NET and Node.
- `npm run pack-linux`, `pack-win`, `pack-mac` and `pack-all` package the app and zip it; all platforms can be packaged from Linux. Each zip contains VisUAL3's `LICENSE` and `CREDITS.md`, with Electron's licence as `LICENSE.electron.txt`.

## Licence and documentation

- Released under the GNU General Public License version 3 (GPL-3.0-only), with the permission of Dr. Thomas Clarke for the VisUAL2 code. `CREDITS.md` names the VisUAL2 authors and contributors.
- A new README; `docs/index.md` describes this fork and `docs/guide.md` points to the built-in guide.

## Removed or not supported

- VisUAL's own assembler syntax.
- NEON (Advanced SIMD), Thumb code, `blx`, system calls (`svc`), exclusive and saturating instructions, macros and conditional assembly.
- In C calls: `long double`, structs passed or returned by value, variadic functions and function pointers.
