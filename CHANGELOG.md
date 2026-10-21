# Changelog

Significant changes in VisUAL3 since [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2) 1.06.10 (commit `6a36478`, December 2019), the version it was forked from. Newest first. Builds are on the [Releases page](https://github.com/ToddRosenkrantz/VisUAL3/releases). Older development releases are removed when a new one is published; their entries stay here.

## Unreleased

### Fixed

- C Call: a struct member whose array size isn't a plain number, such as `char state[2+1]`, now gets an error naming that member and its size (`state[2+1]: an array member's size must be a number, e.g. state[3]`), instead of `name[2]`. C variables declared in `@ C:` comments, such as `int counts[2+1];`, name the variable in the same way.

## 3.0.0-dev.5 (2026-10-21)

### Added

- C Call: a size after an array or quoted string gives a buffer with room to spare, e.g. `"Hello, " [32]` or `{1, 2, 3} [10]`, the rest zeroed. This makes functions such as `strcat` testable.
- C Call: pointers to arrays of structs. Structs are declared in `@ C:` comments (`struct`, `typedef struct`, with `__attribute__((packed))` before or after the braces) and laid out as gcc does on ARM; the panel shows each layout. Arrays are entered as C initializers, shown as a table per member, and checked member by member.
- C Call: C variables declared in `@ C:` comments (`struct student s[] = {...};`, `int counts[4];`, `char msg[] = "hi";`) are placed in memory for each call, shown and checkable before and after. Pointer arguments can be given as addresses into them: `s`, `&s[1]`, `&s[1].name`, `&counts[2]`. Returned `char *` results are shown as text.
- C Call: an expected pointer result can name a place in an argument array, such as `&s[1]` or `&s[1].score`, and pointer checks report results that way.
- C Call: char buffers are also shown as text before and after the call, and string expectations report text instead of byte values.

## 3.0.0-dev.4 (2026-10-20)

### Added

- C Call: `long long`, `int64_t` and `uint64_t` parameters, results and arrays, in even/odd register pairs (r0:r1, r2:r3) or 8-byte stack slots, as AAPCS requires.
- Pointer and Stack tooltips for `vldr`, `vstr`, `vldm`, `vstm`, `vpush` and `vpop`, showing the address and each register's value as a float and in hex.
- Memory tab: a **Floats** button adds a column showing each word as a float, or each 8-byte-aligned pair as a double.
- **Help → Load floating-point example**: five VFP functions with saved C Call cases and expected results (`app/samples/fp_examples.s` and `.ccall`).
- C Call: expected results in a case. The expected result and the expected contents of output buffers are checked after each call, with ✔ or ✘ and the values compared; float and double match within rounding.
- C Call: named cases of arguments for each function, chosen in a new **Case** row. They are saved with the source, as `prog.ccall` beside `prog.s`, when the `.s` is saved, and loaded when it is opened.

## 3.0.0-dev.3 (2026-10-20)

From this release on, development builds are numbered 3.0.0-dev.N, so the final release, 3.0.0, sorts after them.

### Added

- `ldrd` and `strd`: two words into or from an even/odd register pair (r0/r1 … r10/r11), with the second register optional. They take the same addresses as `ldrh` and always need a word-aligned address.
- Bit-field instructions `ubfx`, `sbfx`, `bfi` and `bfc`.
- `clz` (count leading zeros), `rbit` (reverse bits), and `rev`, `rev16` and `revsh` (reverse bytes).
- **Floating point: a VFPv4 unit with 32 double registers** (d0–d31, with s0–s31 overlapping d0–d15) and the FPSCR:
  - `vldr`/`vstr`, `vldm`/`vstm` (`ia`, `db`), `vpush`/`vpop`, and `vmov` in all its forms, including VFP constants
  - `vadd`, `vsub`, `vmul`, `vdiv`, `vnmul`, `vabs`, `vneg`, `vsqrt`; `vmla`, `vmls`, `vnmla`, `vnmls`; fused `vfma`, `vfms`, `vfnma`, `vfnms`
  - `vcmp`/`vcmpe` with `vmrs APSR_nzcv, fpscr` (or `fmstat`), `vmrs`/`vmsr` for the FPSCR
  - `vcvt`/`vcvtr` between float, double, integers and fixed point; `vcvtb`/`vcvtt` for half precision
  - `.float`/`.single` and `.double` data directives
  - exact IEEE 754 results on both .NET and in the app, with the FPSCR rounding modes, FZ, DN and cumulative exception flags, and ARM's default NaN
- A **VFP** tab showing s0–s31 or d0–d31 as decimal values and hex, and the FPSCR, with changed registers highlighted.
- `mrs` and `msr` for the core flags.
- C Call: `float` and `double` parameters, results and arrays, passed with the hard-float convention (armv7hf): s0–s15/d0–d7 with back-filling, then the stack; results in s0/d0. It checks that s16–s31 and the FPSCR rounding mode are preserved.

### Fixed

- The guide's "Not supported" list no longer names instructions that 3.0.2-dev added.
- The guide keeps Operand 2 and address examples, and inline code, on one line, with more room for the text.
- The zips' top-level `LICENSE` is VisUAL3's GPL-3.0 licence, with `CREDITS.md` beside it. It was Electron's MIT licence, which is now `LICENSE.electron.txt`.
- The Help menu item is now **License (GNU GPL v3)**, and Help → About no longer says the core has no FPU.

## 3.0.2-dev (2026-10-20)

The emulated core is now a Cortex-A7-class ARMv7-A processor.

### Added

- `movw` and `movt`, including `#:lower16:symbol` and `#:upper16:symbol` as gcc emits them. Like GNU `as`, `mov Rd, #imm` becomes `movw` when the value fits in 16 bits but is not an ARM immediate.
- `sdiv` and `udiv`. They round toward zero; as on the Cortex-A7, dividing by zero gives 0, and `sdiv` of `0x80000000` by −1 gives `0x80000000`.
- `mls`, and the 64-bit multiplies `umull`, `smull`, `umlal` and `smlal`. With `s`, N and Z come from the 64-bit result.
- `uxtb`, `uxth`, `sxtb` and `sxth`, with an optional `ror #8`, `#16` or `#24`.
- `neg` (the same as `rsb Rd, Rm, #0`) and `nop`.

### Changed

- Building from source uses .NET 10 (pinned by `global.json`), Fable 4.29 and Node.js 22 LTS.

## 3.0.1-dev (2026-10-20)

### Added

- Halfword and signed loads and stores: `ldrh`, `ldrsh`, `ldrsb` and `strh`.
- **Edit → Preferences → Strict Alignment**, off by default. When it is on, unaligned word and halfword accesses stop the run, as on a core that traps alignment faults.

### Changed

- Unaligned `ldr`, `str`, `ldrh`, `ldrsh` and `strh` work as on ARMv7 Linux: the bytes are read or written in little-endian order, with one extra cycle. VisUAL2 stopped with an error. `ldm`, `stm`, `push` and `pop` still need a word-aligned address.
- Load and store addressing matches ARMv7: immediate offsets up to ±4095 (±255 for halfword and signed forms), negative register offsets such as `[r1, -r2]`, and `lsl`, `lsr`, `asr`, `ror` and `rrx` on register offsets.

### Fixed

- The Preferences checkboxes show the saved settings instead of fixed defaults.

## 3.0.0-dev (2026-10-19)

The first VisUAL3 release, renamed from VisUAL2.

### Assembler and emulator

- **GNU assembler syntax** (ARM, unified) replaces VisUAL's own armasm-like syntax, which is no longer accepted:
  - labels end with `:`, including single-letter labels and local labels such as `1:` with `1b`/`1f`
  - comments are `@`, `//` or `/* */`, and `;` separates statements
  - directives include `.text`, `.data`, `.bss`, `.word`, `.hword`, `.byte`, `.ascii`, `.asciz`, `.space`, `.align` and `.equ`, and gcc boilerplate such as `.type`, `.size` and `.cfi_*` is accepted
  - a mnemonic such as `b` is never taken as a label
- New instructions: `mul`, `mla`, `bx`, `push` and `pop`. `ldm`/`stm` work without a suffix. ARMv7 shift forms are accepted, such as `lsl r0, r1, #2`, `lsl r0, #2` and `asl`.
- `b`/`bl` targets are checked against the A32 rules.
- `push` pre-decrements: the stack is full-descending.
- Execution ends when the code returns from the top level with `bx lr`: LR starts at a return address that stops the run. SP starts at `0xFF000000`.
- The core has no floating-point unit. VFP instructions give a clear error.
- **C Call panel:** write a C prototype in an `@ C:` comment, then call the function with typed arguments, including arrays, strings and output buffers. The panel shows the return value, arrays before and after the call, and calling-convention (AAPCS) mistakes. These are r4–r11 or SP not preserved, results not extended to 32 bits, and writes through `const` pointers.
- Testbenches keep the case of labels, so GNU labels work.

### User interface

- Ported to Electron 43, Monaco 0.57, tippy.js 6 and esbuild, from Electron 2.0, Monaco 0.12, tippy.js 2 and webpack 3.
- Hover help, the Help menu, the built-in assembly language guide (F1) and the demo program cover GNU syntax.
- **Help → License** shows the licence. **Help → About** gives the credits.
- Fixed: the layout at small window sizes, the register display at startup and after reset, tooltips, and the editor cursor drifting away from the text.
- A PNG window icon on Linux.

### Build and packaging

- The emulator and UI are compiled with Fable 4 on .NET 8, replacing Fable 2 (netstandard2.0) and Paket. Emulator tests run on both .NET and Node.
- `npm run pack-linux`, `pack-win`, `pack-mac` and `pack-all` package the app and zip it. All platforms can be packaged from Linux.
- The app's settings are kept separately from VisUAL2's.

### Licence and documentation

- Released under the GNU General Public License version 3 (GPL-3.0-only), with the permission of Dr T. Clarke for the VisUAL2 code. `CREDITS.md` names the VisUAL2 authors and contributors.
- New README. `docs/guide.md` points to the built-in guide, and `docs/index.md` describes this fork.
