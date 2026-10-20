# Changelog

Significant changes in VisUAL3 since [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2) 1.06.10 (commit `6a36478`, December 2019), the version it was forked from. Newest first. Builds are on the [Releases page](https://github.com/ToddRosenkrantz/VisUAL3/releases). Older development releases are removed when a new one is published; their entries stay here.

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
