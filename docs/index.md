# VisUAL2: ARMv7 assembly emulator

VisUAL2 is a desktop emulator for learning ARM assembly. You write ARMv7 (A32) code in GNU assembler syntax, the syntax gcc, clang and Linux use, then run or step through it and watch registers, flags, memory and the stack change. Errors are explained where they occur, with help for each instruction.

Its **C Call** panel calls your assembly functions the way compiled C would. You write the C prototype in a comment, enter the arguments, and the panel passes them as the ARM calling convention (AAPCS) requires. It then shows the return value, any arrays before and after the call, and calling-convention mistakes such as not preserving r4–r11.

This version is a fork of [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2) from Imperial College London, which reimplemented Salman Arif's [VisUAL](https://salmanarif.bitbucket.io/visual/). The fork replaces VisUAL2's own assembler syntax with GNU syntax and moves it to current tools.

## Using it

Download the zip for Windows, Linux or macOS, unzip it and run the program inside. The [README](../README.md) explains how, and how to get past the warnings for unsigned apps.

The [assembly language guide](guide.md) covers the syntax, instructions and directives, and calling functions from C. It is also built into the app: **Help → Assembly language guide** (F1).

The emulated core has no floating-point unit and runs ARM (A32) code only. There are no system calls: a program ends by returning with `bx lr` or by running off the end of its code.

## Building it

The [README](../README.md) describes building from source (.NET 8 SDK and Node.js), running the tests and packaging for each platform.
