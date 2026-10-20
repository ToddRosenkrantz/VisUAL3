# VisUAL3: ARMv7 assembly emulator

VisUAL3 is a desktop emulator for learning ARM assembly. You write ARMv7 (A32) code in GNU assembler syntax, the syntax gcc, clang and Linux use, then run or step through it and watch registers, flags, memory and the stack change. Errors are explained where they occur, with help for each instruction.

Its **C Call** panel calls your assembly functions the way compiled C would. You write the C prototype in a comment, enter the arguments, and the panel passes them as the ARM calling convention (AAPCS) requires. It then shows the return value, any arrays before and after the call, and calling-convention mistakes such as not preserving r4–r11.

VisUAL3 is a fork of [VisUAL2](https://github.com/ImperialCollegeLondon/Visual2) from Imperial College London, which reimplemented Salman Arif's [VisUAL](https://salmanarif.bitbucket.io/visual/). The fork replaces VisUAL2's own assembler syntax with GNU syntax and moves it to current tools. VisUAL2 was created by Dr T. Clarke and the 2018 HLP students at Imperial College London. VisUAL3 is free software under the [GNU GPL version 3](../LICENSE); see the [credits](../CREDITS.md).

## Using it

Download the zip for Windows, Linux or macOS from the [Releases page](https://github.com/ToddRosenkrantz/VisUAL3/releases) (the current one is [v3.0.1-dev](https://github.com/ToddRosenkrantz/VisUAL3/releases/tag/v3.0.1-dev)), unzip it and run the program inside. The [README](../README.md) explains how, and how to get past the warnings for unsigned apps.

The [assembly language guide](guide.md) covers the syntax, instructions and directives, and calling functions from C. It is also built into the app: **Help → Assembly language guide** (F1).

The emulated core has no floating-point unit and runs ARM (A32) code only. There are no system calls: a program ends by returning with `bx lr` or by running off the end of its code.

## Building it

The [README](../README.md) describes building from source (.NET 10 SDK and Node.js 22), running the tests and packaging for each platform.
