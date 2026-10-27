# Writing C Call cases: a guide for instructors

VisUAL3's **C Call** panel calls an assembly function the way compiled C would. A **case** is a saved set of arguments for one function, usually with the results the function should produce. When a student opens the assembly file, its cases load into the panel. Pressing **Call** runs the function and shows ✔ or ✘ for each expected result, plus any breach of the ARM calling convention.

This guide explains how to write cases for an exercise: the files involved, how to choose cases that catch real mistakes, the exact syntax, and how to check the cases against a reference solution before handing them out. The examples come from the CSE2312 Fall 2026 homework.

Contents:

1. [How cases are stored](#1-how-cases-are-stored)
2. [The workflow](#2-the-workflow)
3. [Writing the assembly file](#3-writing-the-assembly-file)
4. [Making cases in the panel](#4-making-cases-in-the-panel)
5. [The .ccall file format](#5-the-ccall-file-format)
6. [Argument and expected-value syntax](#6-argument-and-expected-value-syntax)
7. [Choosing good cases](#7-choosing-good-cases)
8. [What is checked automatically](#8-what-is-checked-automatically)
9. [Verifying cases with the checker](#9-verifying-cases-with-the-checker)
10. [Organising and distributing](#10-organising-and-distributing)
11. [Grading submissions from Canvas](#11-grading-submissions-from-canvas)
12. [Worked example: leftString](#12-worked-example-leftstring)
13. [Pitfalls and FAQ](#13-pitfalls-and-faq)

---

## 1. How cases are stored

Cases live in a JSON file beside the assembly file, with the same name and the extension `.ccall`:

```
HW4_2c.s       the assembly: the C prototype in an @ C: comment, and the function
HW4_2c.ccall   the cases for the functions in HW4_2c.s
```

- Opening `HW4_2c.s` in VisUAL3 loads `HW4_2c.ccall` if it is there.
- Saving `HW4_2c.s` writes the cases back to `HW4_2c.ccall`. A student's own cases and edits are kept the same way.
- Changing an argument or an expectation marks the file unsaved.
- A function can have any number of named cases. The panel's **Case** row picks one; **Call** runs only the case shown.

## 2. The workflow

1. **Write a reference solution** to the exercise in assembly. Keep it private: it never goes in the folder students get.
2. **Write the starter file** students receive: comments only, quoting the question, with the `@ C:` prototype (and any struct declarations). Students write `.global`, the label and the code themselves.
3. **Make the cases**, in the panel or by writing the `.ccall` directly (sections 4–6). Each case has a descriptive name, the arguments, and what the call should produce.
4. **Verify** that every case passes against the reference solution (section 9).
5. **Mutation-test:** copy the solution, plant a typical student mistake (a signed compare instead of unsigned, a missing `pop`, a missing string terminator), and check that some case fails. If none does, add a case that would catch it.
6. **Hand out** the starter `.s` and the `.ccall` only.

## 3. Writing the assembly file

### The prototype

Each callable function needs a C prototype in a comment that starts with `@ C:`. The starter is just comments:

```
@ CSE2312 Fall 2026, Homework 2, question 4a
@ uint32_t subU32(uint32_t x, uint32_t y) // returns x-y
@ C: extern uint32_t subU32(uint32_t x, uint32_t y);

@ Write the function below. Open the C Call tab to run the saved test cases,
@ which check the result and the calling convention.
```

The student writes the rest, as they would for a C program to link against:

```
        .global subU32
        .text
subU32:
        sub     r0, r0, r1
        bx      lr
```

- **Parameter names matter:** the `.ccall` file refers to arguments by these names (`x`, `y`).
- **Supported types:**
  - `char`, `signed`/`unsigned char`, `short`, `int`, `long`, `long long` and their `unsigned` forms;
  - `int8_t` … `uint64_t`, `size_t`, `bool`/`_Bool`, `float`, `double`;
  - pointers to any of these, to `void` or to a struct, with `const`.
  - An array parameter can be written `const int32_t x[]`.
- **Returns:** `void`, any of the above except arrays.
- **Not supported:** structs passed or returned by value, `long double`, variadic functions and function pointers.
- **No code in the starter:** until the student writes the label, **Call** reports ``undefined reference to `subU32': no label subU32: in the assembly code``, and without `.global` the panel warns that a C program could not link to it. Leaving out a stub also means no case passes by accident: a `bx lr` stub returns the first argument, so `maxS16(7, 3)` = 7 would pass on it.

### Structs

Declare a struct in `@ C:` comments, across as many lines as needed, before or after the prototypes that use it:

```
@ C: typedef struct _BUSINESS2
@ C: {
@ C:     uint32_t    taxId;
@ C:     char        name[27];
@ C:     ...
@ C: } BUSINESS2;
@ C: extern int32_t findCityAligned(const char city[], const BUSINESS2 business[], uint32_t count);
```

- **Layout:** members are laid out as gcc does on ARM, and the panel shows the layout, e.g. `BUSINESS2: 112 bytes, aligned to 4: … char city[30] at 72 …`. Check it against your answer key.
- **Packed structs:** add `__attribute__((packed))` before or after the braces.
- **One layout per file:** a struct name has one layout per file. If one exercise assumes default alignment and another assumes packing (HW5 2d and 2e), put them in **separate files**, each declaring the struct its own way.
- **Array sizes must be plain numbers** (`char state[3]`, not `char state[2+1]`). This is deliberate: students should work out the real sizes to compute member offsets.
- **Members** can be numbers, `bool`, pointers and fixed arrays. Structs inside structs and unions are not supported.

### C variables

Data can also be declared as C variables, placed in memory fresh for every call:

```
@ C: struct student s[] = {{"Ann", 90, 3.5}, {"Bob", 75, 2.8}};
@ C: int counts[4] = {1, 2, 3, 4};
```

A pointer argument can then be given as an address into them (`s`, `&s[1]`, `&s[1].name`, `&counts[2]`), and each variable gets its own expected box. This suits exercises where the C code passes the address of existing data. For most homework, passing an array argument directly is simpler.

## 4. Making cases in the panel

1. Open the `.s` file and choose the **C Call** tab, then pick the function.
2. Fill in each argument. For a pointer, the drop-down selects how to describe it (section 6): **array**, **string** (char pointers only), **output buffer** or **address**.
3. Fill in **Expected … result** for the return value. For each pointer that is not `const`, fill in the **expected after the call** box under it. Leave a box empty to not check it.
4. Press **Call** to see the checks.
5. To keep it as a new case, type a name in the **Case** row and press **Save as**. The new case becomes the one shown, so editing continues on the copy. **Delete** removes the case shown.
6. Save the `.s` file (Ctrl+S): this writes the `.ccall`.

For many similar cases, it is quicker to make one in the panel, save, and then copy and edit cases in the `.ccall` file with a text editor.

## 5. The .ccall file format

```json
{
  "format": "VisUAL3 C Call cases",
  "version": 1,
  "selected": "leftString",
  "functions": {
    "leftString": {
      "current": "abcdef, 5",
      "cases": [
        {
          "name": "abcdef, 5",
          "args": {
            "strOut": { "kind": "string", "text": "\"XXXXXXXXXXXXXXX\"" },
            "strIn":  { "kind": "string", "text": "abcdef" },
            "length": "5"
          },
          "expect": {
            "strOut": "\"abcde\""
          }
        }
      ]
    }
  }
}
```

| Field | Meaning |
|---|---|
| `format`, `version` | Always `"VisUAL3 C Call cases"` and `1`. |
| `selected` | The function shown when the file is opened (optional). |
| `functions` | One entry per function, keyed by its name as in the prototype. |
| `current` | The case shown first for that function. |
| `cases[].name` | Shown in the Case row. Make it say what the case tests: `"0x8000 * 2 overflows to 0"` beats `"case 3"`. |
| `cases[].args` | One entry per parameter, keyed by parameter name. |
| `cases[].expect` | `"return"` for the return value, a pointer parameter's name for its contents after the call, or a C variable's name. Omit a key to not check it. |

**Arguments:**
- A plain string is used as typed: numbers, and arrays in braces.
- An object `{ "kind": …, "text": … }` also says how a pointer is described: `"array"`, `"string"`, `"output"` or `"address"`.
- Use the object form for strings, output buffers and addresses.
- Inside JSON, a quote in the text is written `\"`.

## 6. Argument and expected-value syntax

### Numbers

| Parameter type | Examples | Notes |
|---|---|---|
| integers | `42`, `-1`, `0x1F`, `0b1010`, `'A'` | Must fit the type: `300` is rejected for `uint8_t`, `-1` for `uint32_t` (write `0xFFFFFFFF`). |
| `long long`, `int64_t`, `uint64_t` | `0x100000000`, `-5` | Passed in a register pair or on the stack, as AAPCS requires. |
| `bool` | `0`, `1` | The panel shows false/true. |
| `float`, `double` | `1.5`, `-0.25`, `3e8` | Passed in s/d registers (hard-float). |

### Pointers

| Kind | You type | Memory gets |
|---|---|---|
| array | `{10, 20, 30}` | 3 elements, exactly as typed. |
| array | `{10, 20, 30} [8]` | 8 elements: 10, 20, 30 and five zeros. The size counts **elements, not bytes**. |
| output | `8` | 8 zeroed elements. |
| string | `hello` | The bytes `h e l l o` and a 0 byte. Quotes are optional, and escapes such as `\n` work. |
| string | `"Hello, " [32]` | A 32-byte buffer holding `Hello, ` and zeros. **Quotes are required** when a size follows. |
| string | `""` (empty) | Just the 0 byte. |
| address | `NULL`, `0x10000`, a label, `&s[1]` | The address itself: a label in the file, or a C variable. |
| array of structs | `{{1001, "Acme", 'N', "Main St", 100, "Arlington", "TX", 76019}, {…}}` | A C initializer per struct, members in declaration order. Members left out are 0. A `char` array member takes a string. |

- **Element order:** arrays are laid out in order from the address passed, using the element type's size. A `double` array is 8 bytes per element.
- **Buffers are placed in their own memory** starting at `0x10000000`. They are fresh for every call, so one case cannot affect the next.

### Expected values

- **Return values** use the same syntax as arguments.
- **Matching rules:**
  - integers, pointers and `bool` must match exactly;
  - `float` matches within a relative 10⁻⁶ and `double` within 10⁻¹²;
  - NaN matches NaN.
- **Pointer results** can be expected as a label, an address, `NULL`, or a place in an argument array: `&s[1]`, `&s[1].score`, or `s`.
- **Contents after the call:**
  - `{0, 1, 4}`: compares the first 3 elements only.
  - `"text"`: compares the characters and the 0 byte after them, for char buffers. Bytes after the terminator are not compared.
  - To check bytes past a terminator, expect the buffer as a list of byte values instead: `{97, 98, 0, 88}` is `a`, `b`, the 0, then an `X` that must still be there. Character literals in such a list (`{'a', 'b', 0, 'X'}`) work from VisUAL3 3.0.0-dev.8; use numbers for earlier versions.
  - `{{"Ann", 95}, {"Bob", 80}}`: for struct arrays, compares only the structs listed, and within each, only the members listed (in order). To check a later member, list the ones before it. To catch a function that changes a member it should leave alone, list that member too.

## 7. Choosing good cases

Aim for 3–8 cases per function: one or two typical calls, then the edges where student code goes wrong. Name each case after what it tests. Checklist:

- **Typical values** first, so a student sees a simple case pass before the tricky ones.
- **Boundaries:** 0, 1, the largest and smallest values of the type (`32767`/`-32768` for `int16_t`, `0xFFFFFFFF` for `uint32_t`, `INT32_MIN`).
- **Signed versus unsigned:** a case that only passes with the right comparison or shift. `maxU32(0xFFFFFFFF, 1)` must return `0xFFFFFFFF`, which fails if `movlt` is used instead of `movlo`. `shiftRightS32(-64, 1)` = -32 needs `asr`.
- **Overflow and wrap-around:** `subU32(3, 10)` = 4294967289; `subS32(INT32_MIN, 1)` = `INT32_MAX`; `addU64(0xFFFFFFFF, 1)` must carry into the high word.
- **Narrow return types:**
  - A function returning `uint16_t`, `int8_t`, `bool` etc. must leave r0 zero- or sign-extended (AAPCS), just as compiled C does inside the callee.
  - `shiftU16(0x8000, 1)` should return 0. Code that leaves `0x10000` in r0 is flagged automatically, even when the low 16 bits are right.
- **Counts:** `count = 0`, and a count smaller than the array (`sumS32({1, 2, 100}, 2)` = 3), which catches code that runs past the end.
- **Strings:**
  - the empty string (just the `\0`);
  - one string a prefix of the other (`"abc"` vs `"abcd"`, both ways);
  - case differences;
  - for a search, a prefix of the target (`"Dal"` must not match `"Dallas"`) and a longer string.
- **Output strings:**
  - Pre-fill the output buffer with non-zero characters, such as `"XXXXXXXXXXXXXXX"`, so a missing terminator shows as `"abcdeX"`. A zeroed buffer would hide the bug.
  - A string comparison stops at the terminator, so it can't see bytes written after it. To catch code that copies too much, give the input bytes after its 0 (`"abcdef\0ZZ"`) and expect the output as byte values, including the first untouched byte after the terminator: `{97, 98, 99, 100, 101, 102, 0, 88}`.
- **Searches:** first, middle and last positions, not found, a duplicate (the first match wins), and a match beyond `count`.
- **Floating point:** an all-negative array for a maximum (catches starting from 0.0), and values that aren't exact in binary (`0.1 + 0.2`). Tolerance handles rounding.
- **Leave ambiguous behaviour untested.** If the exercise doesn't say what happens (an empty array for a maximum), don't make a case for it. Otherwise a reasonable solution fails.
- **Follow the specification literally**, or change the specification. HW4 2e lists only `A`–`F`, so `"1a"` → 0 is a case.

## 8. What is checked automatically

Every call, with or without expectations, is checked against the AAPCS (hard-float) calling convention. Any breach is listed in the result:

| Message | Cause |
|---|---|
| `R4 was not preserved (…): r4-r11 are callee-saved` | r4–r11 changed without being restored. The panel fills them with recognisable values (`0xC0DE0004` …) before the call. |
| `SP not restored (…)` | Unbalanced `push`/`pop`. |
| `s16 was not preserved …` | s16–s31 (d8–d15) changed. |
| `FPSCR control bits changed …` | The rounding mode, FZ or DN was left changed. |
| `r0 = 0x10000 is not a properly zero-extended unsigned short …` | A narrow result was not extended to 32 bits. |
| `strIn points to const data but the function changed it` | A write through a `const` pointer. |

The call also reports if the function never returns (an infinite loop). The checker in section 9 counts any of these as a failure.

## 9. Verifying cases with the checker

### In the app

Put a copy of the `.ccall` beside your solution, with the same name, open the solution, and press **Call** on each case.

### From the command line

The VisUAL3 repository has a checker that runs every case in a folder, with the same emulator and checks as the panel. It needs a source checkout, built once. The repository becomes public with the 3.0.0 release; before that, ask its owner for access, or check in the app as above.

```
git clone https://github.com/ToddRosenkrantz/VisUAL3.git
cd VisUAL3
npm install
npm run fable          # builds the emulator (.NET 10 SDK and Node.js 22 needed; see README)
```

Then:

```
npm run check-cases -- <cases-folder> [solutions-folder]
```

- **Which file is tested:** every `.ccall` under `<cases-folder>` (including subfolders) is run. The code tested is the `.s` with the **same relative path** under `[solutions-folder]`, or, without one, the `.s` beside the `.ccall` (the starter).
- **Output:** one line per case, then totals:

  ```
  HW4/HW4_2c  leftString  abcdef, 5  ok    ✔ strOut: "abcde", as expected
  HW3/HW3_1e  shiftU16  0x8000 * 2 overflows to 0  FAIL  ✔ result: 0 (0x0), as expected | ⚠ r0 = 0x10000 is not a properly zero-extended unsigned short: AAPCS requires the callee to extend narrow results

  154 passed, 0 failed
  ```

- **Exit code:** 1 if any case fails, so it can be scripted.
- **Cases with no expectations** are reported as failures ("nothing was checked"), so every case must expect something.

**Typical runs:**

```
# all cases pass against the reference solutions
npm run check-cases -- "Homework/VisUAL3" "Homework/VisUAL3_solutions"

# the starters load and their cases run; every case fails with "no label", as the starters hold no code
npm run check-cases -- "Homework/VisUAL3"
```

**Mutation testing.** Copy the solutions folder, plant a mistake per function, and run the checker against the copy. Each planted bug should fail at least one case. For the homework, these were all caught:

| Planted mistake | Caught by |
|---|---|
| `maxU32` with `movlt` (signed) | `0xFFFFFFFF is the largest (unsigned)` |
| `shiftU16` without `uxth` | `0x8000 * 2 overflows to 0` (narrow result not extended) |
| `dotpS32` that pushes r4/r5 and never pops | every case: R4/R5 not preserved, SP not restored |
| `leftString` without the terminator | every case: `expected "abcde", got "abcdeX"` |
| `leftString` that copies `length` characters without stopping at the end of `strIn` | `stops at the end of strIn`: `got {…, 0, 90}` (a `Z` was copied) |
| `maxF32` starting from 0.0 | `all negative` |
| `findCityPacked` using the aligned offsets (72, 112) | `first entry`, `Dallas: the first of two`, `last entry` |

## 10. Organising and distributing

- **One file per question**, in a folder per assignment, named as the assignment numbers them:

  ```
  VisUAL3/HW2/HW2_4a.s   VisUAL3/HW2/HW2_4a.ccall
  VisUAL3/HW2/HW2_4b.s   VisUAL3/HW2/HW2_4b.ccall
  ...
  VisUAL3_solutions/HW2/HW2_4a.s   VisUAL3_solutions/HW2/HW2_4a.ccall   (never shared)
  ```

- **Keep solutions in a separate folder** that mirrors the cases folder. The checker pairs them by path, and handing out the cases folder can't leak a solution. A copy of each `.ccall` beside its solution makes the cases load when you open the solution; copy them again whenever the cases change.
- **Each starter's header** quotes the question's C prototype and comment, so the file stands alone.
- **Students can add their own cases** with **Save as**; they are saved into their copy of the `.ccall`.
- **Updating cases:** students who already saved their file keep their own `.ccall`, so new cases need a new copy of the `.ccall`.

## 11. Grading submissions from Canvas

`npm run grade-cases` grades every student's code with the same cases and the same checks as the panel, from Canvas's bulk download:

```sh
npm run grade-cases -- <submissions.zip> <cases-folder> [--points points.json] [--out folder]
                       [--gradebook export.csv --column "Homework 4"]
```

1. In the Canvas assignment, choose **Download Submissions**. Give the grader the `submissions.zip` you get, or the folder you unzip it into.
2. Give it the assignment's cases folder, such as `Homework/VisUAL3/HW4`. Each `HWn_qx.ccall` there is a question; the `@ C:` prototypes come from the starter `HWn_qx.s` beside it, so it doesn't matter if a student edited or deleted theirs.
3. To fill the gradebook, export it from Canvas (**Grades → Export**) and pass the export with `--gradebook`. Pass part of the assignment column's name with `--column`.

**Finding each student's files.** Canvas names each submitted file `<name>[_LATE]_<user id>_<submission id>_<file>`, so files are grouped by the user id. A student may submit a zip, zips inside a zip, or loose `.s` files. A file counts for question `HW4_2c` when its name is `HW4_2c.s`, ignoring folders, letter case and Canvas's copy numbers (`HW4_2c-1.s`, `HW4_2c (1).s`). Zips are read in memory and nothing is extracted, and Mac `__MACOSX` entries are skipped. Files saved by Notepad as UTF-16, or with CR LF line ends, are read correctly. Other archives (`.rar`, `.7z`) can't be read and are reported as such.

**Credit.** A question is worth its points × the fraction of its cases that pass. A case passes only when every check passes and no calling-convention problem is reported, the same rule as the checker. A function that never returns fails each case after a million instructions. Questions are worth 10 points unless `--points` gives a JSON file such as `{"HW4_2d": 30, "HW4_2e": 20}`, or `--default-points` changes the default.

**Results**, in `--out` (default `grades`):

| File | Contents |
|---|---|
| `grades.csv` | A row per student: name, Canvas user id, late or not, points per question (blank when the file wasn't found) and the total. |
| `feedback/<name>_<id>.txt` | Every case's ✔ or ✘ with the expected and actual values, which file was graded, questions with no file, and files that were ignored or couldn't be read. Paste it as a submission comment. |
| `canvas-import.csv` | With `--gradebook`: the export's student columns and the chosen assignment column, filled in. Import it with **Grades → Import**. The total is scaled to the column's points possible, and students with no submission keep their current grade. |

The grader doesn't apply late penalties: `grades.csv` marks late submissions so that you can.

**Before trusting the grades,** run the grader on a small folder of known submissions: the reference solution should get full marks and the starter none. Read a few feedback files. If a student's file is missing because it was named differently, rename it in the download and run the grader again.

## 12. Worked example: leftString

HW4 question 2c: *`void leftString(char * strOut, const char * strIn, uint32_t length)` copies up to, but not exceeding, `length` characters from the start of `strIn`. `strIn = 'abcdef', length = 5` → `'abcde'`; `length = 7` → `'abcdef'`.*

**1. Starter, `HW4/HW4_2c.s`:**

```
@ CSE2312 Fall 2026, Homework 4, question 2c
@ void leftString(char * strOut, const char * strIn, uint32_t length)
@ C: extern void leftString(char *strOut, const char *strIn, uint32_t length);

@ Write the function below. Open the C Call tab to run the saved test cases,
@ which check the result and the calling convention.
```

**2. Cases.** The function returns nothing, so every case checks `strOut` after the call. `strOut` starts as 15 `X`s, so the output must end with its own 0 byte.

| Case | strIn | length | expect strOut |
|---|---|---|---|
| abcdef, 5 | `abcdef` | 5 | `"abcde"` |
| length past the end | `abcdef` | 7 | `"abcdef"` |
| exact length | `abcdef` | 6 | `"abcdef"` |
| length 0 | `abcdef` | 0 | `""` |
| strIn "" (just \0) | `` | 3 | `""` |
| stops at the end of strIn | `"abcdef\0ZZ"` | 9 | `{97, 98, 99, 100, 101, 102, 0, 88}` |

In the `.ccall` (first case shown):

```json
{
  "name": "abcdef, 5",
  "args": {
    "strOut": { "kind": "string", "text": "\"XXXXXXXXXXXXXXX\"" },
    "strIn":  { "kind": "string", "text": "abcdef" },
    "length": "5"
  },
  "expect": { "strOut": "\"abcde\"" }
}
```

The last case exists because of mutation testing: a version that copies `length` characters without stopping at the end of `strIn` passed the first five. It copies the 0 byte too, so every string comparison still matched. Only the byte after the terminator shows the difference.

**3. Verify** against the solution in `VisUAL3_solutions/HW4/HW4_2c.s`: all 6 pass.

**4. Mutation-test:**
- Removing the final `strb` of the 0 byte fails every case, e.g. `expected "abcde", got "abcdeX"`.
- Not stopping at the end of `strIn` fails `stops at the end of strIn` with `got {97, 98, 99, 100, 101, 102, 0, 90}`.

## 13. Pitfalls and FAQ

**A case passes in the panel but the checker says FAIL.**
The checker also fails a case for calling-convention problems, which the panel lists below the checks without turning them into ✘. Look for the ⚠ part of the line.

**"the expected value is not valid".**
The value doesn't fit the type (e.g. `-1` for a `uint32_t` result: write `0xFFFFFFFF`), or a string was expected for a non-char buffer.

**There's no "expected after the call" box for a parameter.**
It is a `const` pointer, so the function must not change it. A write to it is flagged automatically.

**The struct offsets don't match my answer key.**
Check the layout line in the panel. Typical causes: a missing `__attribute__((packed))`, or a member type that differs from the assignment (`uint32_t` vs `uint16_t`).

**A size like `char name[NAME_LEN]` or `[26+1]` is rejected.**
Sizes must be plain numbers; the error names the member.

**Two exercises use the same struct with different layouts.**
Put them in separate files (see HW5 2d and 2e).

**Can a case set registers other than the arguments, or memory outside the arguments?**
No. Pass data through parameters, C variables, or labelled data in the `.s` file passed as an address.

**How many instructions can a call run?**
The checker stops a call after 1,000,000 instructions and reports that it did not return. The app stops after **Max Steps** in **Edit → Preferences** and says `Not returned after N instructions (infinite loop?)`. With Max Steps set to 0 there is no limit, so the call runs until it is stopped.

**Line endings.**
Files with Windows (CR LF) line endings work in both the app and the checker.
