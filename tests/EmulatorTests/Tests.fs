/// Smoke tests for the emulator. Dependency-free so the same file runs on
/// .NET (dotnet run) and as Fable-compiled JavaScript under Node.
module EmulatorTests

open CommonData
open ExecutionTop

type Expect =
    | Reg of RName * uint32
    | Flags of n : bool * z : bool * c : bool * v : bool
    | Mem of uint32 * uint32
    /// the program must be rejected by the assembler
    | ParseError
    /// the program must assemble but stop with a run time error
    | RunError
    /// memory word never written
    | NoMem of uint32
    /// VFP word n (s<n> for n < 32)
    | VW of int * uint32
    /// d register: high word, low word
    | DW of int * uint32 * uint32
    /// the FPSCR
    | Fpscr of uint32

/// Assemble and run `code` (one instruction per line) to completion.
let run (code : string) =
    let lines = code.Split('\n') |> Array.map (fun l -> l.Trim()) |> Array.toList
    let lim = reLoadProgram lines
    if lim.Errors <> [] then
        Error (sprintf "parse errors: %A" lim.Errors)
    else
        let fl = { N = false; Z = false; C = false; V = false }
        let ri = getRunInfoFromImageWithInits NoBreak lim initialRegMap fl Map.empty lim.Mem
        let ri' = asmStep 100000L ri
        match ri'.State with
        | PSError Errors.TBEXIT -> Ok (fst ri'.dpCurrent) // returned from top level
        | PSError e -> Error (sprintf "run error: %A" e)
        | _ -> Ok (fst ri'.dpCurrent)

let check (dp : DataPath) expect =
    match expect with
    | Reg (r, v) ->
        let a = dp.Regs.[r]
        if a = v then None else Some (sprintf "%A = 0x%X, expected 0x%X" r a v)
    | Flags (n, z, c, v) ->
        let f = dp.Fl
        if (f.N, f.Z, f.C, f.V) = (n, z, c, v) then None
        else Some (sprintf "NZCV = %A, expected %A" (f.N, f.Z, f.C, f.V) (n, z, c, v))
    | VW (n, v) ->
        let a = Map.tryFind n dp.Fp.W |> Option.defaultValue 0u
        if a = v then None else Some (sprintf "VFP word %d = 0x%08X, expected 0x%08X" n a v)
    | DW (n, hi, lo) ->
        let w i = Map.tryFind i dp.Fp.W |> Option.defaultValue 0u
        if (w (2 * n + 1), w (2 * n)) = (hi, lo) then None
        else Some (sprintf "d%d = 0x%08X_%08X, expected 0x%08X_%08X" n (w (2 * n + 1)) (w (2 * n)) hi lo)
    | Fpscr v -> if dp.Fp.FPSCR = v then None else Some (sprintf "FPSCR = 0x%08X, expected 0x%08X" dp.Fp.FPSCR v)
    | ParseError -> Some "expected a parse error"
    | RunError -> Some "expected a run time error"
    | NoMem a ->
        match Map.tryFind (WA a) dp.MM with
        | None -> None
        | x -> Some (sprintf "[0x%X] = %A, expected unwritten" a x)
    | Mem (a, v) ->
        match Map.tryFind (WA a) dp.MM with
        | Some (Dat d) when d = v -> None
        | x -> Some (sprintf "[0x%X] = %A, expected 0x%X" a x v)

let tests : (string * string * Expect list) list = [
    "MOV / ADD",
    """MOV R0, #5
       ADD R1, R0, #7
       ADD R2, R1, R0, LSL #2""",
    [ Reg (R0, 5u); Reg (R1, 12u); Reg (R2, 32u) ]

    "large immediate via LDR =",
    """LDR R0, =0xDEADBEEF
       MVN R1, #0""",
    [ Reg (R0, 0xDEADBEEFu); Reg (R1, 0xFFFFFFFFu) ]

    "unsigned wraparound sets Z and C",
    """MVN R0, #0
       ADDS R1, R0, #1""",
    [ Reg (R1, 0u); Flags (false, true, true, false) ]

    "signed overflow sets V",
    """LDR R0, =0x7FFFFFFF
       ADDS R1, R0, #1""",
    [ Reg (R1, 0x80000000u); Flags (true, false, false, true) ]

    "SUBS borrow clears C",
    """MOV R0, #3
       SUBS R1, R0, #5""",
    [ Reg (R1, 0xFFFFFFFEu); Flags (true, false, false, false) ]

    "CMP equal",
    """MOV R0, #9
       CMP R0, #9""",
    [ Flags (false, true, true, false) ]

    "shifts LSR / ASR / ROR",
    """LDR R0, =0x80000010
       MOV R1, R0, LSR #4
       MOV R2, R0, ASR #4
       MOV R3, R0, ROR #4""",
    [ Reg (R1, 0x08000001u); Reg (R2, 0xF8000001u); Reg (R3, 0x08000001u) ]

    "MOVS LSL carry-out",
    """LDR R0, =0x80000001
       MOVS R1, R0, LSL #1""",
    [ Reg (R1, 2u); Flags (false, false, true, false) ]

    "assembler expressions (+ - *)",
    """      MOV R0, #20-3*2
              LDR R1, =DATA+8-4
              ADR R2, DATA
              SUB R3, R1, R2
       DATA:  .word 0, 0""",
    [ Reg (R0, 14u); Reg (R3, 4u) ]

    "conditional execution",
    """MOV R0, #1
       CMP R0, #2
       MOVLT R1, #10
       MOVGE R2, #20""",
    [ Reg (R1, 10u); Reg (R2, 0u) ]

    "loop sums 1..10",
    """      MOV R0, #0
              MOV R1, #10
       LOOP:  ADD R0, R0, R1
              SUBS R1, R1, #1
              BNE LOOP""",
    [ Reg (R0, 55u); Reg (R1, 0u) ]

    "BL / MOV PC, LR subroutine",
    """      MOV R0, #4
              BL DOUBLE
              B FIN
       DOUBLE: ADD R0, R0, R0
              MOV PC, LR
       FIN:   MOV R1, #1""",
    [ Reg (R0, 8u); Reg (R1, 1u) ]

    "LDR/STR with data section",
    """      ADR R0, DATA
              LDR R1, [R0]
              LDR R2, [R0, #4]
              ADD R3, R1, R2
              STR R3, [R0, #8]
              LDR R4, [R0, #8]
       DATA:  .word 100, 23, 0""",
    [ Reg (R3, 123u); Reg (R4, 123u) ]

    "LDRB / STRB",
    """      ADR R0, DATA
              LDRB R1, [R0, #1]
              MOV R2, #0xAB
              STRB R2, [R0]
              LDR R3, [R0]
       DATA:  .word 0x11223344""",
    [ Reg (R1, 0x33u); Reg (R3, 0x112233ABu) ]

    "post-indexed and writeback",
    """      ADR R0, DATA
              LDR R1, [R0], #4
              LDR R2, [R0, #4]!
       DATA:  .word 1, 2, 3""",
    [ Reg (R1, 1u); Reg (R2, 3u) ]

    "STMFD / LDMFD stack round-trip",
    """MOV R1, #1
       MOV R2, #2
       MOV R3, #3
       STMFD R13!, {R1-R3}
       MOV R1, #0
       MOV R2, #0
       MOV R3, #0
       LDMFD R13!, {R1-R3}""",
    [ Reg (R1, 1u); Reg (R2, 2u); Reg (R3, 3u); Reg (R13, initStackPointer) ]

    "STMIA / LDMDB ordering",
    """      ADR R0, DATA
              MOV R1, #7
              MOV R2, #8
              STMIA R0!, {R1, R2}
              LDMDB R0, {R3, R4}
       DATA:  .space 8""",
    [ Reg (R3, 7u); Reg (R4, 8u) ]
]

let mulTests = [
    "MUL basic",
    """MOV R0, #6
       MOV R1, #7
       MUL R2, R0, R1""",
    [ Reg (R2, 42u) ]

    "MUL keeps low 32 bits (JS precision)",
    """MVN R0, #0
       MUL R1, R0, R0
       LDR R2, =0x12345678
       LDR R3, =0x9ABCDEF0
       MUL R4, R2, R3
       LDR R5, =0xFFFF
       MUL R6, R5, R5""",
    [ Reg (R1, 1u); Reg (R4, 0x242D2080u); Reg (R6, 0xFFFE0001u) ]

    "MUL Rd = Rn and two-operand form",
    """MOV R0, #5
       MUL R0, R0, R0
       MOV R1, #3
       MUL R0, R1""",
    [ Reg (R0, 75u) ]

    "MUL lowercase",
    """mov r0, #9
       mov r1, #9
       mul r2, r0, r1""",
    [ Reg (R2, 81u) ]

    "MULS sets Z, keeps C and V",
    """MOV R0, #1
       CMP R0, R0
       MOV R1, #0
       MULS R2, R0, R1""",
    [ Reg (R2, 0u); Flags (false, true, true, false) ]

    "MULS sets N, keeps V",
    """LDR R0, =0x7FFFFFFF
       ADDS R0, R0, #1
       MOV R1, #0x80000000
       MOV R2, #3
       MULS R3, R1, R2""",
    [ Reg (R3, 0x80000000u); Flags (true, false, false, true) ]

    "MUL without S leaves flags",
    """MOV R0, #2
       CMP R0, R0
       MUL R1, R0, R0""",
    [ Reg (R1, 4u); Flags (false, true, true, false) ]

    "MLA basic and wraparound",
    """MOV R0, #3
       MOV R1, #4
       MOV R2, #5
       MLA R3, R0, R1, R2
       MVN R4, #0
       MOV R5, #2
       MLA R6, R4, R5, R2""",
    [ Reg (R3, 17u); Reg (R6, 3u) ]

    "MLAS sets Z",
    """MVN R0, #0
       MOV R1, #1
       MOV R2, #1
       MLAS R3, R0, R1, R2""",
    [ Reg (R3, 0u); Flags (false, true, false, false) ]

    "conditional MUL / MLA",
    """MOV R0, #2
       CMP R0, #2
       MULNE R1, R0, R0
       MLAEQ R2, R0, R0, R0""",
    [ Reg (R1, 0u); Reg (R2, 6u) ]

    "MUL rejects literal", "MUL R0, R1, #3", [ ParseError ]
    "MUL rejects PC", "MUL R15, R0, R1", [ ParseError ]
    "MLA needs four operands", "MLA R0, R1, R2", [ ParseError ]
]

let shiftTests = [
    "op2 LSR #32 / ASR #32",
    """LDR R1, =0x80000011
       MOVS R0, R1, LSR #32
       MOV R2, R1, ASR #32""",
    [ Reg (R0, 0u); Reg (R2, 0xFFFFFFFFu); Flags (false, true, true, false) ]

    "ASRS R0, R1, #32 sets N and C",
    """LDR R1, =0x80000011
       ASRS R0, R1, #32""",
    [ Reg (R0, 0xFFFFFFFFu); Flags (true, false, true, false) ]

    "LSRS #32 standalone",
    """LDR R1, =0x80000011
       LSRS R0, R1, #32""",
    [ Reg (R0, 0u); Flags (false, true, true, false) ]

    "ASL synonym in op2",
    """LDR R1, =0x80000011
       MOV R0, R1, ASL #2
       ADD R2, R0, R0, asl #1""",
    [ Reg (R0, 0x44u); Reg (R2, 0xCCu) ]

    "two-operand LSL / LSR / ASR / ROR / RRX",
    """LDR R1, =0x80000011
       LSL R1, #4
       MOV R2, #0x100
       MOV R3, #4
       LSR R2, R3
       MVN R4, #0
       ASR R4, #8
       LDR R5, =0x12345678
       ROR R5, #8
       MOV R6, #3
       CMP R6, R6
       RRX R6""",
    [ Reg (R1, 0x110u); Reg (R2, 0x10u); Reg (R4, 0xFFFFFFFFu); Reg (R5, 0x78123456u); Reg (R6, 0x80000001u) ]

    "optional Rd for data processing",
    """MOV R0, #5
       ADD R0, #3
       MOV R1, #2
       ADD R0, R1
       ADD R0, R1, LSL #2
       SUB R0, R1, lsl #1
       ORR R0, #0x100
       EOR R0, R1, ROR R1""",
    [ Reg (R0, 0x8000010Eu) ]

    "register shift uses bottom byte",
    """MOV R0, #1
       LDR R2, =0x101
       MOV R3, R0, LSL R2""",
    [ Reg (R3, 2u) ]

    "LSLS by register 32 and 33",
    """LDR R1, =0x80000011
       MOV R2, #32
       LSLS R3, R1, R2
       MOV R4, #33
       MOVS R5, R1, LSL R4""",
    [ Reg (R3, 0u); Reg (R5, 0u); Flags (false, true, false, false) ]

    "LSR / ASR / ROR by register >= 32",
    """LDR R1, =0x80000011
       MOV R2, #32
       MOVS R3, R1, LSR R2
       MOV R4, #40
       MOVS R5, R1, ASR R4
       MOVS R6, R1, ROR R2
       MOV R7, #36
       MOVS R8, R1, ROR R7""",
    [ Reg (R3, 0u); Reg (R5, 0xFFFFFFFFu); Reg (R6, 0x80000011u); Reg (R8, 0x18000001u)
      Flags (false, false, false, false) ]

    "shift by register 0 leaves carry",
    """MOV R0, #1
       CMP R0, R0
       MOV R2, #0
       MOVS R3, R0, LSR R2""",
    [ Reg (R3, 1u); Flags (false, false, true, false) ]

    "LSL #32 rejected", "LSL R0, R1, #32", [ ParseError ]
    "ROR #32 rejected", "MOV R0, R1, ROR #32", [ ParseError ]
    "LSR #33 rejected", "MOV R0, R1, LSR #33", [ ParseError ]
]

let noFloatTests = [
    "VFP instruction in upper case", "VMOV.F32 S1, #2.0\nVADD.F32 S0, S1, S1", [ VW (0, 0x40800000u) ]
    "VFP instruction after a label", "LOOP: vmov.f64 d0, #1.0", [ DW (0, 0x3FF00000u, 0u) ]
    "V-named label still allowed",
    """      B VALUE
       VALUE: MOV R0, #1""",
    [ Reg (R0, 1u) ]
]

let stackTests = [
    "PUSH / POP round trip",
    """MOV R1, #1
       MOV R2, #2
       MOV R3, #3
       PUSH {R1-R3}
       POP {R4-R6}""",
    [ Reg (R4, 1u); Reg (R5, 2u); Reg (R6, 3u); Reg (R13, initStackPointer) ]

    "PUSH memory layout (lowest register at lowest address)",
    """MOV R1, #0x11
       MOV R2, #0x22
       PUSH {R2, R1}""",
    [ Reg (R13, initStackPointer - 8u); Mem (initStackPointer - 8u, 0x11u); Mem (initStackPointer - 4u, 0x22u) ]

    "single register PUSH / POP",
    """MOV R0, #42
       PUSH {R0}
       MOV R0, #0
       POP {R1}""",
    [ Reg (R1, 42u); Reg (R13, initStackPointer) ]

    "nested calls with PUSH {LR} / POP {PC}",
    """          MOV R4, #1
                  BL OUTER
                  B DONE
       OUTER:     PUSH {R4, LR}
                  MOV R4, #2
                  BL INNER
                  ADD R0, R0, R4
                  POP {R4, PC}
       INNER:     PUSH {R4, LR}
                  MOV R4, #10
                  MOV R0, R4
                  POP {R4, PC}
       DONE:      MOV R5, R4""",
    [ Reg (R0, 12u); Reg (R5, 1u); Reg (R13, initStackPointer) ]

    "lowercase push / pop",
    """          mov r4, #7
                  bl fn
                  b done
       fn:        push {r4, lr}
                  mov r4, #9
                  pop {r4, pc}
       done:      mov r0, r4""",
    [ Reg (R0, 7u) ]

    "conditional PUSH / POP",
    """MOV R0, #1
       CMP R0, #1
       PUSHNE {R0}
       PUSHEQ {R0, R1}
       POPEQ {R2, R3}""",
    [ Reg (R2, 1u); Reg (R13, initStackPointer) ]

    "PUSH needs braces", "PUSH R0", [ ParseError ]
    "POP rejects duplicates", "POP {R1, R1}", [ ParseError ]
    "PUSH rejects SP", "PUSH {SP}", [ ParseError ]

    "LDM / STM without suffix mean IA",
    """      ADR R0, DATA
              MOV R1, #5
              MOV R2, #6
              STM R0, {R1, R2}
              LDM R0!, {R3, R4}
              ADR R5, DATA
              SUB R6, R0, R5
       DATA:  .space 8""",
    [ Reg (R3, 5u); Reg (R4, 6u); Reg (R6, 8u) ]

    "LDRIA rejected (not a crash)", "LDRIA R0, {R1}", [ ParseError ]
]

let pushPreDecTests = [
    "PUSH is pre-decrement (full descending, = STMDB SP!)",
    """MOV R0, #0x55
       PUSH {R0}""",
    [ Reg (R13, initStackPointer - 4u); Mem (initStackPointer - 4u, 0x55u); NoMem initStackPointer ]

    "PUSH matches STR Rx, [SP, #-4]! and POP matches LDR Rx, [SP], #4",
    """MOV R0, #0x66
       STR R0, [SP, #-4]!
       POP {R1}
       PUSH {R0}
       LDR R2, [SP], #4""",
    [ Reg (R1, 0x66u); Reg (R2, 0x66u); Reg (R13, initStackPointer) ]
]

let labelTests = [
    "single-letter labels",
    """      MOV R0, #0
              B L
              MOV R0, #99
       L:     ADD R0, R0, #1
       X:     ADR R1, D
              LDR R2, [R1]
       D:     .word 7""",
    [ Reg (R0, 1u); Reg (R2, 7u) ]

    "single-letter label in loop and expression",
    """      MOV R0, #3
       A:     SUBS R0, R0, #1
              BNE A
              LDR R1, =N+1
       .equ N, 4""",
    [ Reg (R0, 0u); Reg (R1, 5u) ]

    "underscore labels (_start)",
    """_start: MOV R0, #1
               B _end
               MOV R0, #2
       _end:   MOV R1, R0""",
    [ Reg (R1, 1u) ]

    "label must not start with a digit", "1A MOV R0, #1", [ ParseError ]
]

let gnuSyntaxTests = [
    "bare B is an instruction, never a label", "B", [ ParseError ]
    "bare mnemonic is not a label", "MOV", [ ParseError ]
    "label without colon rejected", "loop add r0, r0, #1", [ ParseError ]
    "single letter label with colon",
    """b: mov r0, #1
       b c
       mov r0, #2
       c: mov r1, r0""",
    [ Reg (R1, 1u) ]

    "comments and ; separators",
    """# preprocessor-style comment line
       mov r0, #1 @ ARM comment
       mov r1, #2 // C++ comment
       /* block
          comment mov r0, #99 */ mov r2, #3
       mov r3, #4; mov r4, #5 @ two statements""",
    [ Reg (R0, 1u); Reg (R1, 2u); Reg (R2, 3u); Reg (R3, 4u); Reg (R4, 5u) ]

    "several labels, label on its own line",
    """    b two
       one:
       two: three:
           mov r0, #7
           ldr r1, =one
           ldr r2, =three""",
    [ Reg (R0, 7u); Reg (R1, 4u); Reg (R2, 4u) ]

    "numeric local labels 1b / 1f, reused",
    """    mov r0, #0
           mov r1, #3
       1:  add r0, r0, #1
           subs r1, r1, #1
           bne 1b
           b 1f
           mov r0, #99
       1:  mov r2, #2
       2:  subs r2, r2, #1
           bne 2b""",
    [ Reg (R0, 3u); Reg (R2, 0u) ]

    "sections, .asciz, .bss",
    """    .text
           ldr r0, =msg
           ldrb r1, [r0]
           ldrb r2, [r0, #2]
           ldr r3, =buf
           ldr r4, [r3]
           .data
       msg: .asciz "hi"
           .bss
       buf: .space 8""",
    [ Reg (R1, uint32 'h'); Reg (R2, 0u); Reg (R4, 0u) ]

    "strings keep ; and @, escapes",
    """    ldr r0, =s
           ldrb r1, [r0, #1]
           ldrb r2, [r0, #3]
           ldrb r3, [r0, #4]
           .data
       s: .ascii "a;b@\n"
       e: .word e - s""",
    [ Reg (R1, uint32 ';'); Reg (R2, uint32 '@'); Reg (R3, 10u) ]

    ".byte / .hword packing, .align, forward .word refs",
    """    ldr r0, =b
           ldr r1, [r0]
           ldr r2, =w
           ldr r3, [r2]
           ldr r4, =tbl
           ldr r5, [r4, #4]
           .data
       b:  .byte 1, 2, 3, -1
           .hword 0x1234
           .align 2
       w:  .word 5
       tbl: .word w, b""",
    [ Reg (R1, 0xFF030201u); Reg (R3, 5u); Reg (R5, 0x200u) ]

    ".equ / .set / = and GNU expression operators",
    """    .equ SIZE, 1 << 4
           .set MASK, SIZE - 1
           LIMIT = (SIZE | 3) * 2
           mov r0, #MASK
           ldr r1, =LIMIT
           mov r2, #10 / 3
           mov r3, #10 % 3
           mvn r4, #~0xFF
           mov r5, #010
           mov r6, #'A'
           mov r7, #'\n'
           mov r8, #2 + 3 * 4""",
    [ Reg (R0, 15u); Reg (R1, 38u); Reg (R2, 3u); Reg (R3, 1u); Reg (R4, 0xFFu)
      Reg (R5, 8u); Reg (R6, 65u); Reg (R7, 10u); Reg (R8, 14u) ]

    "register aliases fp ip sl sb a1 v1",
    """    mov fp, #11
           mov ip, #12
           mov sl, #10
           mov sb, #9
           mov a1, #1
           mov v1, #4
           push {fp, ip}
           pop {a2, a3}""",
    [ Reg (R11, 11u); Reg (R12, 12u); Reg (R10, 10u); Reg (R9, 9u); Reg (R0, 1u); Reg (R4, 4u)
      Reg (R1, 11u); Reg (R2, 12u) ]

    ".align in .text pads with nops",
    """    mov r0, #1
           .align 3
       x:  mov r1, #2
           ldr r2, =x""",
    [ Reg (R0, 1u); Reg (R1, 2u); Reg (R2, 8u) ]

    "C-callable function with gcc boilerplate",
    """    .syntax unified
           .arch armv7-a
           .arm
           .text
           .global _start
       _start:
           ldr r0, =arr
           mov r1, #4
           bl sum_array
           b done

           .global sum_array
           .type sum_array, %function
       sum_array:              @ r0 = int *a, r1 = n
           .fnstart
           push {r4, lr}
           mov r2, #0
       1:  ldr r3, [r0], #4
           add r2, r2, r3
           subs r1, r1, #1
           bne 1b
           mov r0, r2
           pop {r4, pc}
           .fnend
           .size sum_array, .-sum_array
       done:
           .data
       arr: .word 10, 20, 30, -5
           .section .note.GNU-stack,"",%progbits""",
    [ Reg (R0, 55u); Reg (R13, initStackPointer) ]

    "instruction in .data rejected", ".data\nmov r0, #1", [ ParseError ]
    "DCD rejected (armasm)", "x: DCD 1", [ ParseError ]
    "EQU rejected (armasm)", "X EQU 4", [ ParseError ]
    "unknown directive rejected", ".frobnicate 3", [ ParseError ]
    "Thumb rejected", ".thumb", [ ParseError ]
    ".syntax divided rejected", ".syntax divided", [ ParseError ]
    "unterminated string rejected", ".data\ns: .ascii \"abc", [ ParseError ]
    ".byte out of range rejected", ".data\nb: .byte 300", [ ParseError ]
    "1b with no earlier 1: rejected", "b 1b", [ ParseError ]
    "1f with no later 1: rejected", "1: b 1f", [ ParseError ]
    "invalid label name rejected", "9x: mov r0, #1", [ ParseError ]
    "unterminated block comment rejected", "mov r0, #1 /* oops", [ ParseError ]
    "garbage after expression rejected", "mov r0, #3 4", [ ParseError ]
]

let branchTests = [
    "bx lr returns from a function",
    """_start:
           mov r0, #5
           bl double
           b done
       double:
           add r0, r0, r0
           bx lr
       done:
           mov r1, r0""",
    [ Reg (R0, 10u); Reg (R1, 10u) ]

    "nested calls: push {lr} / pop {lr} / bx lr",
    """    mov r0, #1
           bl f
           b done
       f:  push {lr}
           bl g
           add r0, r0, #10
           pop {lr}
           bx lr
       g:  add r0, r0, #100
           bx lr
       done:""",
    [ Reg (R0, 111u); Reg (R13, initStackPointer) ]

    "conditional bxeq / bxne",
    """    mov r0, #0
           bl f
           b done
       f:  cmp r0, #0
           movne r1, #1
           bxne lr
           mov r1, #2
           bxeq lr
           mov r1, #3
       done:""",
    [ Reg (R1, 2u) ]

    "bx to computed address, BX LR upper case",
    """    ldr r1, =target
           BX r1
           mov r0, #99
       target:
           mov r0, #7
           BL sub
           b done
       sub:
           BX LR
       done:""",
    [ Reg (R0, 7u) ]

    "bx to odd (Thumb) address stops", "ldr r1, =t + 1\nbx r1\nt: mov r0, #1", [ RunError ]
    "bx to halfword address stops", "ldr r1, =t + 2\nbx r1\nt: mov r0, #1", [ RunError ]
    "bx needs a register", "bx", [ ParseError ]
    "bx rejects a label", "t: bx t", [ ParseError ]
    "bx rejects an immediate", "bx #4", [ ParseError ]
    "bl to misaligned address rejected", "bl 0x102", [ ParseError ]
    "b to misaligned address rejected", "b 2", [ ParseError ]
    "bl to numeric address", "    b skip\n    mov r0, #1\nskip: bl 4\n", [ Reg (R0, 1u); Reg (R14, 12u) ]
    "blx is not implemented", "blx lr", [ ParseError ]
]

let topLevelReturnTests = [
    "LR starts as the return-to-caller address", "mov r0, lr", [ Reg (R0, returnToCaller) ]
    "top-level bx lr ends the program",
    """    mov r0, #1
           bx lr
           mov r0, #2""",
    [ Reg (R0, 1u); Reg (R15, returnToCaller) ]
    "top-level mov pc, lr ends the program",
    """    mov r0, #3
           mov pc, lr
           mov r0, #4""",
    [ Reg (R0, 3u); Reg (R15, returnToCaller) ]
    "top-level push {lr} ... pop {pc} ends the program",
    """    push {r4, lr}
           mov r4, #9
           mov r0, r4
           pop {r4, pc}
           mov r0, #5""",
    [ Reg (R0, 9u); Reg (R4, 0u); Reg (R13, initStackPointer); Reg (R15, returnToCaller) ]
    "function first in .text, called by nothing, returns cleanly",
    """    .global add3
       add3:
           add r0, r0, #3
           bx lr
       helper:
           mov r0, #99
           bx lr""",
    [ Reg (R0, 3u); Reg (R15, returnToCaller) ]
    "conditional top-level return",
    """    mov r0, #0
           cmp r0, #0
           bxne lr
           mov r1, #1
           bxeq lr
           mov r1, #2""",
    [ Reg (R1, 1u); Reg (R15, returnToCaller) ]
    "falling off the end still ends normally", "mov r0, #6", [ Reg (R0, 6u); Reg (R14, returnToCaller) ]
]

let halfwordTests = [
    "LDRH / LDRSH / LDRB / LDRSB from data",
    """    ldr r0, =d
           ldrh r1, [r0]
           ldrh r2, [r0, #2]
           ldrsh r3, [r0, #2]
           ldrb r4, [r0, #4]
           ldrsb r5, [r0, #4]
           ldrsb r6, [r0, #5]
           .data
       d:  .hword 0x1234, 0xFFFE
           .byte 0x80, 0x7F""",
    [ Reg (R1, 0x1234u); Reg (R2, 0xFFFEu); Reg (R3, 0xFFFFFFFEu); Reg (R4, 0x80u); Reg (R5, 0xFFFFFF80u); Reg (R6, 0x7Fu) ]

    "STRH stores the low halfword, little-endian",
    """    ldr r0, =buf
           ldr r1, =0xABCD1234
           strh r1, [r0, #2]
           ldr r2, [r0]
           strh r1, [r0]
           ldr r3, [r0]
           .data
       buf: .space 8""",
    [ Reg (R2, 0x12340000u); Reg (R3, 0x12341234u) ]

    "halfword addressing: post-index, pre-index writeback, register and negative offsets",
    """    ldr r0, =d
           ldrh r1, [r0], #2
           ldrsh r2, [r0, #2]!
           mov r4, #-2
           ldrh r3, [r0, r4]
           ldrh r5, [r0, #-4]
           .data
       d:  .hword 10, 20, 0xFFF0""",
    [ Reg (R1, 10u); Reg (R2, 0xFFFFFFF0u); Reg (R3, 20u); Reg (R5, 10u) ]

    "conditional and lower/upper case",
    """    ldr r0, =d
           cmp r0, r0
           LDRSHEQ r1, [r0]
           ldrshne r2, [r0]
           .data
       d:  .hword -5""",
    [ Reg (R1, 0xFFFFFFFBu); Reg (R2, 0u) ]

    "LDRHS is LDR with condition HS, not a halfword load",
    """    ldr r2, =d
           cmp r0, r0
           ldrhs r1, [r2]
           .data
       d:  .word 0x11223344""",
    [ Reg (R1, 0x11223344u) ]

    "halfword offset 255 accepted", "mov r1, #1\nldrh r0, [r1, #255]", [ Reg (R0, 0u) ]
    "halfword offset 256 rejected", "ldrh r0, [r1, #256]", [ ParseError ]
    "halfword shifted register offset rejected", "ldrsh r0, [r1, r2, lsl #1]", [ ParseError ]
    "STRSB rejected", "strsb r0, [r1]", [ ParseError ]
    "STRSH rejected", "strsh r0, [r1]", [ ParseError ]
    "LDRSB has no =value form", "ldrsb r0, =5", [ ParseError ]
]

/// ARMv7 Linux allows unaligned LDR, STR, LDRH, LDRSH and STRH (SCTLR.A = 0)
let unalignedTests = [
    "unaligned LDR, LDRH and LDRSH read little-endian bytes across words",
    """    ldr r0, =d
           ldr r1, [r0, #1]
           ldrh r2, [r0, #3]
           ldrsh r3, [r0, #7]
           .data
       d:  .word 0x44332211, 0x88776655, 0xFF""",
    [ Reg (R1, 0x55443322u); Reg (R2, 0x5544u); Reg (R3, 0xFFFFFF88u) ]

    "unaligned STR and STRH write little-endian bytes across words",
    """    ldr r0, =buf
           ldr r1, =0xAABBCCDD
           str r1, [r0, #3]
           ldr r2, [r0]
           ldr r3, [r0, #4]
           strh r1, [r0, #9]
           ldr r4, [r0, #8]
           .data
       buf: .space 12""",
    [ Reg (R2, 0xDD000000u); Reg (R3, 0x00AABBCCu); Reg (R4, 0x00CCDD00u) ]

    "LDM from an unaligned address stops the run", "ldr r0, =d + 2\nldmia r0, {r1, r2}\n.data\nd: .word 1, 2, 3", [ Expect.RunError ]
    "PUSH with an unaligned SP stops the run", "sub sp, sp, #2\npush {r0}", [ Expect.RunError ]
]

let addressingTests = [
    "word offset need not be a multiple of 4",
    """    ldr r0, =d + 2
           ldr r1, [r0, #2]
           .data
       d:  .word 11, 22""",
    [ Reg (R1, 22u) ]

    "LDRB / STRB offsets up to 4095",
    """    ldr r0, =d - 4000
           ldrb r1, [r0, #4000]
           mov r2, #0x5A
           strb r2, [r0, #4001]
           ldr r3, =d
           ldr r4, [r3]
           .data
       d:  .word 0x11223344""",
    [ Reg (R1, 0x44u); Reg (R4, 0x11225A44u) ]

    "word offset 4095 accepted", "ldr r0, [r1, #4092]\nldrb r2, [r1, #-4095]", [ Reg (R0, 0u) ]
    "word offset 4096 rejected", "ldr r0, [r1, #4096]", [ ParseError ]
    "byte offset -4096 rejected", "ldrb r0, [r1, #-4096]", [ ParseError ]

    "negative and explicitly positive register offsets",
    """    ldr r0, =d + 8
           mov r2, #8
           ldr r1, [r0, -r2]
           ldr r3, [r0, +r2]
           ldr r4, [r0], -r2
           .data
       d:  .word 1, 2, 3, 4, 5""",
    [ Reg (R1, 1u); Reg (R3, 5u); Reg (R4, 3u) ]

    "negative scaled register offset",
    """    ldr r0, =d + 12
           mov r2, #2
           ldr r1, [r0, -r2, lsl #2]
           .data
       d:  .word 1, 2, 3, 4""",
    [ Reg (R1, 2u) ]

    "ROR and RRX register offsets",
    """    ldr r0, =d
           mov r2, #8
           ldr r1, [r0, r2, ror #1]
           mov r3, #0
           cmp r3, #1
           ldr r4, [r0, r2, rrx]
           .data
       d:  .word 1, 2, 3""",
    [ Reg (R1, 2u); Reg (R4, 2u) ]

    "LSR #32 and ASR #32 register offsets",
    """    ldr r0, =d
           mov r2, #8
           ldr r1, [r0, r2, lsr #32]
           ldr r3, =d + 1
           mvn r2, #0
           ldrb r4, [r3, r2, asr #32]
           .data
       d:  .word 0x11223344""",
    [ Reg (R1, 0x11223344u); Reg (R4, 0x44u) ]

    "LSL #0 register offset", "ldr r0, =d\nmov r2, #4\nldr r1, [r0, r2, lsl #0]\n.data\nd: .word 7, 9", [ Reg (R1, 9u) ]
    "LSL #32 offset rejected", "ldr r0, [r1, r2, lsl #32]", [ ParseError ]
    "ROR #32 offset rejected", "ldr r0, [r1, r2, ror #32]", [ ParseError ]
    "LSR #33 offset rejected", "ldr r0, [r1, r2, lsr #33]", [ ParseError ]
    "unknown shift rejected", "ldr r0, [r1, r2, rol #2]", [ ParseError ]

    "halfword negative register offset",
    """    ldr r0, =d + 4
           mov r2, #4
           ldrsh r1, [r0, -r2]
           .data
       d:  .hword -7, 0""",
    [ Reg (R1, 0xFFFFFFF9u) ]
    "halfword RRX offset rejected", "ldrh r0, [r1, r2, rrx]", [ ParseError ]
]

/// Cortex-A7 (ARMv7-A) instructions: MOVW/MOVT, extends, long multiplies, MLS, NEG, NOP
let a7Tests = [
    "MOVW and MOVT build a 32-bit constant",
    "movw r0, #0x5678\nmovt r0, #0x1234\nmov r1, #-1\nmovt r1, #0", [ Reg (R0, 0x12345678u); Reg (R1, 0xFFFFu) ]
    "MOVW/MOVT with :lower16: and :upper16:",
    """    movw r0, #:lower16:big
           movt r0, #:upper16:big
           movw r1, #:lower16:d
           movt r1, #:upper16:d
           ldr r2, =d
           .equ big, 0xCAFEF00D
           .data
       d:  .word 0""",
    [ Reg (R0, 0xCAFEF00Du); Reg (R2, 0x200u); Reg (R1, 0x200u) ]
    "MOVW value over 16 bits rejected", "movw r0, #0x10000", [ ParseError ]
    "MOVWS rejected", "movws r0, #1", [ ParseError ]
    "MOVT to PC rejected", "movt pc, #1", [ ParseError ]
    "conditional MOVW", "cmp r0, #1\nmovweq r1, #5\nmovwne r2, #6", [ Reg (R1, 0u); Reg (R2, 6u) ]
    "MOV with a 16-bit value becomes MOVW", "mov r0, #0x1234\nmov r1, #65535", [ Reg (R0, 0x1234u); Reg (R1, 0xFFFFu) ]
    "MOVS with a non-ARM 16-bit value rejected", "movs r0, #0x1234", [ ParseError ]
    "MOV over 16 bits still rejected", "mov r0, #0x12345", [ ParseError ]

    "UXTB, UXTH, SXTB, SXTH",
    """    ldr r1, =0x1234F680
           uxtb r2, r1
           uxth r3, r1
           sxtb r4, r1
           sxth r5, r1
           sxtb r6, r1, ror #8
           uxtb r7, r1, ror #24""",
    [ Reg (R2, 0x80u); Reg (R3, 0xF680u); Reg (R4, 0xFFFFFF80u); Reg (R5, 0xFFFFF680u); Reg (R6, 0xFFFFFFF6u); Reg (R7, 0x12u) ]
    "extend rotation must be 8, 16 or 24", "uxtb r0, r1, ror #4", [ ParseError ]
    "extend has no S form", "uxtbs r0, r1", [ ParseError ]

    "MLS",
    "mov r1, #6\nmov r2, #7\nmov r3, #50\nmls r0, r1, r2, r3", [ Reg (R0, 8u) ]
    "UMULL and SMULL",
    """    mvn r2, #0
           mov r3, #2
           umull r0, r1, r2, r3
           smull r4, r5, r2, r3""",
    [ Reg (R0, 0xFFFFFFFEu); Reg (R1, 1u); Reg (R4, 0xFFFFFFFEu); Reg (R5, 0xFFFFFFFFu) ]
    "UMULL with large operands",
    "ldr r2, =0xDEADBEEF\nldr r3, =0xCAFEBABE\numull r0, r1, r2, r3", [ Reg (R0, 0x88CF5B62u); Reg (R1, 0xB092AB7Bu) ]
    "UMLAL and SMLAL accumulate into 64 bits",
    """    mvn r0, #0
           mov r1, #0
           mov r2, #1
           mov r3, #1
           umlal r0, r1, r2, r3
           mov r4, #5
           mov r5, #0
           mvn r6, #0
           smlal r4, r5, r6, r3""",
    [ Reg (R0, 0u); Reg (R1, 1u); Reg (R4, 4u); Reg (R5, 0u) ]
    "UMULLS sets N and Z from the 64-bit result",
    "mov r2, #0\nmov r3, #5\numulls r0, r1, r2, r3", [ Flags (false, true, false, false) ]
    "SMULLS negative sets N",
    "mvn r2, #0\nmov r3, #5\nsmulls r0, r1, r2, r3", [ Flags (true, false, false, false) ]
    "UMULL with RdLo = RdHi rejected", "umull r0, r0, r1, r2", [ ParseError ]

    "SDIV and UDIV round toward zero",
    """    mov r1, #7
           mov r2, #2
           sdiv r0, r1, r2
           mvn r3, #6
           sdiv r4, r3, r2
           udiv r5, r3, r2
           mov r6, #100
           udiv r6, r2""",
    [ Reg (R0, 3u); Reg (R4, 0xFFFFFFFDu); Reg (R5, 0x7FFFFFFCu); Reg (R6, 50u) ]
    "divide by zero gives 0", "mov r1, #5\nmov r2, #0\nsdiv r0, r1, r2\nudiv r3, r1, r2", [ Reg (R0, 0u); Reg (R3, 0u) ]
    "SDIV INT_MIN by -1 gives INT_MIN", "mov r1, #0x80000000\nmvn r2, #0\nsdiv r0, r1, r2", [ Reg (R0, 0x80000000u) ]
    "SDIV leaves the flags alone", "mov r1, #0\ncmp r1, #0\nmov r2, #3\nsdiv r0, r2, r2", [ Flags (false, true, true, false) ]
    "SDIVS rejected", "sdivs r0, r1, r2", [ ParseError ]
    "UDIV immediate rejected", "udiv r0, r1, #2", [ ParseError ]

    "NEG and NEGS", "mov r1, #7\nneg r0, r1\nnegs r2, r2", [ Reg (R0, 0xFFFFFFF9u); Reg (R2, 0u); Flags (false, true, true, false) ]
    "NOP does nothing", "mov r0, #3\nnop\nnopeq\nadd r0, #1", [ Reg (R0, 4u) ]
    "NOP takes no operands", "nop r0", [ ParseError ]
]

/// LDRD and STRD: an even/odd register pair, addressing mode 3, word-aligned address
let doubleTests = [
    "LDRD loads a register pair, low word first",
    """    ldr r2, =d
           ldrd r0, r1, [r2]
           ldrd r4, [r2, #4]
           .data
       d:  .word 0x11111111, 0x22222222, 0x33333333""",
    [ Reg (R0, 0x11111111u); Reg (R1, 0x22222222u); Reg (R4, 0x22222222u); Reg (R5, 0x33333333u) ]
    "STRD stores a register pair",
    """    ldr r2, =buf
           ldr r4, =0xAAAA0001
           ldr r5, =0xBBBB0002
           strd r4, r5, [r2, #4]
           ldr r0, [r2, #4]
           ldr r1, [r2, #8]
           ldr r3, [r2]
           .data
       buf: .space 12""",
    [ Reg (R0, 0xAAAA0001u); Reg (R1, 0xBBBB0002u); Reg (R3, 0u) ]
    "STRD and LDRD with pre-index write-back and post-index (saving a pair on the stack)",
    """    mov r4, #4
           mov r5, #5
           strd r4, r5, [sp, #-8]!
           mov r6, sp
           mov r4, #0
           mov r5, #0
           ldrd r4, r5, [sp], #8""",
    [ Reg (R4, 4u); Reg (R5, 5u); Reg (R6, 0xFEFFFFF8u); Reg (R13, 0xFF000000u) ]
    "LDRD with a register offset", "ldr r2, =d\nmov r3, #8\nldrd r0, r1, [r2, -r3]\n.data\n.word 7, 9\nd: .word 0", [ Reg (R0, 7u); Reg (R1, 9u) ]
    "conditional LDRD", "ldr r2, =d\ncmp r2, #0\nldrdeq r0, [r2]\nldrdne r4, [r2]\n.data\nd: .word 1, 2", [ Reg (R0, 0u); Reg (R4, 1u); Reg (R5, 2u) ]
    "LDRD odd first register rejected", "ldrd r1, r2, [r0]", [ ParseError ]
    "LDRD non-consecutive pair rejected", "ldrd r0, r2, [r3]", [ ParseError ]
    "LDRD lr pair rejected", "ldrd lr, pc, [r0]", [ ParseError ]
    "LDRD offset 256 rejected", "ldrd r0, r1, [r2, #256]", [ ParseError ]
    "LDRD shifted register offset rejected", "ldrd r0, r1, [r2, r3, lsl #2]", [ ParseError ]
    "LDRD write-back to a loaded register rejected", "ldrd r0, r1, [r1, #8]!", [ ParseError ]
    "LDRSD rejected", "ldrsd r0, r1, [r2]", [ ParseError ]
    "LDRD from an unaligned address stops the run", "ldr r2, =d + 2\nldrd r0, r1, [r2]\n.data\nd: .word 1, 2, 3", [ Expect.RunError ]
    "STRD to an unaligned address stops the run", "ldr r2, =d + 1\nstrd r0, r1, [r2]\n.data\nd: .word 1, 2, 3", [ Expect.RunError ]
]

/// bit-field instructions, CLZ, RBIT and the REV family
let bitTests = [
    "UBFX, SBFX, BFI, BFC",
    """    ldr r1, =0x12345678
           ubfx r0, r1, #8, #8
           sbfx r2, r1, #4, #4
           sbfx r3, r1, #12, #4
           mov r4, r1
           mov r5, #0xA
           bfi r4, r5, #12, #4
           mov r6, r4
           bfc r6, #0, #8""",
    [ Reg (R0, 0x56u); Reg (R2, 7u); Reg (R3, 5u); Reg (R4, 0x1234A678u); Reg (R6, 0x1234A600u) ]
    "SBFX sign-extends a negative field", "ldr r1, =0x0000F000\nsbfx r0, r1, #12, #4\nsbfx r2, r1, #13, #3", [ Reg (R0, 0xFFFFFFFFu); Reg (R2, 0xFFFFFFFFu) ]
    "full-width and top fields",
    """    ldr r1, =0x80000001
           ubfx r0, r1, #0, #32
           ubfx r2, r1, #31, #1
           sbfx r3, r1, #31, #1
           mvn r4, #0
           bfc r4, #0, #32
           mvn r5, #0
           bfi r5, r1, #16, #16""",
    [ Reg (R0, 0x80000001u); Reg (R2, 1u); Reg (R3, 0xFFFFFFFFu); Reg (R4, 0u); Reg (R5, 0x0001FFFFu) ]
    "bit field past bit 31 rejected", "ubfx r0, r1, #28, #5", [ ParseError ]
    "bit field width 0 rejected", "bfi r0, r1, #4, #0", [ ParseError ]
    "bit field lsb 32 rejected", "bfc r0, #32, #1", [ ParseError ]
    "bit field needs # constants", "ubfx r0, r1, r2, #4", [ ParseError ]
    "UBFXS rejected", "ubfxs r0, r1, #0, #4", [ ParseError ]
    "CLZ",
    "mov r1, #0x100\nclz r0, r1\nmov r2, #0\nclz r3, r2\nmvn r4, #0\nclz r5, r4\nmov r6, #1\nclz r7, r6",
    [ Reg (R0, 23u); Reg (R3, 32u); Reg (R5, 0u); Reg (R7, 31u) ]
    "RBIT, and RBIT + CLZ counts trailing zeros",
    "ldr r1, =0x00000001\nrbit r0, r1\nldr r2, =0x12345678\nrbit r3, r2\nmov r4, #0x50\nrbit r5, r4\nclz r5, r5",
    [ Reg (R0, 0x80000000u); Reg (R3, 0x1E6A2C48u); Reg (R5, 4u) ]
    "REV, REV16, REVSH",
    "ldr r1, =0x11223344\nrev r0, r1\nrev16 r2, r1\nrevsh r3, r1\nldr r4, =0x000080FF\nrevsh r5, r4",
    [ Reg (R0, 0x44332211u); Reg (R2, 0x22114433u); Reg (R3, 0x4433u); Reg (R5, 0xFFFFFF80u) ]
    "conditional REV and CLZ flags unchanged", "mov r1, #1\ncmp r1, #1\nrevne r0, r1\nclzeq r2, r1", [ Reg (R0, 0u); Reg (R2, 31u); Flags (false, true, true, false) ]
    "CLZ with PC rejected", "clz r0, pc", [ ParseError ]
    "REVS rejected", "revs r0, r1", [ ParseError ]
]

/// VFP (VFPv4, D32), MRS and MSR
let vfpTests = [
    "vmov immediate and vadd.f32", "vmov.f32 s0, #1.5\nvmov.f32 s1, #2.25\nvadd.f32 s2, s0, s1", [ VW (2, 0x40700000u); Fpscr 0u ]
    "single precision rounds: 0.1f + 0.2f",
    """    ldr r0, =d
           vldr s0, [r0]
           vldr.32 s1, [r0, #4]
           vadd.f32 s2, s0, s1
           vstr s2, [r0, #8]
           ldr r1, [r0, #8]
           .data
       d:  .float 0.1, 0.2
           .word 0""",
    [ VW (2, 0x3E99999Au); Reg (R1, 0x3E99999Au); Fpscr 0x10u ]
    "double precision, and d registers overlap s registers",
    """    ldr r0, =d
           vldr d0, [r0]
           vldr.f64 d1, [r0, #8]
           vadd.f64 d2, d0, d1
           .data
       d:  .double 0.1, 0.2""",
    [ DW (2, 0x3FD33333u, 0x33333334u); VW (4, 0x33333334u); VW (5, 0x3FD33333u) ]
    "d16-d31 are separate from s registers", "vmov.f64 d16, #1.0\nvmov.f64 d31, #-2.0", [ DW (16, 0x3FF00000u, 0u); DW (31, 0xC0000000u, 0u); VW (0, 0u) ]
    "vmov between core and VFP registers",
    """    ldr r0, =0x12345678
           ldr r2, =0xAAAA0000
           ldr r3, =0xBBBB0000
           vmov s3, r0
           vmov r1, s3
           vmov d5, r2, r3
           vmov r4, r5, d5
           vmov s6, s7, r2, r3
           vmov.32 d6[1], r0
           vmov r6, d6[1]
           vmov r7, r8, s6, s7""",
    [ VW (3, 0x12345678u); Reg (R1, 0x12345678u); DW (5, 0xBBBB0000u, 0xAAAA0000u); Reg (R4, 0xAAAA0000u); Reg (R5, 0xBBBB0000u)
      VW (6, 0xAAAA0000u); VW (13, 0x12345678u); Reg (R6, 0x12345678u); Reg (R7, 0xAAAA0000u); Reg (R8, 0xBBBB0000u) ]
    "double arithmetic",
    """    vmov.f64 d0, #2.5
           vmov.f64 d1, #4.0
           vmov.f64 d6, #1.0
           vmov.f64 d7, #3.0
           vsub.f64 d2, d0, d6
           vmul.f64 d3, d0, d1
           vdiv.f64 d4, d6, d7
           vmov.f64 d8, #2.0
           vsqrt.f64 d5, d8
           vneg.f64 d9, d0
           vabs.f64 d10, d9
           vnmul.f64 d11, d0, d1
           vadd.f64 d0, d6""",
    [ DW (2, 0x3FF80000u, 0u); DW (3, 0x40240000u, 0u); DW (4, 0x3FD55555u, 0x55555555u); DW (5, 0x3FF6A09Eu, 0x667F3BCDu)
      DW (9, 0xC0040000u, 0u); DW (10, 0x40040000u, 0u); DW (11, 0xC0240000u, 0u); DW (0, 0x400C0000u, 0u) ]
    "fused multiply-add rounds once, vmla twice",
    """    ldr r0, =d
           vldr d0, [r0]
           vmov.f64 d1, #10.0
           vmov.f64 d2, #-1.0
           vmov.f64 d3, d2
           vmla.f64 d3, d0, d1
           vmov.f64 d4, d2
           vfma.f64 d4, d0, d1
           .data
       d:  .double 0.1""",
    [ DW (3, 0u, 0u); DW (4, 0x3C900000u, 0u) ]
    "multiply-accumulate family",
    """    vmov.f32 s0, #2.0
           vmov.f32 s1, #3.0
           vmov.f32 s10, #10.0
           vmov.f32 s2, s10
           vmls.f32 s2, s0, s1
           vmov.f32 s3, s10
           vnmla.f32 s3, s0, s1
           vmov.f32 s4, s10
           vnmls.f32 s4, s0, s1
           vmov.f32 s5, s10
           vfms.f32 s5, s0, s1
           vmov.f32 s6, s10
           vfnma.f32 s6, s0, s1
           vmov.f32 s7, s10
           vfnms.f32 s7, s0, s1""",
    [ VW (2, 0x40800000u); VW (3, 0xC1800000u); VW (4, 0xC0800000u); VW (5, 0x40800000u); VW (6, 0xC1800000u); VW (7, 0xC0800000u) ]
    "vcmp, vmrs APSR_nzcv and a conditional branch",
    """    vmov.f32 s0, #1.0
           vmov.f32 s1, #2.0
           vcmp.f32 s0, s1
           vmrs APSR_nzcv, fpscr
           movlt r0, #1
           movgt r1, #1
           blt done
           mov r2, #1
       done:""",
    [ Reg (R0, 1u); Reg (R1, 0u); Reg (R2, 0u); Flags (true, false, false, false); Fpscr 0x80000000u ]
    "unordered compare after 0/0, vcmp with #0, fmstat",
    """    vsub.f32 s0, s0, s0
           vdiv.f32 s2, s0, s0
           vcmp.f32 s2, s0
           vmrs APSR_nzcv, fpscr
           movvs r2, #1
           vmov.f32 s3, #1.0
           vcmpe.f32 s3, #0
           fmstat
           movgt r3, #1""",
    [ VW (2, 0x7FC00000u); Reg (R2, 1u); Reg (R3, 1u); Fpscr 0x20000001u ]
    "conversions",
    """    vmov.f32 s0, #-2.5
           vcvt.s32.f32 s1, s0
           vcvtr.s32.f32 s2, s0
           vcvt.f64.f32 d2, s0
           vcvt.f32.f64 s6, d2
           mvn r0, #6
           vmov s7, r0
           vcvt.f32.s32 s8, s7
           ldr r1, =3000000000
           vmov s9, r1
           vcvt.f64.u32 d5, s9
           vcvt.u32.f64 s12, d5
           vmov.f32 s13, #1.5
           vcvt.s32.f32 s13, s13, #16
           vcvt.f32.u16 s14, s14, #4""",
    [ VW (1, 0xFFFFFFFEu); VW (2, 0xFFFFFFFEu); DW (2, 0xC0040000u, 0u); VW (6, 0xC0200000u); VW (8, 0xC0E00000u)
      DW (5, 0x41E65A0Bu, 0xC0000000u); VW (12, 3000000000u); VW (13, 0x00018000u); VW (14, 0u) ]
    "half precision with vcvtb and vcvtt",
    """    vmov.f32 s0, #1.5
           vmov.f32 s2, #-2.0
           vcvtb.f16.f32 s1, s0
           vcvtt.f16.f32 s1, s2
           vcvtt.f32.f16 s3, s1
           vcvtb.f32.f16 s4, s1""",
    [ VW (1, 0xC0003E00u); VW (3, 0xC0000000u); VW (4, 0x3FC00000u) ]
    "vpush, vpop, vstm and vldm",
    """    vmov.f64 d8, #1.0
           vmov.f64 d9, #2.0
           vpush {d8-d9}
           mov r5, sp
           vmov.f64 d8, #0.5
           vmov.f64 d9, #0.5
           vpop {d8, d9}
           ldr r0, =buf
           vmov.f32 s0, #1.0
           vmov.f32 s1, #2.0
           vmov.f32 s2, #3.0
           vstmia r0!, {s0-s2}
           vldmdb r0!, {s4-s6}
           mov r1, r0
           ldr r2, =buf
           .data
       buf: .space 16""",
    [ DW (8, 0x3FF00000u, 0u); DW (9, 0x40000000u, 0u); Reg (R5, 0xFEFFFFF0u); Reg (R13, 0xFF000000u)
      VW (4, 0x3F800000u); VW (5, 0x40000000u); VW (6, 0x40400000u) ]
    "vmsr sets the rounding mode; flags accumulate",
    """    mov r0, #0x00C00000
           vmsr fpscr, r0
           vmov.f32 s0, #1.0
           vmov.f32 s1, #3.0
           vdiv.f32 s2, s0, s1
           vsub.f32 s3, s3, s3
           vdiv.f32 s4, s0, s3
           vmrs r1, fpscr""",
    [ VW (2, 0x3EAAAAAAu); VW (4, 0x7F800000u); Reg (R1, 0x00C00012u) ]
    "mrs and msr",
    """    mov r0, #0
           cmp r0, #0
           mrs r1, apsr
           msr APSR_nzcvq, #0x80000000
           mrs r2, cpsr
           mov r3, #0x30000000
           msr cpsr_f, r3""",
    [ Reg (R1, 0x60000000u); Reg (R2, 0x80000000u); Flags (false, false, true, true) ]
    ".single and .double directives",
    "ldr r0, =d\nldr r1, [r0]\nldr r2, [r0, #4]\nldr r3, [r0, #8]\nldr r4, [r0, #12]\n.data\nd: .single 1.5\n.float -2\n.double -0.0",
    [ Reg (R1, 0x3FC00000u); Reg (R2, 0xC0000000u); Reg (R3, 0u); Reg (R4, 0x80000000u) ]
    "vldr from a nearby label", "vldr s0, k\nvldr d1, k2\n.data\nk: .float 1.5\n.align 3\nk2: .double 2.0", [ VW (0, 0x3FC00000u); DW (1, 0x40000000u, 0u) ]
    "vldr from a label too far away rejected", "vldr s0, k\n.data\n.space 2000\nk: .float 1.5", [ ParseError ]
    "two-operand arithmetic form", "vmov.f32 s0, #2.0\nvmov.f32 s1, #3.0\nvmul.f32 s0, s1", [ VW (0, 0x40C00000u) ]
    "conditional VFP instruction", "mov r0, #1\ncmp r0, #2\nvmoveq.f32 s0, #1.0\nvmovne.f32 s1, #1.0", [ VW (0, 0u); VW (1, 0x3F800000u) ]
    "NEON: .f32 on d registers rejected", "vadd.f32 d0, d1, d2", [ ParseError ]
    "NEON: integer vector type rejected", "vadd.i32 d0, d1, d2", [ ParseError ]
    "NEON: q registers rejected", "vmov q0, q1", [ ParseError ]
    "NEON: vld1 rejected", "vld1.32 {d0}, [r0]", [ ParseError ]
    ".f64 on s registers rejected", "vadd.f64 s0, s1, s2", [ ParseError ]
    "vadd without a type or registers rejected", "vadd r0, r1, r2", [ ParseError ]
    "vmov immediate that cannot be encoded rejected", "vmov.f32 s0, #0.1", [ ParseError ]
    "vmov immediate zero rejected", "vmov.f64 d0, #0", [ ParseError ]
    "vldr offset not a multiple of 4 rejected", "vldr s0, [r0, #2]", [ ParseError ]
    "vldr register offset rejected", "vldr s0, [r0, r1]", [ ParseError ]
    "vldr write-back rejected", "vldr d0, [r0, #8]!", [ ParseError ]
    "non-consecutive vpush list rejected", "vpush {d0, d2}", [ ParseError ]
    "mixed vpush list rejected", "vpush {s0, d1}", [ ParseError ]
    "more than 16 d registers rejected", "vpush {d0-d16}", [ ParseError ]
    "vcvt with the wrong register kind rejected", "vcvt.s32.f32 s0, d0", [ ParseError ]
    "vmov to the same two core registers rejected", "vmov r0, r0, d0", [ ParseError ]
    "vmov s pair must be consecutive", "vmov s0, s2, r0, r1", [ ParseError ]
    "vmov with PC rejected", "vmov pc, s0", [ ParseError ]
    "vmsr fpexc rejected", "vmsr fpexc, r0", [ ParseError ]
    "vmrs fpsid rejected", "vmrs r0, fpsid", [ ParseError ]
    "msr without a field rejected", "msr apsr, r0", [ ParseError ]
    "mrs spsr rejected", "mrs r0, spsr", [ ParseError ]
    "fixed-point vcvt needs the same register", "vcvt.s32.f32 s0, s1, #16", [ ParseError ]
    "fixed-point fraction bits out of range", "vcvt.s32.f32 s0, s0, #33", [ ParseError ]
    ".float with a bad number rejected", ".data\n.float 1.2.3", [ ParseError ]
    "unaligned vldr stops the run", "mov r0, #2\nvldr s0, [r0]", [ Expect.RunError ]
    "unaligned vpush stops the run", "sub sp, sp, #2\nvpush {d0}", [ Expect.RunError ]
    "short vector mode stops the run", "mov r0, #0x10000\nvmsr fpscr, r0", [ Expect.RunError ]
]

let allTests = vfpTests @ bitTests @ doubleTests @ a7Tests @ unalignedTests @ tests @ mulTests @ shiftTests @ noFloatTests @ stackTests @ pushPreDecTests @ labelTests @ gnuSyntaxTests @ branchTests @ topLevelReturnTests @ halfwordTests @ addressingTests

// ///////////////////////////// C calls ///////////////////////////////////////

open CCall

let srcLines (code : string) = code.Split('\n') |> Array.map (fun l -> l.Trim()) |> Array.toList

/// assemble code, find its first "@ C:" prototype, call it with argument texts
let cCall (code : string) (argTexts : string list) =
    let lines = srcLines code
    let lim = reLoadProgram lines
    match findPrototypes lines with
    | Ok proto :: _ ->
        let typeOf i = match List.tryItem i proto.Params with | Some p -> p.PType | None -> CIntT(CInt32 true)
        let args = argTexts |> List.mapi (fun i t -> parseArgText (typeOf i) t)
        match args |> List.choose (function | Error e -> Some e | _ -> None) with
        | [] -> callFunction lim lines proto (args |> List.map (function | Ok a -> a | Error _ -> failwith "?")) 100000L
                |> Result.mapError (String.concat "; ")
        | errs -> Error(String.concat "; " errs)
    | Error(_, e) :: _ -> Error("prototype: " + e)
    | [] -> Error "no prototype"

let expectEq what (actual : 'a) (expected : 'a) =
    if actual = expected then [] else [ sprintf "%s = %A, expected %A" what actual expected ]

let returns code args (expected : int64) (problems : int) =
    match cCall code args with
    | Ok(setup, res) ->
        expectEq "outcome" res.Outcome Returned
        @ expectEq "return value" res.ReturnValue (Some expected)
        @ expectEq "number of problems" res.Problems.Length problems
        @ (if res.Problems.Length <> problems then [ sprintf "problems: %A" res.Problems ] else [])
    | Error e -> [ "call failed: " + e ]

let callFails code args (fragment : string) =
    match cCall code args with
    | Error e when e.Contains fragment -> []
    | Error e -> [ sprintf "error '%s' does not mention '%s'" e fragment ]
    | Ok(_, res) -> [ sprintf "expected an error, got %A" res.Outcome ]

let hasProblem code args (fragment : string) =
    match cCall code args with
    | Ok(_, res) when res.Problems |> List.exists (fun p -> p.Contains fragment) -> []
    | Ok(_, res) -> [ sprintf "no problem mentioning '%s' in %A" fragment res.Problems ]
    | Error e -> [ "call failed: " + e ]

let protoIs txt (expected : string) =
    match parsePrototype 1 txt with
    | Ok p ->
        let shown = sprintf "%s %s(%s)" (typeName p.Ret) p.FName (p.Params |> List.map (fun q -> typeName q.PType + " " + q.PName) |> String.concat ", ")
        expectEq "prototype" shown expected
    | Error e -> [ "parse failed: " + e ]

let protoFails txt (fragment : string) =
    match parsePrototype 1 txt with
    | Error e when e.Contains fragment -> []
    | Error e -> [ sprintf "error '%s' does not mention '%s'" e fragment ]
    | Ok p -> [ sprintf "expected an error, parsed %A" p ]

let sumArray = """@ C: extern int sum_array(const int *a, int n);
    .global sum_array
sum_array:
    mov r2, #0
1:  ldr r3, [r0], #4
    add r2, r2, r3
    subs r1, r1, #1
    bne 1b
    mov r0, r2
    bx lr"""

/// cycles taken to run code to completion
let cycles (code : string) =
    let lim = reLoadProgram (srcLines code)
    let fl = { N = false; Z = false; C = false; V = false }
    let ri = asmStep 100000L (getRunInfoFromImageWithInits NoBreak lim initialRegMap fl Map.empty lim.Mem)
    ri.CyclesDone

/// run code with strict alignment on
let runStrict (code : string) =
    Memory.strictAlignment <- true
    try run code finally Memory.strictAlignment <- false

let alignTests : (string * (unit -> string list)) list = [
    "unaligned LDR takes one extra cycle", fun () ->
        let aligned = cycles "ldr r0, =d\nldr r1, [r0]\n.data\nd: .word 1, 2"
        let unaligned = cycles "ldr r0, =d\nldr r1, [r0, #1]\n.data\nd: .word 1, 2"
        expectEq "extra cycles" (unaligned - aligned) 1L
    "strict alignment: unaligned LDR, LDRH, STR, STRH stop the run", fun () ->
        [ "ldr r1, [r0, #1]"; "ldrh r1, [r0, #1]"; "ldrsh r1, [r0, #3]"; "str r1, [r0, #2]"; "strh r1, [r0, #1]" ]
        |> List.collect (fun ins ->
            match runStrict ("ldr r0, =d\n" + ins + "\n.data\nd: .word 0, 0") with
            | Error e when e.StartsWith "run error" -> []
            | r -> [ sprintf "%s: expected a run error, got %A" ins r ])
    "strict alignment: aligned and byte accesses still work", fun () ->
        match runStrict "ldr r0, =d\nldrh r1, [r0, #2]\nldrb r2, [r0, #1]\nldr r3, [r0]\n.data\nd: .word 0x44332211" with
        | Ok dp -> expectEq "r1, r2, r3" (dp.Regs.[R1], dp.Regs.[R2], dp.Regs.[R3]) (0x4433u, 0x22u, 0x44332211u)
        | Error e -> [ e ]
]

// ///////////////////////////// floating point core ////////////////////////////

open FpMath

let f32 (w : uint32) = rawOfWord w
let f64 (hi : uint32) (lo : uint32) = rawOfWords lo hi
let lit fmt txt = match parseFloat fmt txt with | Ok r -> r | Error e -> failwith e
let isRes what (r : FpRes) (bits : bigint) (flags : uint32) =
    expectEq (what + " bits") (r.Bits.ToString("X")) (bits.ToString("X")) @ expectEq (what + " flags") r.Flags flags

let fpTests : (string * (unit -> string list)) list = [
    "fp: single rounding 0.1f + 0.2f", fun () ->
        isRes "sum" (add defaultCtl F32 (lit F32 "0.1") (lit F32 "0.2")) (f32 0x3E99999Au) IXC
    "fp: 1/3, sqrt 2, 0.1 + 0.2 in double", fun () ->
        isRes "1/3f" (div defaultCtl F32 (lit F32 "1") (lit F32 "3")) (f32 0x3EAAAAABu) IXC
        @ isRes "sqrt2f" (sqrt defaultCtl F32 (lit F32 "2")) (f32 0x3FB504F3u) IXC
        @ isRes "1/3d" (div defaultCtl F64 (lit F64 "1") (lit F64 "3")) (f64 0x3FD55555u 0x55555555u) IXC
        @ isRes "sqrt2d" (sqrt defaultCtl F64 (lit F64 "2")) (f64 0x3FF6A09Eu 0x667F3BCDu) IXC
        @ isRes "0.1+0.2d" (add defaultCtl F64 (lit F64 "0.1") (lit F64 "0.2")) (f64 0x3FD33333u 0x33333334u) IXC
        @ isRes "exact" (mul defaultCtl F32 (lit F32 "1.5") (lit F32 "2")) (f32 0x40400000u) 0u
    "fp: special values and flags", fun () ->
        isRes "0/0" (div defaultCtl F32 (f32 0u) (f32 0u)) (f32 0x7FC00000u) IOC
        @ isRes "1/0" (div defaultCtl F32 (lit F32 "1") (f32 0x80000000u)) (f32 0xFF800000u) DZC
        @ isRes "inf-inf" (sub defaultCtl F64 (infBits F64 false) (infBits F64 false)) (f64 0x7FF80000u 0u) IOC
        @ isRes "sqrt -1" (sqrt defaultCtl F32 (lit F32 "-1")) (f32 0x7FC00000u) IOC
        @ isRes "sqrt -0" (sqrt defaultCtl F32 (f32 0x80000000u)) (f32 0x80000000u) 0u
        @ isRes "overflow" (mul defaultCtl F32 (lit F32 "3e38") (lit F32 "10")) (f32 0x7F800000u) (OFC ||| IXC)
        @ isRes "overflow RZ" (mul { defaultCtl with Mode = RZ } F32 (lit F32 "3e38") (lit F32 "10")) (f32 0x7F7FFFFFu) (OFC ||| IXC)
        @ isRes "underflow" (div defaultCtl F32 (f32 1u) (lit F32 "2")) (f32 0u) (UFC ||| IXC)
        @ isRes "subnormal exact" (div defaultCtl F32 (f32 2u) (lit F32 "2")) (f32 1u) 0u
        @ isRes "FZ flushes input" (add { defaultCtl with FZ = true } F32 (f32 1u) (f32 0u)) (f32 0u) IDC
        @ isRes "x + -x" (add defaultCtl F32 (lit F32 "1") (lit F32 "-1")) (f32 0u) 0u
        @ isRes "x + -x RM" (add { defaultCtl with Mode = RM } F32 (lit F32 "1") (lit F32 "-1")) (f32 0x80000000u) 0u
    "fp: NaN propagation", fun () ->
        isRes "sNaN" (add defaultCtl F32 (lit F32 "1") (f32 0x7F800001u)) (f32 0x7FC00001u) IOC
        @ isRes "first qNaN" (add defaultCtl F32 (f32 0xFFC00005u) (f32 0x7FC00009u)) (f32 0xFFC00005u) 0u
        @ isRes "sNaN beats qNaN" (mul defaultCtl F32 (f32 0x7FC00009u) (f32 0xFF800003u)) (f32 0xFFC00003u) IOC
        @ isRes "DN" (add { defaultCtl with DN = true } F32 (f32 0xFFC00005u) (lit F32 "1")) (f32 0x7FC00000u) 0u
    "fp: fused multiply-add rounds once", fun () ->
        isRes "fma" (mulAdd defaultCtl F64 (lit F64 "-1") (lit F64 "0.1") (lit F64 "10")) (f64 0x3C900000u 0u) 0u
        @ isRes "unfused" (add defaultCtl F64 (lit F64 "-1") (mul defaultCtl F64 (lit F64 "0.1") (lit F64 "10")).Bits) (f64 0u 0u) 0u
        @ isRes "0 x inf + qNaN" (mulAdd defaultCtl F32 (f32 0x7FC00001u) (f32 0u) (infBits F32 false)) (f32 0x7FC00000u) IOC
    "fp: compare", fun () ->
        expectEq "eq" (compare defaultCtl F32 false (f32 0u) (f32 0x80000000u)) (0x60000000u, 0u)
        @ expectEq "lt" (compare defaultCtl F32 false (lit F32 "-2") (lit F32 "1")) (0x80000000u, 0u)
        @ expectEq "gt" (compare defaultCtl F64 false (lit F64 "1e300") (lit F64 "-inf")) (0x20000000u, 0u)
        @ expectEq "lt same sign" (compare defaultCtl F64 false (lit F64 "-3") (lit F64 "-2.5")) (0x80000000u, 0u)
        @ expectEq "unordered" (compare defaultCtl F32 false (f32 0x7FC00000u) (f32 0u)) (0x30000000u, 0u)
        @ expectEq "vcmpe unordered" (compare defaultCtl F32 true (f32 0x7FC00000u) (f32 0u)) (0x30000000u, IOC)
    "fp: conversions", fun () ->
        isRes "f32->f64" (convert defaultCtl F32 F64 (lit F32 "0.1")) (f64 0x3FB99999u 0xA0000000u) 0u
        @ isRes "f64->f32" (convert defaultCtl F64 F32 (lit F64 "0.1")) (f32 0x3DCCCCCDu) IXC
        @ isRes "f32->f16" (convert defaultCtl F32 F16 (lit F32 "1.5")) (rawOfWord 0x3E00u) 0u
        @ isRes "sNaN f32->f64" (convert defaultCtl F32 F64 (f32 0x7F800001u)) (f64 0x7FF80000u 0x20000000u) IOC
        @ expectEq "-1.5 to s32" (toFixed defaultCtl F32 true 32 0 false (lit F32 "-1.5")) (0xFFFFFFFFu, IXC)
        @ expectEq "2.5 RN to s32" (toFixed defaultCtl F32 true 32 0 true (lit F32 "2.5")) (2u, IXC)
        @ expectEq "3e9 to s32" (toFixed defaultCtl F64 true 32 0 false (lit F64 "3e9")) (0x7FFFFFFFu, IOC)
        @ expectEq "3e9 to u32" (toFixed defaultCtl F64 false 32 0 false (lit F64 "3e9")) (3000000000u, 0u)
        @ expectEq "-1 to u32" (toFixed defaultCtl F32 false 32 0 false (lit F32 "-1")) (0u, IOC)
        @ expectEq "nan to s32" (toFixed defaultCtl F32 true 32 0 false (f32 0x7FC00000u)) (0u, IOC)
        @ expectEq "1.25 to s16 fixed 4" (toFixed defaultCtl F32 true 16 4 false (lit F32 "-1.25")) (0xFFFFFFECu, 0u)
        @ isRes "s32 to f32" (ofFixed defaultCtl F32 true 32 0 0xFFFFFFFFu) (lit F32 "-1") 0u
        @ isRes "u32 to f32 inexact" (ofFixed defaultCtl F32 false 32 0 0xFFFFFFFFu) (f32 0x4F800000u) IXC
        @ isRes "fixed 16.16 to f64" (ofFixed defaultCtl F64 true 32 16 0x00018000u) (lit F64 "1.5") 0u
    "fp: literals and immediates", fun () ->
        expectEq "1.5" (lit F32 "1.5") (f32 0x3FC00000u)
        @ expectEq "0.1d" (lit F64 "0.1") (f64 0x3FB99999u 0x9999999Au)
        @ expectEq "pi" (lit F32 "3.14159265358979") (f32 0x40490FDBu)
        @ expectEq "hex" (lit F32 "0x1.8p1") (f32 0x40400000u)
        @ expectEq "-0" (lit F32 "-0") (f32 0x80000000u)
        @ expectEq "1e39" (lit F32 "1e39") (f32 0x7F800000u)
        @ expectEq ".5" (lit F64 ".5") (lit F64 "0.5")
        @ expectEq "bad" (parseFloat F32 "1.2.3" |> Result.isError) true
        @ expectEq "imm 1.0" (isVfpImmediate F32 (lit F32 "1.0")) true
        @ expectEq "imm 31" (isVfpImmediate F64 (lit F64 "31")) true
        @ expectEq "imm 0.125" (isVfpImmediate F32 (lit F32 "0.125")) true
        @ expectEq "imm 0.1" (isVfpImmediate F32 (lit F32 "0.1")) false
        @ expectEq "imm 0" (isVfpImmediate F32 (lit F32 "0")) false
        @ expectEq "imm 32" (isVfpImmediate F32 (lit F32 "32")) false
    "fp: display", fun () ->
        expectEq "0.1f" (format F32 (lit F32 "0.1")) "0.1"
        @ expectEq "1/3f" (format F32 (div defaultCtl F32 (lit F32 "1") (lit F32 "3")).Bits) "0.33333334"
        @ expectEq "0.3d" (format F64 (add defaultCtl F64 (lit F64 "0.1") (lit F64 "0.2")).Bits) "0.30000000000000004"
        @ expectEq "3" (format F32 (lit F32 "3")) "3.0"
        @ expectEq "123456" (format F64 (lit F64 "123456")) "123456.0"
        @ expectEq "-1.5e-7" (format F32 (lit F32 "-1.5e-7")) "-1.5e-7"
        @ expectEq "1e20" (format F64 (lit F64 "1e20")) "1e20"
        @ expectEq "0.00125" (format F64 (lit F64 "0.00125")) "0.00125"
        @ expectEq "inf" (format F32 (infBits F32 true)) "-inf"
        @ expectEq "nan" (format F64 (defaultNaN F64)) "nan"
        @ expectEq "-0" (format F32 (f32 0x80000000u)) "-0.0"
#if !FABLE_COMPILER
    "fp: random cross-check against .NET IEEE arithmetic", fun () ->
        let rnd = System.Random 2312
        let interesting () =
            match rnd.Next 8 with
            | 0 -> uint32 (rnd.Next 0x800000) ||| (if rnd.Next 2 = 0 then 0x80000000u else 0u) // subnormal
            | 1 -> 0x3F800000u + uint32 (rnd.Next 0x1000)
            | _ -> uint32 (rnd.Next()) ||| (if rnd.Next 2 = 0 then 0x80000000u else 0u)
        let bitsF (x : float32) = rawOfWord (System.BitConverter.SingleToUInt32Bits x)
        let bitsD (x : float) = let u = System.BitConverter.DoubleToUInt64Bits x in rawOfWords (uint32 u) (uint32 (u >>> 32))
        let ofF (w : uint32) = System.BitConverter.UInt32BitsToSingle w
        let randD () =
            let u = (uint64 (uint32 (rnd.Next())) <<< 33) ^^^ uint64 (uint32 (rnd.Next())) ^^^ (uint64 (rnd.Next 4) <<< 62)
            System.BitConverter.UInt64BitsToDouble u
        let check what (mine : FpRes) (theirs : bigint) fmt =
            if isNaN fmt theirs && isNaN fmt mine.Bits then [] // NaN payloads differ by platform
            elif mine.Bits = theirs then []
            else [ sprintf "%s: got %s expected %s" what (mine.Bits.ToString("X")) (theirs.ToString("X")) ]
        [ for _ in 1 .. 4000 do
            let a, b, c = interesting (), interesting (), interesting ()
            let fa, fb, fc = ofF a, ofF b, ofF c
            yield! check (sprintf "add %08X %08X" a b) (add defaultCtl F32 (f32 a) (f32 b)) (bitsF (fa + fb)) F32
            yield! check (sprintf "sub %08X %08X" a b) (sub defaultCtl F32 (f32 a) (f32 b)) (bitsF (fa - fb)) F32
            yield! check (sprintf "mul %08X %08X" a b) (mul defaultCtl F32 (f32 a) (f32 b)) (bitsF (fa * fb)) F32
            yield! check (sprintf "div %08X %08X" a b) (div defaultCtl F32 (f32 a) (f32 b)) (bitsF (fa / fb)) F32
            yield! check (sprintf "sqrt %08X" a) (sqrt defaultCtl F32 (f32 a)) (bitsF (System.MathF.Sqrt fa)) F32
            yield! check (sprintf "fma %08X %08X %08X" a b c) (mulAdd defaultCtl F32 (f32 c) (f32 a) (f32 b)) (bitsF (System.MathF.FusedMultiplyAdd(fa, fb, fc))) F32
            yield! check (sprintf "f32->f64 %08X" a) (convert defaultCtl F32 F64 (f32 a)) (bitsD (float fa)) F64
            let da, db, dc = randD (), randD (), randD ()
            let ba, bb, bc = bitsD da, bitsD db, bitsD dc
            yield! check (sprintf "dadd %g %g" da db) (add defaultCtl F64 ba bb) (bitsD (da + db)) F64
            yield! check (sprintf "dmul %g %g" da db) (mul defaultCtl F64 ba bb) (bitsD (da * db)) F64
            yield! check (sprintf "ddiv %g %g" da db) (div defaultCtl F64 ba bb) (bitsD (da / db)) F64
            yield! check (sprintf "dsqrt %g" da) (sqrt defaultCtl F64 ba) (bitsD (System.Math.Sqrt da)) F64
            yield! check (sprintf "dfma %g %g %g" da db dc) (mulAdd defaultCtl F64 bc ba bb) (bitsD (System.Math.FusedMultiplyAdd(da, db, dc))) F64
            yield! check (sprintf "f64->f32 %g" da) (convert defaultCtl F64 F32 ba) (bitsF (float32 da)) F32
            let txt = da.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
            if not (System.Double.IsNaN da || System.Double.IsInfinity da) then
                yield! expectEq ("format/parse " + txt) (parseFloat F64 (format F64 ba)) (Ok ba)
        ] |> List.truncate 10
#endif
]

let cTests : (string * (unit -> string list)) list = [
    "prototype: pointer and value", fun () -> protoIs "extern int sum_array(const int *a, int n);" "int sum_array(const int * a, int n)"
    "prototype: (void), unnamed, char/short/unsigned", fun () ->
        protoIs "unsigned char g(char, signed char c, short s, unsigned u, uint16_t w)" "unsigned char g(char arg1, signed char c, short s, unsigned int u, unsigned short w)"
        @ protoIs "void f(void)" "void f()"
    "prototype: arrays, pointer to pointer, return pointer", fun () ->
        protoIs "int *h(int a[], char **argv, const char *const s)" "int * h(int * a, char * * argv, const char * s)"
    "prototype: double rejected", fun () -> protoFails "double f(int x)" "no FPU"
    "prototype: long long rejected", fun () -> protoFails "long long f(int x)" "64-bit"
    "prototype: varargs rejected", fun () -> protoFails "int printf(const char *fmt, ...)" "variadic"
    "prototype: struct rejected", fun () -> protoFails "int f(struct point p)" "struct"
    "prototype: function pointer rejected", fun () -> protoFails "int f(int (*cb)(int))" "function pointer"
    "prototype: not a prototype", fun () -> protoFails "int x;" "prototype"
    "find @ C: prototypes", fun () ->
        let found = findPrototypes (srcLines "@ C: int a(int);\nmov r0, #1\n@ c: void b(void)\n@ not one")
        expectEq "count" found.Length 2
        @ (match found with | [ Ok p1; Ok p2 ] -> expectEq "names" (p1.FName, p1.Line, p2.FName, p2.Line) ("a", 1, "b", 3) | _ -> [ "bad parse" ])

    "call: sum_array({10, 20, 30, -5}, 4)", fun () -> returns sumArray [ "{10, 20, 30, -5}"; "4" ] 55L 0
    "call: array buffer is in the caller area, n in r1", fun () ->
        match cCall sumArray [ "{1, 2}"; "2" ] with
        | Ok(setup, _) -> expectEq "argument words" setup.ArgWords [ callerArea; 2u ]
        | Error e -> [ e ]
    "call: not .global warns", fun () ->
        hasProblem "@ C: int one(void)\none:\nmov r0, #1\nbx lr" [] "not declared .global"
    "call: undefined function", fun () -> callFails "@ C: int nothere(int x)\nmov r0, #1" [ "1" ] "undefined reference to `nothere'"
    "call: data label is not a function", fun () -> callFails "@ C: int d(void)\n.data\nd: .word 1" [] "data label"
    "call: wrong argument count", fun () -> callFails sumArray [ "{1}" ] "takes 2 argument(s)"
    "call: value out of range", fun () -> callFails "@ C: int f(signed char c)\n.global f\nf: bx lr" [ "200" ] "does not fit"
    "call: negative to unsigned rejected", fun () -> callFails "@ C: int f(unsigned u)\n.global f\nf: bx lr" [ "-1" ] "unsigned"
    "call: string for int * rejected", fun () -> callFails sumArray [ "\"ab\""; "2" ] "char *"

    "call: char and short are extended in registers", fun () ->
        let code = "@ C: int f(signed char c, short s, unsigned char u, unsigned short w)\n.global f\nf: bx lr"
        match cCall code [ "-1"; "-2"; "200"; "0xFFFF" ] with
        | Ok(setup, _) -> expectEq "argument words" setup.ArgWords [ 0xFFFFFFFFu; 0xFFFFFFFEu; 200u; 0xFFFFu ]
        | Error e -> [ e ]
    "call: char literal argument", fun () -> returns "@ C: int id(char c)\n.global id\nid: bx lr" [ "'A'" ] 65L 0
    "call: char result shown as character", fun () ->
        match cCall "@ C: char next(char c)\n.global next\nnext: add r0, r0, #1\nbx lr" [ "'A'" ] with
        | Ok(_, res) -> expectEq "return text" res.ReturnText "66 'B'"
        | Error e -> [ e ]
    "call: signed return decoded", fun () -> returns "@ C: int neg(int x)\n.global neg\nneg: rsb r0, r0, #0\nbx lr" [ "5" ] -5L 0
    "call: unsigned return decoded", fun () -> returns "@ C: unsigned big(void)\n.global big\nbig: mvn r0, #0\nbx lr" [] 4294967295L 0

    "call: string argument (strlen)", fun () ->
        returns """@ C: int my_strlen(const char *s);
                   .global my_strlen
                   my_strlen:
                   mov r1, r0
                   1: ldrb r2, [r1], #1
                   cmp r2, #0
                   bne 1b
                   sub r0, r1, r0
                   sub r0, r0, #1
                   bx lr""" [ "\"hello\"" ] 5L 0

    "call: output buffer [4] filled with squares", fun () ->
        let code = """@ C: void squares(int *out, int n)
                      .global squares
                      squares:
                      mov r2, #0
                      1: mul r3, r2, r2
                      str r3, [r0], #4
                      add r2, r2, #1
                      cmp r2, r1
                      blt 1b
                      bx lr"""
        match cCall code [ "[4]"; "4" ] with
        | Ok(_, res) ->
            match res.BufferResults with
            | [ br ] -> expectEq "buffer" br.After [ 0L; 1L; 4L; 9L ] @ expectEq "changed" br.Changed [ 1; 2; 3 ] @ expectEq "return" res.ReturnText "void"
            | _ -> [ "expected one buffer" ]
        | Error e -> [ e ]

    "call: six arguments, two on the stack", fun () ->
        let code = """@ C: int sum6(int a, int b, int c, int d, int e, int f)
                      .global sum6
                      sum6:
                      add r0, r0, r1
                      add r0, r0, r2
                      add r0, r0, r3
                      ldr r1, [sp]
                      add r0, r0, r1
                      ldr r1, [sp, #4]
                      add r0, r0, r1
                      bx lr"""
        match cCall code [ "1"; "2"; "3"; "4"; "5"; "6" ] with
        | Ok(setup, res) -> expectEq "return" res.ReturnValue (Some 21L) @ expectEq "SP 8-byte aligned" (setup.SPAtCall % 8u) 0u
        | Error e -> [ e ]

    "call: r4 not preserved is reported", fun () ->
        hasProblem "@ C: int f(int x)\n.global f\nf: mov r4, r0\nmov r0, r4\nbx lr" [ "1" ] "R4 was not preserved"
    "call: r4 saved with push/pop is fine", fun () ->
        returns "@ C: int f(int x)\n.global f\nf: push {r4, lr}\nmov r4, r0\nadd r0, r4, #1\npop {r4, pc}" [ "1" ] 2L 0
    "call: SP not restored is reported", fun () ->
        hasProblem "@ C: int f(void)\n.global f\nf: push {r0}\nbx lr" [] "SP not restored"
    "call: unextended unsigned char result reported", fun () ->
        hasProblem "@ C: unsigned char f(void)\n.global f\nf: mvn r0, #0\nbx lr" [] "zero-extended"
    "call: unextended signed char result reported", fun () ->
        hasProblem "@ C: signed char f(void)\n.global f\nf: mov r0, #0xFF\nbx lr" [] "sign-extended"
    "call: invalid _Bool result reported", fun () ->
        hasProblem "@ C: _Bool f(void)\n.global f\nf: mov r0, #2\nbx lr" [] "_Bool"
    "call: writing through const pointer reported", fun () ->
        hasProblem "@ C: void f(const int *p)\n.global f\nf: mov r1, #1\nstr r1, [r0]\nbx lr" [ "{5}" ] "const"

    "call: pointer result described", fun () ->
        let code = """@ C: int *find_max(int *a, int n)
                      .global find_max
                      find_max:
                      mov r2, r0
                      ldr r3, [r0]
                      1: ldr r12, [r0], #4
                      cmp r12, r3
                      movgt r3, r12
                      subgt r2, r0, #4
                      subs r1, r1, #1
                      bne 1b
                      mov r0, r2
                      bx lr"""
        match cCall code [ "{10, 5, 30, 20}"; "4" ] with
        | Ok(_, res) -> expectEq "return text" res.ReturnText "&a[2] -> 30 (0x1E)"
        | Error e -> [ e ]
    "call: NULL and label pointer arguments", fun () ->
        let code = "@ C: int f(int *p, int *q)\n.global f\nf: mov r0, r1\nbx lr\n.data\ntable: .word 7"
        match cCall code [ "NULL"; "table" ] with
        | Ok(setup, res) ->
            expectEq "first word" setup.ArgWords.[0] 0u
            @ (match setup.Run.st |> Map.tryFind "table" with
               | Some(addr, _) -> expectEq "second word" setup.ArgWords.[1] addr
               | None -> [ "no table symbol" ])
        | Error e -> [ e ]
    "call: infinite loop hits step limit", fun () ->
        match cCall "@ C: void spin(void)\n.global spin\nspin: b spin" [] with
        | Ok(_, { Outcome = StepLimit _ }) -> []
        | Ok(_, res) -> [ sprintf "outcome %A" res.Outcome ]
        | Error e -> [ e ]
    "call: running off the end of the code is reported", fun () ->
        match cCall "@ C: int f(void)\n.global f\nf: mov r0, #1" [] with
        | Ok(_, { Outcome = RanOffEnd }) -> []
        | Ok(_, res) -> [ sprintf "outcome %A" res.Outcome ]
        | Error e -> [ e ]
    "call: formatted summary", fun () ->
        match cCall sumArray [ "{1, 2, 3}"; "3" ] with
        | Ok(setup, res) -> expectEq "summary" (formatCall setup res) [ "sum_array(a=&a[0], n=3)"; "  returned 6 (0x6)"; "  a[3]: {1, 2, 3}" ]
        | Error e -> [ e ]
]

[<EntryPoint>]
let main _ =
    let mutable failed = 0
    for (name, code, expects) in allTests do
        match run code with
        | Error e when expects = [ ParseError ] && e.StartsWith "parse errors" -> printfn "pass %s" name
        | Error e when expects = [ Expect.RunError ] && e.StartsWith "run error" -> printfn "pass %s" name
        | Error e ->
            failed <- failed + 1
            printfn "FAIL %s: %s" name e
        | Ok dp ->
            match List.choose (check dp) expects with
            | [] -> printfn "pass %s" name
            | errs ->
                failed <- failed + 1
                printfn "FAIL %s: %s" name (String.concat "; " errs)
    for (name, test) in fpTests @ alignTests @ cTests do
        match (try test () with e -> [ "exception: " + e.Message ]) with
        | [] -> printfn "pass %s" name
        | errs ->
            failed <- failed + 1
            printfn "FAIL %s: %s" name (String.concat "; " errs)
    let total = allTests.Length + fpTests.Length + alignTests.Length + cTests.Length
    printfn "%d/%d passed" (total - failed) total
    if failed = 0 then 0 else 1
