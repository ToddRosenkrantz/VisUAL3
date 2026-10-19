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
    | ParseError -> Some "expected a parse error"
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
       DATA   DCD 0, 0""",
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
       LOOP   ADD R0, R0, R1
              SUBS R1, R1, #1
              BNE LOOP""",
    [ Reg (R0, 55u); Reg (R1, 0u) ]

    "BL / MOV PC, LR subroutine",
    """      MOV R0, #4
              BL DOUBLE
              B FIN
       DOUBLE ADD R0, R0, R0
              MOV PC, LR
       FIN    MOV R1, #1""",
    [ Reg (R0, 8u); Reg (R1, 1u) ]

    "LDR/STR with data section",
    """      ADR R0, DATA
              LDR R1, [R0]
              LDR R2, [R0, #4]
              ADD R3, R1, R2
              STR R3, [R0, #8]
              LDR R4, [R0, #8]
       DATA   DCD 100, 23, 0""",
    [ Reg (R3, 123u); Reg (R4, 123u) ]

    "LDRB / STRB",
    """      ADR R0, DATA
              LDRB R1, [R0, #1]
              MOV R2, #0xAB
              STRB R2, [R0]
              LDR R3, [R0]
       DATA   DCD 0x11223344""",
    [ Reg (R1, 0x33u); Reg (R3, 0x112233ABu) ]

    "post-indexed and writeback",
    """      ADR R0, DATA
              LDR R1, [R0], #4
              LDR R2, [R0, #4]!
       DATA   DCD 1, 2, 3""",
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
       DATA   FILL 8""",
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

let allTests = tests @ mulTests @ shiftTests

[<EntryPoint>]
let main _ =
    let mutable failed = 0
    for (name, code, expects) in allTests do
        match run code with
        | Error e when expects = [ ParseError ] && e.StartsWith "parse errors" -> printfn "pass %s" name
        | Error e ->
            failed <- failed + 1
            printfn "FAIL %s: %s" name e
        | Ok dp ->
            match List.choose (check dp) expects with
            | [] -> printfn "pass %s" name
            | errs ->
                failed <- failed + 1
                printfn "FAIL %s: %s" name (String.concat "; " errs)
    printfn "%d/%d passed" (allTests.Length - failed) allTests.Length
    if failed = 0 then 0 else 1
