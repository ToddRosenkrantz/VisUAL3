(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.ErrorDocs
    Description: create markdown for assembler hover message hints (GNU as syntax)
*)
module ErrorDocs

open EEExtensions
open Refs

/// Hover help for one instruction root or directive
type OpDoc = {
    Root : string // upper case mnemonic root, or directive such as ".WORD"
    Suffixes : string list // allowed between root and condition
    Legend : string // what it does
    Syntax : string
    Examples : string list
}

let private conds = [ ""; "EQ"; "NE"; "CS"; "HS"; "CC"; "LO"; "MI"; "PL"; "VS"; "VC"; "HI"; "LS"; "GE"; "LT"; "GT"; "LE"; "AL" ]

let private doc root suffixes legend syntax examples =
    { Root = root; Suffixes = suffixes; Legend = legend; Syntax = syntax; Examples = examples }

let private dp3 (root : string) legend =
    let r = root.ToLower()
    doc root [ ""; "S" ] legend (sprintf "%s{s}{cond} Rd, Rn, op2   (or %s Rd, op2 meaning Rd, Rd, op2)" r r)
        [ sprintf "%s r0, r1, r2" r; sprintf "%s r5, r5, #101" r; sprintf "%s r0, #1" r; sprintf "%s r7, r10, r3, lsl #2" r ]

let private dp2 (root : string) legend =
    let r = root.ToLower()
    doc root [ ""; "S" ] legend (sprintf "%s{s}{cond} Rd, op2" r)
        [ sprintf "%s r0, r1" r; sprintf "%s r5, #0x5a" r; sprintf "%s r7, r2, asr #3" r ]

let private cmp (root : string) legend =
    let r = root.ToLower()
    doc root [ ""; "S" ] legend (sprintf "%s{cond} Rn, op2" r)
        [ sprintf "%s r0, r1" r; sprintf "%s r5, #101" r; sprintf "%s r2, r3, lsl #1" r ]

let private shift (root : string) legend =
    let r = root.ToLower()
    doc root [ ""; "S" ] legend (sprintf "%s{s}{cond} Rd, Rm, #n  or  %s Rd, Rm, Rs  (Rd may be omitted)" r r)
        [ sprintf "%s r0, r1, #2" r; sprintf "%s r0, r1, r2" r; sprintf "%s r3, #1" r ]

let private dataDir root legend syntax examples = doc root [ "" ] legend syntax examples

let private vfp3 (root : string) legend =
    let r = root.ToLower()
    doc root [ "" ] legend (sprintf "%s{cond}.f32 Sd, Sn, Sm   or   %s.f64 Dd, Dn, Dm   (Fd may be omitted)" r r)
        [ sprintf "%s.f32 s0, s1, s2" r; sprintf "%s.f64 d0, d1, d2" r ]

let private vfpMac (root : string) legend =
    let r = root.ToLower()
    doc root [ "" ] legend (sprintf "%s{cond}.f32 Sd, Sn, Sm   or   %s.f64 Dd, Dn, Dm" r r) [ sprintf "%s.f64 d0, d1, d2" r ]

let private vfp2 (root : string) legend =
    let r = root.ToLower()
    doc root [ "" ] legend (sprintf "%s{cond}.f32 Sd, Sm   or   %s.f64 Dd, Dm" r r) [ sprintf "%s.f32 s0, s1" r; sprintf "%s.f64 d0, d1" r ]

/// All instructions and directives the assembler accepts
let opDocs = [
    dp3 "ADD" "Rd := Rn + op2"
    dp3 "SUB" "Rd := Rn - op2"
    dp3 "RSB" "Rd := op2 - Rn"
    dp3 "ADC" "Rd := Rn + op2 + C"
    dp3 "SBC" "Rd := Rn - op2 - NOT C"
    dp3 "RSC" "Rd := op2 - Rn - NOT C"
    dp3 "AND" "Rd := Rn AND op2"
    dp3 "ORR" "Rd := Rn OR op2"
    dp3 "EOR" "Rd := Rn XOR op2"
    dp3 "BIC" "Rd := Rn AND NOT op2"
    dp2 "MOV" "Rd := op2 (a 16-bit value that is not an ARM immediate becomes movw)"
    doc "MOVW" [ "" ] "Rd := a 16-bit value (0-65535)" "movw{cond} Rd, #imm16" [ "movw r0, #0x1234"; "movw r0, #:lower16:table" ]
    doc "MOVT" [ "" ] "top half of Rd := a 16-bit value; the bottom half is kept" "movt{cond} Rd, #imm16"
        [ "movt r0, #0x1234       @ after movw r0, #0x5678: r0 = 0x12345678"; "movt r0, #:upper16:table" ]
    doc "NEG" [ ""; "S" ] "Rd := 0 - Rm (same as rsb Rd, Rm, #0)" "neg{s}{cond} Rd, Rm" [ "neg r0, r1"; "negs r2, r2" ]
    doc "NOP" [ "" ] "do nothing" "nop{cond}" [ "nop" ]
    dp2 "MVN" "Rd := NOT op2"
    cmp "CMP" "set NZCV from Rn - op2"
    cmp "CMN" "set NZCV from Rn + op2"
    cmp "TST" "set NZ (and C from the shift) from Rn AND op2"
    cmp "TEQ" "set NZ (and C from the shift) from Rn XOR op2"
    shift "LSL" "Rd := Rm shifted left (same as mov Rd, Rm, lsl n)"
    shift "LSR" "Rd := Rm shifted right, zero fill"
    shift "ASR" "Rd := Rm shifted right, sign fill"
    shift "ROR" "Rd := Rm rotated right"
    doc "RRX" [ ""; "S" ] "Rd := Rm rotated right one bit through C" "rrx{s}{cond} Rd, Rm" [ "rrx r0, r1"; "rrxs r2" ]
    doc "MUL" [ ""; "S" ] "Rd := Rn * Rm (low 32 bits). S sets N and Z only" "mul{s}{cond} Rd, Rn, Rm" [ "mul r0, r1, r2"; "mul r3, r3, r4"; "muls r0, r1" ]
    doc "MLA" [ ""; "S" ] "Rd := Rn * Rm + Ra (low 32 bits). S sets N and Z only" "mla{s}{cond} Rd, Rn, Rm, Ra" [ "mla r0, r1, r2, r3" ]
    doc "MLS" [ "" ] "Rd := Ra - Rn * Rm (low 32 bits)" "mls{cond} Rd, Rn, Rm, Ra" [ "mls r0, r1, r2, r3" ]
    doc "UMULL" [ ""; "S" ] "RdHi:RdLo := Rn * Rm, unsigned 64-bit product. S sets N and Z" "umull{s}{cond} RdLo, RdHi, Rn, Rm" [ "umull r0, r1, r2, r3" ]
    doc "SMULL" [ ""; "S" ] "RdHi:RdLo := Rn * Rm, signed 64-bit product. S sets N and Z" "smull{s}{cond} RdLo, RdHi, Rn, Rm" [ "smull r0, r1, r2, r3" ]
    doc "UMLAL" [ ""; "S" ] "RdHi:RdLo += Rn * Rm, unsigned 64-bit. S sets N and Z" "umlal{s}{cond} RdLo, RdHi, Rn, Rm" [ "umlal r0, r1, r2, r3" ]
    doc "SMLAL" [ ""; "S" ] "RdHi:RdLo += Rn * Rm, signed 64-bit. S sets N and Z" "smlal{s}{cond} RdLo, RdHi, Rn, Rm" [ "smlal r0, r1, r2, r3" ]
    doc "SDIV" [ "" ] "Rd := Rn / Rm, signed, rounded toward zero. Dividing by zero gives 0" "sdiv{cond} Rd, Rn, Rm" [ "sdiv r0, r1, r2"; "sdiv r0, r2            @ r0 := r0 / r2" ]
    doc "UDIV" [ "" ] "Rd := Rn / Rm, unsigned, rounded toward zero. Dividing by zero gives 0" "udiv{cond} Rd, Rn, Rm" [ "udiv r0, r1, r2" ]
    doc "UBFX" [ "" ] "Rd := width bits of Rn starting at bit lsb, zero-extended" "ubfx{cond} Rd, Rn, #lsb, #width" [ "ubfx r0, r1, #8, #8    @ bits 8-15 of r1" ]
    doc "SBFX" [ "" ] "Rd := width bits of Rn starting at bit lsb, sign-extended" "sbfx{cond} Rd, Rn, #lsb, #width" [ "sbfx r0, r1, #4, #4" ]
    doc "BFI" [ "" ] "insert the low width bits of Rn into Rd at bit lsb; the rest of Rd is unchanged" "bfi{cond} Rd, Rn, #lsb, #width" [ "bfi r0, r1, #12, #4   @ bits 12-15 of r0 := low 4 bits of r1" ]
    doc "BFC" [ "" ] "clear width bits of Rd starting at bit lsb" "bfc{cond} Rd, #lsb, #width" [ "bfc r0, #0, #8         @ clear the low byte" ]
    doc "CLZ" [ "" ] "Rd := number of zero bits above the highest 1 bit of Rm (32 if Rm = 0)" "clz{cond} Rd, Rm" [ "clz r0, r1"; "rsb r0, r0, #31        @ after clz: index of the highest set bit" ]
    doc "RBIT" [ "" ] "Rd := the bits of Rm in reverse order (bit 0 <-> bit 31)" "rbit{cond} Rd, Rm" [ "rbit r0, r1"; "clz r0, r0             @ after rbit: number of trailing zeros" ]
    doc "REV" [ "" ] "Rd := the four bytes of Rm in reverse order (swap endianness)" "rev{cond} Rd, Rm" [ "rev r0, r1             @ 0x11223344 -> 0x44332211" ]
    doc "REV16" [ "" ] "Rd := Rm with the two bytes of each halfword swapped" "rev16{cond} Rd, Rm" [ "rev16 r0, r1           @ 0x11223344 -> 0x22114433" ]
    doc "REVSH" [ "" ] "Rd := the two bytes of Rm's low halfword swapped, sign-extended" "revsh{cond} Rd, Rm" [ "revsh r0, r1           @ 0x000080FF -> 0xFFFFFF80" ]
    doc "UXTB" [ "" ] "Rd := low byte of Rm, zero-extended (Rm may be rotated first)" "uxtb{cond} Rd, Rm {, ror #8|16|24}" [ "uxtb r0, r1"; "uxtb r0, r1, ror #8   @ byte 1" ]
    doc "UXTH" [ "" ] "Rd := low halfword of Rm, zero-extended" "uxth{cond} Rd, Rm {, ror #8|16|24}" [ "uxth r0, r1"; "uxth r0, r1, ror #16  @ top half" ]
    doc "SXTB" [ "" ] "Rd := low byte of Rm, sign-extended" "sxtb{cond} Rd, Rm {, ror #8|16|24}" [ "sxtb r0, r1" ]
    doc "SXTH" [ "" ] "Rd := low halfword of Rm, sign-extended" "sxth{cond} Rd, Rm {, ror #8|16|24}" [ "sxth r0, r1" ]
    doc "LDR" [ ""; "B"; "H"; "SB"; "SH"; "D" ]
        "load a word into Rd; b/h = byte/halfword (zero-extended), sb/sh = signed byte/halfword (sign-extended), d = two words into an even/odd register pair"
        "ldr{b|h|sb|sh|d}{cond} Rd, address   or   ldr Rd, =value"
        [ "ldr r0, [r1]"; "ldr r0, [r1, #4]"; "ldr r0, [r1], #4       @ post-indexed"; "ldr r0, [r1, #-8]!     @ pre-indexed, write back"
          "ldr r0, [r1, r2, lsl #2]"; "ldr r0, [r1, -r2]      @ register offset subtracted"; "ldrb r3, [r0, #1]"; "ldrsh r2, [r0, #2]     @ h/sb/sh: offset -255..255"
          "ldrh r2, [r0, r1]      @ h/sb/sh: register offset, no shift"; "ldrd r4, r5, [r0, #8]  @ r4 = word at r0+8, r5 = the next word"; "ldr r0, =table         @ address of a label"; "ldr r0, =0x12345678" ]
    doc "STR" [ ""; "B"; "H"; "D" ] "store a word from Rd into memory; b/h = its low byte/halfword; d = an even/odd register pair as two words" "str{b|h|d}{cond} Rd, address"
        [ "strd r4, r5, [sp, #-8]! @ save a register pair"; "str r0, [r1]"; "str r0, [r1, #4]!"; "strb r2, [r3], #1"; "strh r2, [r3, #2]      @ h: offset -255..255"; "str r0, [sp, #-4]!" ]
    doc "LDM" [ ""; "IA"; "IB"; "DA"; "DB"; "FD"; "ED"; "FA"; "EA" ] "load several registers from consecutive words (lowest register at lowest address)"
        "ldm{mode}{cond} Rn{!}, {register list}" [ "ldm r0, {r1-r3}"; "ldmia r0!, {r4, r5}"; "ldmfd sp!, {r4, pc}" ]
    doc "STM" [ ""; "IA"; "IB"; "DA"; "DB"; "FD"; "ED"; "FA"; "EA" ] "store several registers to consecutive words (lowest register at lowest address)"
        "stm{mode}{cond} Rn{!}, {register list}" [ "stm r0, {r1-r3}"; "stmdb sp!, {r4, lr}"; "stmfd sp!, {r0-r3}" ]
    doc "PUSH" [ "" ] "push registers on the full-descending stack (stmdb sp!, {list})" "push{cond} {register list}" [ "push {r4, lr}"; "push {r0-r3, r12}" ]
    doc "POP" [ "" ] "pop registers from the stack (ldmia sp!, {list}); popping pc returns" "pop{cond} {register list}" [ "pop {r4, pc}"; "pop {r0-r3}" ]
    doc "B" [ "" ] "branch to label" "b{cond} label" [ "b loop"; "bne 1b           @ back to the previous local label 1:"; "bge done" ]
    doc "BL" [ "" ] "branch to label and put the return address in lr (call a function)" "bl{cond} label" [ "bl sum_array"; "bleq handler" ]
    doc "BX" [ "" ] "branch to the address in a register (return from a function)" "bx{cond} Rm" [ "bx lr"; "bxne lr"; "bx r3" ]
    doc "ADR" [ "" ] "Rd := address of a nearby label (PC-relative)" "adr{cond} Rd, label" [ "adr r0, table" ]
    vfp3 "VADD" "Fd := Fn + Fm"
    vfp3 "VSUB" "Fd := Fn - Fm"
    vfp3 "VMUL" "Fd := Fn * Fm"
    vfp3 "VDIV" "Fd := Fn / Fm (x/0 gives infinity and sets DZC; 0/0 gives NaN and sets IOC)"
    vfp3 "VNMUL" "Fd := -(Fn * Fm)"
    vfpMac "VMLA" "Fd := Fd + Fn * Fm, rounding the product and the sum separately"
    vfpMac "VMLS" "Fd := Fd - Fn * Fm, rounding twice"
    vfpMac "VNMLA" "Fd := -Fd - Fn * Fm, rounding twice"
    vfpMac "VNMLS" "Fd := -Fd + Fn * Fm, rounding twice"
    vfpMac "VFMA" "Fd := Fd + Fn * Fm, fused: rounded once"
    vfpMac "VFMS" "Fd := Fd - Fn * Fm, fused: rounded once"
    vfpMac "VFNMA" "Fd := -Fd - Fn * Fm, fused: rounded once"
    vfpMac "VFNMS" "Fd := -Fd + Fn * Fm, fused: rounded once"
    vfp2 "VABS" "Fd := |Fm| (clears the sign bit)"
    vfp2 "VNEG" "Fd := -Fm (flips the sign bit)"
    vfp2 "VSQRT" "Fd := square root of Fm"
    doc "VCMP" [ "" ] "compare Fd with Fm or 0.0, setting N, Z, C, V in the FPSCR; copy them to the core flags with vmrs APSR_nzcv, fpscr"
        "vcmp{cond}.f32 Sd, Sm   vcmp.f64 Dd, Dm   vcmp.f32 Sd, #0"
        [ "vcmp.f32 s0, s1"; "vcmp.f64 d0, #0"; "vmrs APSR_nzcv, fpscr   @ then blt, beq, bgt ..." ]
    doc "VCMPE" [ "" ] "as vcmp, but a quiet NaN operand also sets IOC" "vcmpe{cond}.f32 Sd, Sm" [ "vcmpe.f64 d0, d1" ]
    doc "VCVT" [ "" ] "convert: vcvt.<to>.<from>. To an integer it rounds toward zero (saturating, NaN gives 0)"
        "vcvt{cond}.<to>.<from> Fd, Fm   or   vcvt.<to>.<from> Fd, Fd, #fbits (fixed point)"
        [ "vcvt.s32.f32 s0, s1     @ float to int"; "vcvt.f64.s32 d0, s2     @ int to double"; "vcvt.f32.f64 s0, d1"; "vcvt.s32.f32 s0, s0, #16  @ to 16.16 fixed point" ]
    doc "VCVTR" [ "" ] "convert to an integer using the FPSCR rounding mode (round to nearest by default)" "vcvtr{cond}.s32.f32 Sd, Sm" [ "vcvtr.s32.f64 s0, d1" ]
    doc "VCVTB" [ "" ] "convert between single and the half-precision value in the bottom 16 bits of an s register"
        "vcvtb{cond}.f32.f16 Sd, Sm   or   vcvtb.f16.f32 Sd, Sm" [ "vcvtb.f16.f32 s1, s0"; "vcvtb.f32.f16 s2, s1" ]
    doc "VCVTT" [ "" ] "convert between single and the half-precision value in the top 16 bits of an s register"
        "vcvtt{cond}.f32.f16 Sd, Sm   or   vcvtt.f16.f32 Sd, Sm" [ "vcvtt.f16.f32 s1, s0" ]
    doc "VMOV" [ "" ] "copy bits between VFP registers, or between core and VFP registers, or load a VFP constant"
        "vmov{cond}{.f32|.f64} Fd, Fm   vmov Sn, Rt   vmov Rt, Sn   vmov Dm, Rlo, Rhi   vmov Rlo, Rhi, Dm   vmov.f32 Sd, #imm"
        [ "vmov.f64 d0, d1"; "vmov s0, r0"; "vmov r0, r1, d0       @ r0 = low half"; "vmov.f32 s0, #1.5     @ ±(16..31)/16 × 2^n, n -3..4"; "vmov.32 d2[1], r3" ]
    doc "VLDR" [ "" ] "load an s or d register from memory (word-aligned)" "vldr{cond} Fd, [Rn{, #±offset}]   or   vldr Fd, label"
        [ "vldr s0, [r0]"; "vldr d1, [r0, #8]"; "vldr.64 d2, [sp, #-16]" ]
    doc "VSTR" [ "" ] "store an s or d register to memory (word-aligned)" "vstr{cond} Fd, [Rn{, #±offset}]" [ "vstr s0, [r0, #4]"; "vstr d1, [sp]" ]
    doc "VLDM" [ ""; "IA"; "DB" ] "load consecutive VFP registers from consecutive words" "vldm{ia|db}{cond} Rn{!}, {list}"
        [ "vldmia r0!, {s0-s3}"; "vldm r1, {d0, d1}"; "vldmdb r2!, {d4}" ]
    doc "VSTM" [ ""; "IA"; "DB" ] "store consecutive VFP registers to consecutive words" "vstm{ia|db}{cond} Rn{!}, {list}"
        [ "vstmia r0!, {s0-s3}"; "vstmdb sp!, {d8-d9}" ]
    doc "VPUSH" [ "" ] "push consecutive VFP registers (vstmdb sp!, {list})" "vpush{cond} {list}" [ "vpush {d8-d15}"; "vpush {s16, s17}" ]
    doc "VPOP" [ "" ] "pop consecutive VFP registers (vldmia sp!, {list})" "vpop{cond} {list}" [ "vpop {d8-d15}" ]
    doc "VMRS" [ "" ] "read the FPSCR into a core register, or copy its N, Z, C, V into the core flags"
        "vmrs{cond} Rt, fpscr   or   vmrs APSR_nzcv, fpscr" [ "vmrs APSR_nzcv, fpscr"; "vmrs r0, fpscr" ]
    doc "FMSTAT" [ "" ] "copy the FPSCR N, Z, C, V into the core flags (same as vmrs APSR_nzcv, fpscr)" "fmstat{cond}" [ "fmstat" ]
    doc "VMSR" [ "" ] "write the FPSCR: rounding mode (bits 23-22), FZ (24), DN (25), flags and exception bits"
        "vmsr{cond} fpscr, Rt" [ "mov r0, #0x00C00000   @ round toward zero"; "vmsr fpscr, r0" ]
    doc "MRS" [ "" ] "Rd := the core flags N, Z, C, V in bits 31-28" "mrs{cond} Rd, apsr" [ "mrs r0, apsr" ]
    doc "MSR" [ "" ] "set the core flags N, Z, C, V from bits 31-28 of a register or constant" "msr{cond} APSR_nzcvq, Rm   or   msr APSR_nzcvq, #imm"
        [ "msr APSR_nzcvq, r0"; "msr APSR_nzcvq, #0x20000000   @ C = 1" ]
    dataDir ".FLOAT" "store single-precision floating point values" ".float value, ..." [ "vals: .float 1.5, -0.1, 3e8" ]
    dataDir ".SINGLE" "store single-precision values (same as .float)" ".single value, ..." [ ".single 0.5" ]
    dataDir ".DOUBLE" "store double-precision floating point values" ".double value, ..." [ "pi: .double 3.141592653589793" ]
    dataDir ".WORD" "store 32-bit values" ".word value, ..." [ "table: .word 1, 2, -3, 0x10"; "ptrs:  .word table, buffer" ]
    dataDir ".LONG" "store 32-bit values (same as .word)" ".long value, ..." [ ".long 100000" ]
    dataDir ".INT" "store 32-bit values (same as .word)" ".int value, ..." [ ".int 7" ]
    dataDir ".4BYTE" "store 32-bit values (same as .word)" ".4byte value, ..." [ ".4byte 7" ]
    dataDir ".HWORD" "store 16-bit values" ".hword value, ..." [ ".hword 0x1234, -1" ]
    dataDir ".SHORT" "store 16-bit values (same as .hword)" ".short value, ..." [ ".short 500" ]
    dataDir ".2BYTE" "store 16-bit values (same as .hword)" ".2byte value, ..." [ ".2byte 500" ]
    dataDir ".BYTE" "store 8-bit values" ".byte value, ..." [ ".byte 1, 2, 'A', -1" ]
    dataDir ".ASCII" "store a string with no terminating zero" ".ascii \"text\", ..." [ "msg: .ascii \"Hi\\n\"" ]
    dataDir ".ASCIZ" "store a zero-terminated (C) string" ".asciz \"text\", ..." [ "msg: .asciz \"hello\"" ]
    dataDir ".STRING" "store a zero-terminated (C) string (same as .asciz)" ".string \"text\", ..." [ ".string \"hello\"" ]
    dataDir ".SPACE" "reserve bytes, filled with 0 (or fill)" ".space size{, fill}" [ "buffer: .space 64"; ".space 4, 0xff" ]
    dataDir ".SKIP" "reserve bytes (same as .space)" ".skip size{, fill}" [ ".skip 16" ]
    dataDir ".ZERO" "reserve zeroed bytes" ".zero size" [ ".zero 32" ]
    dataDir ".FILL" "repeat a value of size 1, 2 or 4 bytes" ".fill repeat{, size{, value}}" [ ".fill 10, 4, -1" ]
    dataDir ".ALIGN" "pad to a multiple of 2^n bytes" ".align n" [ ".align 2      @ word aligned" ]
    dataDir ".P2ALIGN" "pad to a multiple of 2^n bytes" ".p2align n" [ ".p2align 3" ]
    dataDir ".BALIGN" "pad to a multiple of n bytes" ".balign n" [ ".balign 4" ]
    dataDir ".EQU" "define a constant symbol" ".equ symbol, expression" [ ".equ SIZE, 16"; ".equ MASK, (1 << 4) - 1" ]
    dataDir ".SET" "define a constant symbol (same as .equ)" ".set symbol, expression" [ ".set COUNT, 10" ]
    dataDir ".TEXT" "following lines are code" ".text" [ ".text" ]
    dataDir ".DATA" "following lines are initialised data" ".data" [ ".data" ]
    dataDir ".BSS" "following lines are zeroed data" ".bss" [ ".bss"; "buf: .space 256" ]
    dataDir ".SECTION" "switch section: .text, .data, .rodata or .bss" ".section name" [ ".section .rodata" ]
    dataDir ".GLOBAL" "make a label visible to C (needed for C Call)" ".global symbol, ..." [ ".global sum_array" ]
]

/// Find the documentation for an opcode: root + allowed suffix + condition, or a directive.
/// A VFP type such as .F32 is ignored.
let findDoc (opc : string) =
    let u = opc.ToUpper()
    let u = if u.StartsWith "." then u else u.Split('.').[0]
    if u.StartsWith "." then
        opDocs |> List.tryFind (fun d -> d.Root = u)
    else
        opDocs
        |> List.filter (fun d ->
            u.StartsWith d.Root &&
            (let rest = u.Substring d.Root.Length
             d.Suffixes |> List.exists (fun s -> rest.StartsWith s && List.contains (rest.Substring s.Length) conds)))
        |> List.sortByDescending (fun d -> d.Root.Length)
        |> List.tryHead

/// Remove a GNU as comment (@ or //) from a line
let private stripComment (line : string) =
    let cut (s : string) (marker : string) = match s.IndexOf marker with | -1 -> s | i -> s.Substring(0, i)
    cut (cut line "@") "//"

/// Hover help for opcode opc on source line: markdown lines, and the column range of the opcode
let getOpcHover (mess : string) (opc : string) (line : string) =
    if opc = "" then failwithf "can't get hover for '' opcode"
    let code = stripComment line
    let uLine = (code + " ").ToUpper().Replace(",", " ").Replace("#", " ")
    let badShift = [ " ROL "; " RRL " ] |> List.tryFind (fun s -> uLine.Contains s)
    let help =
        match findDoc opc with
        | Some d ->
            [ yield sprintf "**%s**: %s" (d.Syntax.Split(' ').[0]) d.Legend
              yield sprintf "`%s`" d.Syntax
              yield "```arm\n" + String.concat "\n" d.Examples + "\n```"
              match badShift with
              | Some bad -> yield sprintf "*%s is not an ARM shift: use lsl, lsr, asr, ror or rrx*" (bad.Trim().ToLower())
              | None -> () ]
        | None -> [ sprintf "'%s' is not an instruction or directive this assembler knows" opc ]
    let lineText = sprintf "```arm\n%s\n```\n" (code.TrimEnd())
    let range =
        match code.ToUpper().IndexOf(opc.ToUpper()) with
        | -1 -> (1, max 1 code.Length)
        | i -> (i + 1, i + opc.Length)
    ([ lineText ] @ (if mess = "" then help else mess :: help)), range
