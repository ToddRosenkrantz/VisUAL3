(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.ParseTop
    Description: Top-level code to parse instructions
*)

/// Top-level code implementing assembler parsing
module ParseTop
    open CommonLex
    open CommonData

    open Errors
    open Expressions

    /// allows different modules to return different instruction types
    type Instr =
        | IMEM of Memory.Instr
        | IDP of DP.Instr
        | IMISC of Misc.Instr
        | IBRANCH of Branch.Instr
        | EMPTY



    let Blank lab = {
        PCond = Cal
        PInstr = Ok EMPTY
        PLabel = lab
        ISize = 0u
        DSize = Some 0u
        POpCode = ""
        PStall = 0
    }

    /// Split line on whitespace into an list
    let splitIntoWords (line : string) =
        line.Split(([| ' '; '\t'; '\f'; '\r'; '\n'; '\b' |] : char array),
          System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.collect (function | "" -> [||] | s -> [| s |])
        |> Array.map (fun s -> s.Trim())
        |> Array.toList

    /// Note that Instr in Mem and DP modules is NOT same as Instr in this module
    /// Instr here is all possible isntruction values combines with a D.U.
    /// that tags the Instruction class
    /// Similarly ErrParse
    /// Similarly IMatch here is combination of module IMatches
    let IMatch(ld : LineData) : Parse<Instr> option =
        let copy cons pa =
            { // NB need this copy by fields because types do not match

                PCond = pa.PCond
                PInstr = Result.map cons pa.PInstr
                PLabel = pa.PLabel
                POpCode = pa.POpCode
                ISize = pa.ISize
                DSize = pa.DSize
                PStall = pa.PStall
            } |> Some


        match ld with
        | Memory.IMatch pa -> copy IMEM pa
        | DP.IMatch pa -> copy IDP pa
        | Misc.IMatch pa -> copy IMISC pa
        | Branch.IMatch pa -> copy IBRANCH pa
        | _ -> None



    type CondInstr = {
        Cond : Condition
        InsExec : Instr
        InsOpCode : string
        Cycles : int64
        }

    let makeParse labOpt la ins =
        {
            PInstr = ins
            PLabel = labOpt |> Option.map (fun lab -> lab, la)
            ISize = 0u
            DSize = Some 0u
            PCond = Cal
            POpCode = ""
            PStall = 0
        }




    /// VFP/NEON floating point mnemonics, e.g. VADD.F32, vldr, VPUSHEQ
    let isVfpOpCode (opc : string) =
        let o = opc.ToUpper()
        [ "VADD"; "VSUB"; "VMUL"; "VDIV"; "VMLA"; "VMLS"; "VNMUL"; "VNMLA"; "VNMLS"; "VFMA"; "VFMS"
          "VNEG"; "VABS"; "VSQRT"; "VCMP"; "VCVT"; "VMOV"; "VMRS"; "VMSR"; "VLDR"; "VSTR"
          "VLDM"; "VSTM"; "VPUSH"; "VPOP" ]
        |> List.exists o.StartsWith

    /// The emulated CPU has no FPU (soft-float): floating point must use library calls
    let noHardFloatError opc =
        sprintf "'%s' is a hardware floating point (VFP) instruction. This emulator models an ARMv7 core without an FPU: use integer code or soft-float library calls" opc
        |> ``Invalid instruction`` |> Error

    /// Parse one statement from the GNU front end (GnuLex)
    let parseStmt (symtab : SymbolTable) (loadI : uint32, loadD : uint32) (st : GnuLex.Stmt) =
        let defParse lab = makeParse lab (Ok loadI)
        let lineData op operands = {
                OpCode = op
                Operands = operands
                Label = st.Label
                LoadAddr = (if GnuLex.dataDirectives.Contains op || GnuLex.alignDirectives.Contains op then loadD else loadI) |> WA
                SymTab = symtab
            }
        match st.LexError, st.Op with
        | Some msg, _ -> defParse None (``Invalid instruction`` msg |> Error)
        | None, "" -> defParse st.Label (EMPTY |> Ok)
        | None, op ->
            match IMatch(lineData op st.Operands) with
            | Some pa -> pa
            | None when isVfpOpCode op -> defParse None (noHardFloatError op)
            | None ->
                // "loop add r0, r0, #1": a label written without its ':'
                let words = splitIntoWords st.Operands
                let looksLikeLabel =
                    match words with
                    | next :: rest ->
                        let rest = st.Operands.Substring(st.Operands.IndexOf next + next.Length).Trim()
                        (IMatch(lineData (next.ToUpper()) rest)).IsSome || isVfpOpCode next
                    | [] -> false
                if looksLikeLabel then
                    let lab = op.ToLower()
                    defParse None (``Invalid instruction`` (sprintf "'%s' is not an instruction. A label must end with ':' (%s:)" lab lab) |> Error)
                else
                    defParse None (``Unimplemented instruction`` op |> Error)
