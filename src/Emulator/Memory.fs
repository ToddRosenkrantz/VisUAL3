(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.Memory
    Description: Implement ARM memory instructions
*)

/// Implement ARM memory instructions
module Memory
    open CommonData
    open CommonLex
    open Errors
    open Expressions
    open Helpers

    /// Size of a single load or store
    type MSize =
        | MWord /// LDR, STR
        | MByte /// LDRB, LDRSB, STRB
        | MHalf /// LDRH, LDRSH, STRH

    type LSType =
        | LOAD /// LDR,LDRB
        | STORE /// STR,STRB

    /// Type of memory instruction
    type MMode =
        | PostIndex /// change base register after memory access
        | PreIndex /// change base register before memory access
        | NoIndex /// no chnage to base register

    type ScaledShiftCode =
        | SASR /// ASR scaling
        | SLSR /// LSR scaling
        | SLSL /// LSL scaling
        | SROR /// ROR scaling
        | SRRX /// RRX: rotate right one bit through C

    /// Single Store/Load memory instruction. LDR, LDRB, STR, STRB
    /// op{type}{cond} Rt, [Rn {, #offset}]        ; immediate offset
    /// op{type}{cond} Rt, [Rn, #offset]!          ; pre-indexed
    /// op{type}{cond} Rt, [Rn], #offset           ; post-indexed
    [<Struct>]
    type InstrMemSingle =
        {
            LSType : LSType
            Rd : RName;
            Rb : RName;
            MAddr : DataPath -> int32 -> int32;
            MemMode : MMode
            MemSize : MSize
            /// LDRSB, LDRSH: sign-extend the loaded value
            Signed : bool
        }

    /// Suffixes for LDM and STM
    type MultSuffix =
        | IA | IB | DA | DB
        | FD | ED | FA | EA

    /// Multiple Store/Load memory instruction. LDM, STM
    /// op{addr_mode}{cond} Rn{!}, reglist
    [<Struct>]
    type InstrMemMult = {
        Rn : RName;
        WB : bool
        rList : List<RName>;
        suff : MultSuffix
        }

    type Instr =
        | LDR of InstrMemSingle
        | STR of InstrMemSingle
        | LDM of InstrMemMult
        | STM of InstrMemMult
        | LDREQUAL of RName * uint32


    let memSpecSingle = {
        InstrC = MEM
        Roots = [ "LDR"; "STR" ]
        // STRSB and STRSH are rejected by the parser: stores are never signed
        Suffixes = [ ""; "B"; "H"; "SB"; "SH" ]
    }

    let memSpecMultiple = {
        InstrC = MEM
        Roots = [ "STM"; "LDM" ]
        // "" is IA (ARMv7 UAL)
        Suffixes = [ ""; "IA"; "IB"; "DA"; "DB"; "FD"; "ED"; "FA"; "EA" ]
    }

    /// PUSH {list} = STMDB SP!, {list}; POP {list} = LDMIA SP!, {list}
    let memSpecStack = {
        InstrC = MEM
        Roots = [ "PUSH"; "POP" ]
        Suffixes = [ "" ]
    }


    let memTypeSingleMap =
        Map.ofList [
            "LDR", LDR;
            "STR", STR;
        ]
    let memTypeMultMap =
        Map.ofList [
            "LDM", LDM;
            "STM", STM;
        ]



    /// map of all possible opcodes recognised
    let opCodes =
        let toArr = opCodeExpand >> Map.toArray
        Array.collect toArr [| memSpecSingle; memSpecMultiple; memSpecStack |]
        |> Map.ofArray



    /// Contructs an Instruction of InstrMemMult for LDM, STM
    let consMemMult wb reg rLst suffix =
            {
                Rn = reg;
                WB = wb
                rList = rLst;
                suff = suffix;
            }




    /// Where everything happens
    let parse (ls : LineData) : Parse<Instr> option =
        let (WA la) = ls.LoadAddr



        let parseLoad32 pCond =
            let parse =
                Ok((), ls.Operands)
                |> ResREGMATCH(fun _ rn -> rn)
                |> ResREMOVEPREFIX ","
                |> ResREMOVEPREFIX "="
                |> ResExpr(fun rn exp -> rn, exp)
                |> ResCheckDone

            parse
            |> Result.bind (fun (rd, exp) ->
                eval ls.SymTab exp
                |> Result.map (fun lv -> LDREQUAL(rd, lv)))

            |> (fun ins -> copyParse ls ins pCond)
            |> (fun pa -> { pa with PStall = 2 })



        let parseSingle (lsType : LSType) (uRoot : string) (uSuffix : string) pCond : Parse<Instr> =

            let mapOptHeadResult x =
                let f (r, txt) = Result.map x r, txt
                Option.map f

            let mSize, signed =
                match uSuffix with
                | "" -> MWord, false
                | "B" -> MByte, false
                | "H" -> MHalf, false
                | "SB" -> MByte, true
                | "SH" -> MHalf, true
                | _ -> failwithf "What? Suffix '%s' on LDR or STR is not possible" uSuffix
            /// LDRH, LDRSB, LDRSH and STRH use ARMv7's addressing mode 3:
            /// immediate offsets up to 255, register offsets without a shift
            let mode3 = mSize = MHalf || signed

            /// matches "!" or "" at end of memory instruction
            /// removes initial whitespace
            let (|WRITEBACK|_|) (txt : string) =
                match trim txt with
                | "!" -> Some true
                | "" -> Some false
                | _ -> None

            let (|IMMEXPR|_|) txt =
                match removeWs txt with
                | REMOVEPREFIX "#" (Expr(ast, txt)) ->
                    match eval ls.SymTab ast with
                    | Ok uint32 -> Some(uint32, txt)
                    | _ -> None
                | _ -> None

            let (|ImmExprUnresolved|_|) txt =
                match removeWs txt with
                | REMOVEPREFIX "#" (Expr(ast, txt)) ->
                    match eval ls.SymTab ast with
                    | Error e -> Some e
                    | _ -> None
                | _ -> None

            /// matches the offset part of an addressing mode (ARMv7)
            /// removes initial whitespace
            /// "" -> no offset.
            /// ", #n" -> immediate offset: -4095..4095 (-255..255 for H, SB, SH).
            /// ", Rm" or ", -Rm" or ", +Rm" -> register offset, added or subtracted.
            /// ", Rm, LSL #n" (or LSR, ASR, ROR #n, or RRX) -> scaled register offset (not for H, SB, SH).
            /// Returns (offset gen function, restOfTxt)
            let (|OFFSET|_|) (txt : string) =
                let (|COMMA|_|) = (|REMOVEPREFIX|_|) ","
                let (|R|_|) = (|REGMATCH|_|)
                /// optional sign before a register offset: true = subtract
                let (|SIGNED|_|) (txt : string) =
                    match trim txt with
                    | REMOVEPREFIX "-" s -> Some(true, s)
                    | REMOVEPREFIX "+" s -> Some(false, s)
                    | s -> Some(false, s)
                let (|SHIFT|_|) (txt : string) =
                    match txt with
                    | REMOVEPREFIXUNCASED "ASR" s -> (SASR, s) |> Some
                    | REMOVEPREFIXUNCASED "LSR" s -> (SLSR, s) |> Some
                    | REMOVEPREFIXUNCASED "LSL" s -> (SLSL, s) |> Some
                    | REMOVEPREFIXUNCASED "ROR" s -> (SROR, s) |> Some
                    | _ -> None
                let (|MIMM|_|) immTxt =
                    let bMax = if mode3 then 255 else 4095
                    match immTxt with
                    | IMMEXPR(n, txt) when int n <= bMax && int n >= -bMax -> (Ok n, txt) |> Some
                    | IMMEXPR(n, txt) ->
                        (makeParseError (sprintf "immediate offset in range -%d..%d" bMax bMax) ("offset=" + (int32 n).ToString()) "ea", txt) |> Some
                    | ImmExprUnresolved e -> Some(Error e, txt)
                    | LITERALNUMB(_, txt) -> (makeFormatError "Numeric offset in LDR/STR must have # prefix (#1000)" immTxt "ea", txt) |> Some
                    | _ -> None
                /// shift amounts allowed in an addressing mode: ARMv7's ranges, plus #0 (no shift),
                /// which is also accepted for data-processing shifts
                let shiftRange = function | SLSL -> 0u, 31u | SLSR | SASR -> 0u, 32u | SROR -> 0u, 31u | SRRX -> 0u, 0u
                /// value of register rx after the shift (shifts by 32 handled explicitly: .NET and JS mask the count)
                let scaled (rx : RName) (shift : ScaledShiftCode) (n : uint32) (dp : DataPath) : uint32 =
                    let x = dp.Regs.[rx]
                    match shift with
                    | SLSL -> x <<< int n
                    | SLSR -> if n = 32u then 0u else x >>> int n
                    | SASR -> uint32 ((int32 x) >>> (if n = 32u then 31 else int n))
                    | SROR -> (x >>> int n) ||| (x <<< (32 - int n))
                    | SRRX -> (x >>> 1) ||| (if dp.Fl.C then 0x80000000u else 0u)
                /// register offset rx (subtracted if neg), with an optional shift in rest
                let regOffset (neg : bool) (rx : RName) (rest : string) =
                    let make shift n =
                        Ok(fun (dp : DataPath) -> let v = int32 (scaled rx shift n dp) in if neg then -v else v)
                    let noShift () =
                        makeParseError (sprintf "an offset of #imm or a register: %s%s does not allow a shifted register" uRoot uSuffix) rest ""
                    match trim rest with
                    | COMMA(REMOVEPREFIXUNCASED "RRX" s) when mode3 -> Some(noShift (), s)
                    | COMMA(REMOVEPREFIXUNCASED "RRX" s) -> Some(make SRRX 0u, s)
                    | COMMA(SHIFT(_, s)) when mode3 -> Some(noShift (), s)
                    | COMMA(SHIFT(shift, IMM(n, s))) ->
                        let lo, hi = shiftRange shift
                        if n >= lo && n <= hi then Some(make shift n, s)
                        else Some(makeParseError (sprintf "%s shift amount in range #%d..#%d" ((sprintf "%A" shift).Substring 1) lo hi) (sprintf "#%d" (int n)) "ea", s)
                    | s when not (s.StartsWith ",") -> Some(make SLSL 0u, s)
                    | s -> Some(makeParseError "a shift: LSL, LSR, ASR or ROR with #n, or RRX" s "ea", s)
                match trim txt with
                | "" -> Some(Ok(fun _ -> 0), "")
                | COMMA(MIMM(offset, s)) ->
                    let makeOffsetFunc (offst : uint32) (dp : DataPath) = int offst
                    (Result.map makeOffsetFunc offset, s) |> Some
                | COMMA(SIGNED(neg, R(rx, rest))) -> regOffset neg rx rest
                | _ -> Some (makeParseError "valid memory offset: #n, Rm, -Rm, or Rm with a shift (Rm, LSL #n)"
                                        ("invalid:'" + txt + "'") "list#single-memory-tranfer-instructions", txt)
                // include base register value as second parameter of returned function
                |> mapOptHeadResult (fun fo dp rbv -> fo dp + rbv)

            match ls.Operands with
            | _ when lsType = STORE && signed ->
                makeParseError (sprintf "STRB or STRH: STR%s does not exist, because a store is never signed" uSuffix) ls.OpCode ""
            | REGMATCH(rd, (REMOVEPREFIX "," txt)) ->
                match PreIndex, PostIndex, txt with
                | indexType, _, BRACKETED '[' ']' (REGMATCH(rb, (OFFSET(spf, ""))), WRITEBACK w)
                | _, indexType, BRACKETED '[' ']' ((REGMATCH(rb, ""), OFFSET(spf, WRITEBACK w))) ->
                    let mode =
                        match w, indexType with
                        | false, PreIndex -> Ok NoIndex
                        | true, PostIndex -> makeParseError "Valid addressing mode" (" '!' is not valid in post-increment addressing:'" + txt + "'") "ea"
                        | _, it -> Ok it
                    match mode, spf with
                    | _, Error e -> Error e
                    | Error e, _ -> Error e
                    | Ok mode', Ok spf' ->
                        Ok {
                            LSType = lsType
                            Rd = rd
                            Rb = rb
                            MAddr = spf'
                            MemMode = mode'
                            MemSize = mSize
                            Signed = signed
                        }
                | _ -> makeParseError "LDR/STR Effective address" txt "list#single-register-memory-transfer-instructions"
            | _ -> makeParseError "LDR/STR register name" ls.Operands "list#single-register-memory-transfer-instructions"
            |> fun ins -> copyParse ls (Result.map memTypeSingleMap.[uRoot] ins) pCond
            |> (fun pa -> { pa with PStall = match uRoot with | "LDR" -> 2 | "STR" -> 1 | _ -> 0 })


        /// parse for LDM, STM (operands: "Rn{!}, {reglist}")
        let parseMult (root : string) suffix pCond (operands : string) : Parse<Instr> =

            /// For matching the list of regs
            let (|RegListMatch|_|) (str : string) =
                match str.ToUpper() with
                | ParseRegex "([A-Z]+[0-9]*)}" address -> address |> Some
                | ParseRegex "\[([A-Z]+[0-9]*)" address -> address |> Some
                | _ -> None

            /// Regex match the numbers in a hyphen list {r1 - r7}
            /// in order to construct full reg list.
            /// return the two numbers as low, high
            let (|RegListExpand|_|) (str : string) =
                match str.ToUpper() with
                | ParseRegex2 "([A-Z]+[0-9]*)-([A-Z]+[0-9]*)" (low, high) when regNames.ContainsKey low && regNames.ContainsKey high ->
                    (regNums.[regNames.[low]], regNums.[regNames.[high]]) |> Some
                | _ -> None

            /// Matches the registers
            let (|RegListMatch|_|) (str : string) =
                /// nice function to make register names from the
                /// high and low values
                /// {r2-r7} -> 2, 7 -> R2,R3,R4,R5,R6,R7
                let optionNumToRegList n =
                    match n with
                    | RegListExpand(low, high) ->
                        let fullRegList = List.map (fun r -> r |> makeRegFn) [ int low..int high ]
                        fullRegList |> Some
                    | _ -> None

                let optionMakeList n =
                    [ n ] |> Some

                match str.ToUpper() with
                | ParseRegex "(([A-Z]+[0-9]*)-([A-Z]+[0-9]*))" listReg -> optionNumToRegList listReg
                | ParseRegex "([A-Z]+[0-9]*)!" bangReg -> bangReg |> optionMakeList
                | ParseRegex "([A-Z]+[0-9]*)" reg -> reg |> optionMakeList
                | _ -> None

            /// split the operands at a {
            let splitMult = splitAny operands '{'

            let checkMultSuffix = function
                | "IA" -> IA |> Ok
                | "IB" -> IB |> Ok
                | "DA" -> DA |> Ok
                | "DB" -> DB |> Ok
                | "FD" -> FD |> Ok
                | "ED" -> ED |> Ok
                | "FA" -> FA |> Ok
                | "EA" -> EA |> Ok
                | "" -> IA |> Ok
                | x -> makeParseError "Valid LDM/STM suffix IA,IB,DA,DB,FD,ED,FA,EA" x "list#multiple-register-memory-transfer-instructions"

            let ops =
                match true, false, operands with
                | wb, _, REGMATCH(rOp1, (REMOVEPREFIX "!" (REMOVEPREFIX "," (BRACKETED '{' '}' (rl, TRIM "")))))
                | _, wb, REGMATCH(rOp1, (REMOVEPREFIX "," (BRACKETED '{' '}' (rl, TRIM "")))) ->

                    let regList = splitAny rl ','

                    let matcher = function
                        | RegListMatch x -> x
                        | x -> [ x ]

                    let checker = function
                        | RegCheck x -> x
                        | _ -> failwith alwaysMatchesFM

                    let checkedRegs =
                        regList
                        |> List.collect matcher
                        |> List.map checker
                        |> condenseResultList (id)
                        |> Result.bind (fun lst ->
                            let makeListError wanted = makeParseError wanted (String.concat "," regList) ""
                            let lst' = List.distinct lst
                            if lst.Length <> lst'.Length
                            then makeListError "Register list without duplicates"
                            elif List.contains rOp1 lst then makeListError <| sprintf "Register list not containing index register '%A'" rOp1
                            else Ok lst')
                        |> Result.map (List.distinct >> List.sortBy (fun rn -> rn.RegNum))


                    checkedRegs
                    |> Result.map (consMemMult wb rOp1)
                    |> mapErrorApplyResult (checkMultSuffix suffix)
                | _ ->
                    makeParseError "valid LDM/STM operands" operands "list#single-register-memory-transfer-instructions"

            copyParse ls (Result.map memTypeMultMap.[root] ops) pCond
            |> (fun pa -> { pa with PStall = let rootStall = match root with | "LDM" -> 1 | _ -> 0
                                             match ops with | Ok o -> (max 1 o.rList.Length) + rootStall | _ -> 0 })

        let parse' (_instrC, (root : string, suffix : string, pCond)) =
            let uRoot = root.ToUpper()
            let uSuffix = suffix.ToUpper()
            let singleSuffix = match uSuffix with | "" | "B" | "H" | "SB" | "SH" -> true | _ -> false
            /// PUSH/POP: register list in braces only, base is always SP with writeback
            let parseStack mRoot =
                match ls.Operands.Trim() with
                | ops when ops.StartsWith "{" -> parseMult mRoot "FD" pCond ("R13!, " + ops)
                | ops -> copyParse ls (makeParseError (sprintf "%s {register-list}" uRoot) ops "") pCond
            match singleSuffix, uRoot with
            | true, "LDR" when String.exists ((=) '=') ls.Operands && uSuffix = ""
                -> parseLoad32 pCond
            | true, "LDR" -> parseSingle LOAD uRoot uSuffix pCond
            | true, "STR" -> parseSingle STORE uRoot uSuffix pCond
            | _, "LDM" -> parseMult uRoot uSuffix pCond ls.Operands
            | _, "STM" -> parseMult uRoot uSuffix pCond ls.Operands
            | _, "PUSH" -> parseStack "STM"
            | _, "POP" -> parseStack "LDM"
            | _ -> failwithf "What? We appear to have an impossible memory root and suffix: %s %s" root suffix

        Map.tryFind (uppercase ls.OpCode) opCodes
        |> Option.map parse'

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse

//*******************************************************************************************
//                                      Memory execution
//*******************************************************************************************

    let getDataMemWord (ef : uint32) (dp : DataPath) =
        if ef % 4u <> 0u then
            ``Run time error`` (ef, (sprintf "word address %d must be divisible by 4" ef)) |> Error
        else
            match Map.tryFind (WA ef) dp.MM with
            | Some CodeSpace ->
                ``Run time error`` (ef, (sprintf "Memory read of code memory location %x is not allowed" ef)) |> Error
            | Some(Dat d) -> Ok d
            | None -> Ok 0u

    let getDataMemByte (ef : uint32) (dp : DataPath) =
        let wordA = (ef &&& 0xFFFFFFFCu)
        let bitOffset = (ef &&& 0x3u) * 8u |> int
        getDataMemWord wordA dp
        |> Result.map (fun w -> (w >>> bitOffset) &&& 0xFFu)

    /// Load a halfword (little-endian): the address must be divisible by 2
    let getDataMemHalf (ef : uint32) (dp : DataPath) =
        if ef % 2u <> 0u then
            ``Run time error`` (ef, (sprintf "halfword address %d must be divisible by 2" ef)) |> Error
        else
            getDataMemWord (ef &&& 0xFFFFFFFCu) dp
            |> Result.map (fun w -> (w >>> int ((ef &&& 2u) * 8u)) &&& 0xFFFFu)

    /// Store the low halfword of value (little-endian): the address must be divisible by 2
    let updateMemHalf (value : uint32) (ef : uint32) (dp : DataPath) =
        if ef % 2u <> 0u then
            ``Run time error`` (ef, (sprintf "halfword address %d must be divisible by 2" ef)) |> Error
        else
            updateMemByte (value &&& 0xFFu |> byte) ef dp
            |> Result.bind (updateMemByte ((value >>> 8) &&& 0xFFu |> byte) (ef + 1u))

    /// Sign-extend the low 8 or 16 bits of a loaded value
    let signExtend (size : MSize) (v : uint32) =
        match size with
        | MByte -> uint32 (int32 (sbyte (v &&& 0xFFu)))
        | MHalf -> uint32 (int32 (int16 (v &&& 0xFFFFu)))
        | MWord -> v

    let executeLDRSTR (ins : InstrMemSingle) (dp : DataPath) =
        let addr = ins.MAddr dp (int32 dp.Regs.[ins.Rb]) |> uint32
        let rbv = dp.Regs.[ins.Rb]
        let ef =
            match ins.MemMode with
            | NoIndex
            | PreIndex -> uint32 addr
            | PostIndex -> rbv
        //printfn "Executing Rb=%A(%x) addr = %x  ef = %x" ins.Rb dp.Regs.[ins.Rb] addr ef
        dp
        |> updateReg (uint32 (if ins.MemMode = NoIndex then (match ins.Rb with | R15 -> rbv - 4u | _ -> rbv) else addr >>> 0)) ins.Rb
        |> (fun dp ->
                match ins.LSType with
                | LOAD ->
                     match ins.MemSize with
                     | MWord -> (getDataMemWord ef dp)
                     | MByte -> (getDataMemByte ef dp)
                     | MHalf -> (getDataMemHalf ef dp)
                     |> Result.map (fun dat -> updateReg (if ins.Signed then signExtend ins.MemSize dat else dat) ins.Rd dp)
                | STORE ->
                    match ins.MemSize with
                    | MWord -> updateMemData (Dat dp.Regs.[ins.Rd]) ef dp
                    | MByte -> updateMemByte (dp.Regs.[ins.Rd] |> byte) ef dp
                    | MHalf -> updateMemHalf dp.Regs.[ins.Rd] ef dp)

    let offsetList start suffix rl wb isLDM =
        let rec makeOffsetList inlst outlist incr start =
            match inlst with
            | _ :: tail -> (start + incr) |> makeOffsetList tail (start :: outlist) incr
            | [] -> outlist
        let lst, rDiff =
            match suffix with
            | IA -> 0, 4
            | IB -> 4, 4
            | DA -> 0, -4
            | DB -> -4, -4
            | _ ->
                match suffix, isLDM with
                | FD, true | EA, false -> 0, 4
                | ED, true | FA, false -> 4, 4
                | FA, true | ED, false -> 0, -4
                | EA, true | FD, false -> -4, -4
                | _ -> failwithf "What? Cannot happen!"
            |> fun (st, chg) ->
                makeOffsetList rl [] chg (start + st)
                |> if chg < 0 then id else List.rev
                |> (fun lst -> lst, (if wb then chg * lst.Length else 0))

        List.map (fun el -> el |> uint32) lst, rDiff

    let executeMem instr (cpuData : DataPath) =
        /// get multiple memory
        let rec getMemMult addrList contentsLst cpuData =
            match addrList with
            | head :: tail ->
                let addedVal = (getDataMemWord head cpuData) :: contentsLst
                getMemMult tail addedVal cpuData
            | [] -> contentsLst |> List.rev

        let executeLDM wb suffix rn rl cpuData =
            let baseAddrInt = (cpuData.Regs.[rn]) |> int32
            let lst, rEnd = offsetList baseAddrInt suffix rl wb true
            let contents = getMemMult lst [] cpuData
            let condensedContents = condenseResultList (id) contents
            Result.map (fun conts -> setMultRegs rl conts cpuData) condensedContents
            |> Result.map (fun dp -> setReg rn ((dp.Regs.[rn] + uint32 rEnd) &&& 0xFFFFFFFFu) dp)

        let executeSTM wb suffix rn rl cpuData =
            let getReg rn = cpuData.Regs.[rn]
            let baseAddrInt = (getReg rn) |> int32
            let regContentsList = List.map (getReg) rl
            let lst, rEnd = offsetList baseAddrInt suffix rl wb false
            setMultMem (regContentsList |> List.map Dat) lst cpuData
            |> Result.map (fun dp -> setReg rn ((dp.Regs.[rn] + uint32 rEnd) &&& 0xFFFFFFFFu) dp)

        match instr with
        | LDR ins | STR ins -> executeLDRSTR ins cpuData
        | LDM operands ->
            executeLDM operands.WB operands.suff operands.Rn operands.rList cpuData
        | STM operands ->
            executeSTM operands.WB operands.suff operands.Rn operands.rList cpuData
        | LDREQUAL(rn, loadVal) -> setReg rn loadVal cpuData |> Ok
