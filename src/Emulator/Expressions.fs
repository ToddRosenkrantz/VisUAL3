(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Emulator.Expressions
    Description: Parse expressions with constants, symbols, and arithmetic
*)


/// Assembler code expressions (symbols, operators, and constants)
module Expressions
    open System.Text.RegularExpressions
    open Errors
    open EEExtensions


    /// Match the start of txt with pat
    /// Return a tuple of the matched text and the rest
    let (|RegexPrefix|_|) pat txt =
        // Match from start, ignore whitespace
        let m = Regex.Match(txt, "^[\\s]*" + pat + "[\\s]*")
        match m.Success with
        | true -> (m.Value, txt.Substring(m.Value.Length)) |> Some
        | false -> None


    /// Remove all whitespace from a matched string
    let removeWs txt = Regex.Replace(txt, "[\\s]*", "")

    /// GNU as symbol characters: letters, digits, '_', '.', '$' (not starting with a digit)
    let isSymbolStart (c : char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '.' || c = '$'
    let isSymbolChar (c : char) = isSymbolStart c || (c >= '0' && c <= '9')

    /// Active pattern for matching labels
    /// Also removes any whitespace from around the label
    let (|LabelExpr|_|) txt =
        match txt with
        | RegexPrefix "[a-zA-Z_.$][a-zA-Z0-9_.$]*" (var, rst) ->
            (// Remove whitespace from the label
            removeWs var, rst) |> Some
        | _ -> None



    type SymbolTable = Map<string, uint32>

    /// Expression AST. Operators are GNU as operator strings.
    type Expression =
        | BinOp of string * Expression * Expression
        | UnOp of string * Expression
        | Label of string
        | Literal of uint32

    type Resolvable = Result<uint32, ParseError>


    /// Evaluate exp against the symbol table syms in 64-bit arithmetic, like GNU as.
    /// Returns a list of all errors or the (untruncated) result
    let evalI64 (syms : Map<string, uint32>) exp : Result<int64, ErrCode> =
        let joinErrors a b =
            match a, b with
            | ``Undefined symbol`` a', ``Undefined symbol`` b' ->
                ``Undefined symbol`` (a' @ b') |> Error
            | ``Undefined symbol`` _, a'
            | a', _ -> a' |> Error
        let getSymError x =
            let x' = String.toUpper x
            let symLst = syms |> Map.toList |> List.map fst
            match List.tryFind (fun sym -> x' = String.toUpper sym) symLst with
            | Some sym -> x, sprintf "'%s' which has different case from label '%s'" x sym
            | None -> x, sprintf "'%s' which is not defined as a label" x
        let applyBin op (a : int64) (b : int64) =
            match op with
            | "+" -> Ok(a + b)
            | "-" -> Ok(a - b)
            | "*" -> Ok(a * b)
            | "/" | "%" when b = 0L -> ``Invalid instruction`` "Division by zero in expression" |> Error
            | "/" -> Ok(a / b)
            | "%" -> Ok(a % b)
            | "<<" -> Ok(if b < 0L || b > 63L then 0L else a <<< int b)
            | ">>" -> Ok(if b < 0L || b > 63L then (if a < 0L then -1L else 0L) else a >>> int b)
            | "&" -> Ok(a &&& b)
            | "|" -> Ok(a ||| b)
            | "^" -> Ok(a ^^^ b)
            | _ -> failwithf "What? unknown operator %s" op
        let rec eval' exp : Result<int64, ErrCode> =
            match exp with
            | Literal x -> Ok(int64 x)
            | Label x ->
                match Map.tryFind x syms with
                | Some v -> Ok(int64 v)
                | None -> ``Undefined symbol`` [ getSymError x ] |> Error
            | UnOp(op, x) ->
                eval' x |> Result.map (fun v -> match op with | "-" -> -v | "~" -> ~~~v | _ -> v)
            | BinOp(op, x, y) ->
                match eval' x, eval' y with
                | Ok a, Ok b -> applyBin op a b
                | Error a, Error b -> joinErrors a b
                | Error a, _ -> Error a
                | _, Error b -> Error b
        eval' exp

    /// Evaluate exp against the symbol table syms: 64-bit arithmetic truncated to 32 bits
    let eval (syms : Map<string, uint32>) exp : Result<uint32, ErrCode> =
        evalI64 syms exp |> Result.map (fun v -> uint32 (v &&& 0xFFFFFFFFL))


    /// Value of a backslash escape in a string or character literal (GNU as rules)
    /// s.[i] is the character after '\'. Returns (value, index after escape).
    let parseEscape (s : string) (i : int) : (uint32 * int) option =
        let isOct c = c >= '0' && c <= '7'
        let hexVal (c : char) =
            match c with
            | c when c >= '0' && c <= '9' -> Some(uint32 c - uint32 '0')
            | c when c >= 'a' && c <= 'f' -> Some(uint32 c - uint32 'a' + 10u)
            | c when c >= 'A' && c <= 'F' -> Some(uint32 c - uint32 'A' + 10u)
            | _ -> None
        if i >= s.Length then None
        else
            match s.[i] with
            | 'n' -> Some(10u, i + 1)
            | 't' -> Some(9u, i + 1)
            | 'r' -> Some(13u, i + 1)
            | 'b' -> Some(8u, i + 1)
            | 'f' -> Some(12u, i + 1)
            | 'x' | 'X' ->
                let mutable j = i + 1
                let mutable v = 0u
                while j < s.Length && (hexVal s.[j]).IsSome do
                    v <- (v * 16u + (hexVal s.[j]).Value) &&& 0xFFu
                    j <- j + 1
                if j = i + 1 then None else Some(v, j)
            | c when isOct c ->
                let mutable j = i
                let mutable v = 0u
                while j < s.Length && j < i + 3 && isOct s.[j] do
                    v <- v * 8u + (uint32 s.[j] - uint32 '0')
                    j <- j + 1
                Some(v &&& 0xFFu, j)
            | c -> Some(uint32 c, i + 1) // \\ \" \' and any other character stand for themselves


    /// Active pattern for matching GNU as expressions.
    /// Returns an Expression AST and the text after the expression.
    /// Operator precedence (as GNU as): unary - ~ +, then * / % << >>, then | & ^, then + -
    let (|Expr|_|) (s : string) : (Expression * string) option =
        let n = s.Length
        let rec skipWs i = if i < n && System.Char.IsWhiteSpace s.[i] then skipWs (i + 1) else i
        let isDigit c = c >= '0' && c <= '9'

        /// numeric literal starting at i (s.[i] is a digit)
        let number i =
            let digitsFrom j pred =
                let mutable k = j
                while k < n && pred s.[k] do k <- k + 1
                k
            let isHex (c : char) = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
            let radix, first, last =
                if i + 1 < n && s.[i] = '0' && (s.[i + 1] = 'x' || s.[i + 1] = 'X') then
                    16, i + 2, digitsFrom (i + 2) isHex
                elif i + 1 < n && s.[i] = '0' && (s.[i + 1] = 'b' || s.[i + 1] = 'B') && i + 2 < n && (s.[i + 2] = '0' || s.[i + 2] = '1') then
                    2, i + 2, digitsFrom (i + 2) (fun c -> c = '0' || c = '1')
                elif s.[i] = '0' && i + 1 < n && isDigit s.[i + 1] then
                    8, i + 1, digitsFrom (i + 1) isDigit // leading 0 is octal in GNU as
                else
                    10, i, digitsFrom i isDigit
            if last = first || (last < n && isSymbolChar s.[last]) then None // e.g. 0x, 12abc, 09
            else
                let digitVal (c : char) =
                    if isDigit c then int64 c - int64 '0'
                    elif c >= 'a' then int64 c - int64 'a' + 10L
                    else int64 c - int64 'A' + 10L
                let rec acc k (v : int64) =
                    if k = last then Some v
                    else
                        let d = digitVal s.[k]
                        let v' = v * int64 radix + d
                        if d >= int64 radix || v' > 0xFFFFFFFFL then None else acc (k + 1) v'
                acc first 0L |> Option.map (fun v -> Literal(uint32 v), last)

        /// character literal 'c or 'c' (s.[i] = '\'')
        let charLit i =
            if i + 1 >= n then None
            else
                let v =
                    if s.[i + 1] = '\\' then parseEscape s (i + 2)
                    else Some(uint32 s.[i + 1], i + 2)
                v |> Option.map (fun (v, j) -> Literal v, (if j < n && s.[j] = '\'' then j + 1 else j))

        let rec primary i =
            let i = skipWs i
            if i >= n then None
            else
                match s.[i] with
                | '-' | '~' | '+' as op -> primary (i + 1) |> Option.map (fun (e, j) -> UnOp(string op, e), j)
                | '(' ->
                    match level3 (i + 1) with
                    | Some(e, j) ->
                        let j = skipWs j
                        if j < n && s.[j] = ')' then Some(e, j + 1) else None
                    | None -> None
                | '\'' -> charLit i
                | c when isDigit c -> number i
                | c when isSymbolStart c ->
                    let mutable j = i
                    while j < n && isSymbolChar s.[j] do j <- j + 1
                    Some(Label(s.Substring(i, j - i)), j)
                | _ -> None

        /// binary operator at i from ops (longest first), returns (op, index after op)
        and binOp ops i =
            let i = skipWs i
            ops |> List.tryFind (fun (op : string) -> i + op.Length <= n && s.Substring(i, op.Length) = op)
                |> Option.map (fun op -> op, i + op.Length)

        and leftAssoc next ops i =
            let rec loop (lhs, j) =
                match binOp ops j with
                | Some(op, k) ->
                    match next k with
                    | Some(rhs, k') -> loop (BinOp(op, lhs, rhs), k')
                    | None -> None
                | None -> Some(lhs, j)
            next i |> Option.bind loop

        and level1 i = leftAssoc primary [ "<<"; ">>"; "*"; "/"; "%" ] i
        and level2 i = leftAssoc level1 [ "|"; "&"; "^" ] i
        and level3 i = leftAssoc level2 [ "+"; "-" ] i

        match level3 0 with
        | Some(e, j) -> Some(e, s.Substring(skipWs j))
        | None -> None


    let printUintRes r =
        match r with
        | Ok u -> printfn "OK %d" u; r
        | Error(code, eTxt, eMess) -> printfn "Error:<%s><%s>" eTxt eMess; r

    /// Evaluate op, which must be a complete expression
    let parseEvalNumericExpression syms (op : string) =
        match op.Trim() with
        | Expr(ast, "") -> eval syms ast
        | Expr(_, rest) -> makeFormatError "Unexpected characters after expression" rest ""
        | _ when String.contains "#" op -> makeParseError "Numeric expression (without #)" op ""
        | _ -> makeParseError "Numeric expression" op ""

    let parse syms (op : string) =
        match op.Trim() with
        | Expr(ast, txt) -> Result.map (fun e -> e, txt) (eval syms ast)
        | _ when String.contains "#" op -> makeParseError "Numeric expression (without #)" op ""
        | _ -> makeParseError "Numeric expression" op ""


    type PartsOfASM = | ALabel | AOpCode | AOperand of int

    /// Code to implement accurate error reporting
    /// Malformed lines will be best effort parsed into parts
    /// Each part will be returned with its position in the original line
    let getASMPart symTab isOpCode (part : PartsOfASM) (line : string) =
        let isSymbol s = Map.containsKey s symTab
        let pack (thing : string) (restOfLine : string) =
            let n = line.Length
            let r = restOfLine.Length
            let ePos = n - r + 1
            let sPos = ePos - thing.Length
            thing, sPos, ePos
        match part, line with
        | ALabel, LabelExpr(lab, rst) when isOpCode lab |> not -> pack lab rst
        | _ -> failwithf "Not implemented"

