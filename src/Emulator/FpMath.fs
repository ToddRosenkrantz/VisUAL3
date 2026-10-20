/// IEEE 754 arithmetic for the VFP, done exactly with bigint and then rounded once.
/// Results and FPSCR flags are bit-identical on .NET and in Fable's JavaScript,
/// which has no single precision of its own. Values are raw bit patterns (bigint).
module FpMath

open Fable.Core

/// An IEEE binary format
type FpFormat = { P : int; FracBits : int; ExpBits : int; Bias : int; EMin : int; EMax : int }

let F16 = { P = 11; FracBits = 10; ExpBits = 5; Bias = 15; EMin = -14; EMax = 15 }
let F32 = { P = 24; FracBits = 23; ExpBits = 8; Bias = 127; EMin = -126; EMax = 127 }
let F64 = { P = 53; FracBits = 52; ExpBits = 11; Bias = 1023; EMin = -1022; EMax = 1023 }

type RoundMode = | RN | RP | RM | RZ

/// FPSCR cumulative exception bits
let IOC = 0x01u
let DZC = 0x02u
let OFC = 0x04u
let UFC = 0x08u
let IXC = 0x10u
let IDC = 0x80u

/// The FPSCR controls that affect arithmetic
type FpCtl = { Mode : RoundMode; FZ : bool; DN : bool }

let defaultCtl = { Mode = RN; FZ = false; DN = false }

let ctlOfFpscr (fpscr : uint32) =
    { Mode = (match (fpscr >>> 22) &&& 3u with | 0u -> RN | 1u -> RP | 2u -> RM | _ -> RZ)
      FZ = fpscr &&& 0x01000000u <> 0u
      DN = fpscr &&& 0x02000000u <> 0u }

/// Result bits and the exceptions raised
type FpRes = { Bits : bigint; Flags : uint32 }

// ---------------------------------------------------------------- bigint helpers

let pow2 (n : int) = bigint.Pow(2I, n)
let private two32 = pow2 32
let private mask32 = two32 - 1I

/// number of bits in m (m >= 0)
let bitLength (m : bigint) =
    let rec chunks n (x : bigint) = if x >= two32 then chunks (n + 32) (x >>> 32) else bits n x
    and bits n (x : bigint) = if x = 0I then n else bits (n + 1) (x >>> 1)
    chunks 0 m

/// floor of the square root of n (n >= 0)
let isqrt (n : bigint) =
    if n < 2I then n
    else
        let rec go (x : bigint) =
            let y = (x + n / x) / 2I
            if y >= x then x else go y
        go (pow2 ((bitLength n + 1) / 2 + 1))

let rawOfWord (w : uint32) = bigint (int64 w)
let rawOfWords (lo : uint32) (hi : uint32) = bigint (int64 lo) + (bigint (int64 hi) <<< 32)
let wordOfRaw (r : bigint) = uint32 (r &&& mask32)
let wordsOfRaw (r : bigint) = uint32 (r &&& mask32), uint32 ((r >>> 32) &&& mask32)

// ---------------------------------------------------------------- encoding

let private signBit fmt = pow2 (fmt.FracBits + fmt.ExpBits)
let private expAllOnes fmt = (pow2 fmt.ExpBits - 1I) <<< fmt.FracBits
let private withSign fmt neg (r : bigint) = if neg then r + signBit fmt else r

let fields fmt (raw : bigint) =
    let neg = (raw >>> (fmt.FracBits + fmt.ExpBits)) &&& 1I = 1I
    let exp = int ((raw >>> fmt.FracBits) &&& (pow2 fmt.ExpBits - 1I))
    let frac = raw &&& (pow2 fmt.FracBits - 1I)
    neg, exp, frac

let private maxExp fmt = (1 <<< fmt.ExpBits) - 1
let isNaN fmt raw = let _, e, f = fields fmt raw in e = maxExp fmt && f <> 0I
let isSNaN fmt raw = isNaN fmt raw && (raw &&& pow2 (fmt.FracBits - 1)) = 0I
let isQNaN fmt raw = isNaN fmt raw && not (isSNaN fmt raw)
let isNeg fmt raw = let n, _, _ = fields fmt raw in n
let quietNaN fmt (raw : bigint) = raw ||| pow2 (fmt.FracBits - 1)
let defaultNaN fmt = expAllOnes fmt + pow2 (fmt.FracBits - 1)
let infBits fmt neg = withSign fmt neg (expAllOnes fmt)
let zeroBits fmt neg = withSign fmt neg 0I
let private maxFiniteBits fmt neg = withSign fmt neg (expAllOnes fmt - 1I)
/// flip the sign bit (VNEG): no exceptions, NaNs included
let negBits fmt (raw : bigint) = raw ^^^ signBit fmt
/// clear the sign bit (VABS)
let absBits fmt (raw : bigint) = raw &&& (signBit fmt - 1I)

/// A decoded value: ±m * 2^e for finite non-zero values (m > 0)
type FpVal =
    | FZero of bool
    | FInf of bool
    | FNaN
    | FFin of bool * bigint * int

/// decode a value; with FZ, a subnormal input counts as zero and raises IDC
let decode ctl fmt raw =
    let neg, exp, frac = fields fmt raw
    if exp = maxExp fmt then (if frac = 0I then FInf neg else FNaN), 0u
    elif exp = 0 then
        if frac = 0I then FZero neg, 0u
        elif ctl.FZ then FZero neg, IDC
        else FFin(neg, frac, fmt.EMin - fmt.FracBits), 0u
    else FFin(neg, frac + pow2 fmt.FracBits, exp - fmt.Bias - fmt.FracBits), 0u

let private signOf = function | FZero n | FInf n | FFin(n, _, _) -> n | FNaN -> false

let private overflow ctl fmt neg =
    let toInf = match ctl.Mode with | RN -> true | RP -> not neg | RM -> neg | RZ -> false
    { Bits = (if toInf then infBits fmt neg else maxFiniteBits fmt neg); Flags = OFC ||| IXC }

/// round ±(m * 2^e) to fmt; sticky means there are non-zero bits below m.
/// Underflow is detected before rounding, as ARM does.
let round ctl fmt (neg : bool) (m0 : bigint) (e0 : int) (sticky : bool) : FpRes =
    let m, e = if sticky then m0 * 2I + 1I, e0 - 1 else m0, e0
    if m = 0I then { Bits = zeroBits fmt neg; Flags = 0u }
    else
        let len = bitLength m
        let exp = e + len - 1
        let tiny = exp < fmt.EMin
        if tiny && ctl.FZ then { Bits = zeroBits fmt neg; Flags = UFC }
        else
            let keep = if tiny then fmt.P - (fmt.EMin - exp) else fmt.P
            let shift = len - keep
            let q, inexact =
                if shift <= 0 then m, false
                else
                    let q0 = m >>> shift
                    let rem = m - (q0 <<< shift)
                    if rem = 0I then q0, false
                    else
                        let half = pow2 (shift - 1)
                        let up =
                            match ctl.Mode with
                            | RN -> rem > half || (rem = half && q0 % 2I = 1I)
                            | RP -> not neg
                            | RM -> neg
                            | RZ -> false
                        (if up then q0 + 1I else q0), true
            let flags = (if inexact then IXC else 0u) ||| (if tiny && inexact then UFC else 0u)
            let eq = e + max shift 0
            if q = 0I then { Bits = zeroBits fmt neg; Flags = flags }
            else
                // a carry out of the top bit adds one bit
                let q, eq = if bitLength q > fmt.P then q >>> 1, eq + 1 else q, eq
                let qlen = bitLength q
                let qexp = eq + qlen - 1
                if qexp > fmt.EMax then overflow ctl fmt neg
                elif qexp < fmt.EMin then
                    { Bits = withSign fmt neg (q <<< (eq - (fmt.EMin - fmt.FracBits))); Flags = flags }
                else
                    let frac = (q <<< (fmt.P - qlen)) - pow2 fmt.FracBits
                    let biased = bigint (qexp + fmt.Bias)
                    { Bits = withSign fmt neg ((biased <<< fmt.FracBits) + frac); Flags = flags }

let private invalid fmt = { Bits = defaultNaN fmt; Flags = IOC }
let private exact fmt bits = { Bits = bits; Flags = 0u }
let private addFlags fl (r : FpRes) = { r with Flags = r.Flags ||| fl }

/// ARM's NaN rules: the first signalling NaN (quietened, IOC), else the first quiet NaN.
/// With DN, the default NaN instead.
let processNaNs ctl fmt (raws : bigint list) : FpRes option =
    let pick =
        match List.tryFind (isSNaN fmt) raws with
        | Some s -> Some(quietNaN fmt s, IOC)
        | None -> List.tryFind (isNaN fmt) raws |> Option.map (fun q -> q, 0u)
    pick |> Option.map (fun (r, fl) -> { Bits = (if ctl.DN then defaultNaN fmt else r); Flags = fl })

/// the signed integer (scaled to exponent e) of a finite value
let private scaledTo e (n : bool, m : bigint, e1 : int) = (if n then -m else m) <<< (e1 - e)

/// exact sum of two finite non-zero values, rounded; an exact zero is +0 (-0 rounding down)
let private sumFinite ctl fmt (a : bool * bigint * int) (b : bool * bigint * int) =
    let (_, _, e1), (_, _, e2) = a, b
    let e = min e1 e2
    let s = scaledTo e a + scaledTo e b
    if s = 0I then exact fmt (zeroBits fmt (ctl.Mode = RM))
    else round ctl fmt (s < 0I) (abs s) e false

/// re-encode a finite value exactly
let private ofFin ctl fmt (n, m, e) = round ctl fmt n m e false

// ---------------------------------------------------------------- arithmetic

let add ctl fmt a b =
    match processNaNs ctl fmt [ a; b ] with
    | Some r -> r
    | None ->
        let va, fa = decode ctl fmt a
        let vb, fb = decode ctl fmt b
        match va, vb with
        | FInf n1, FInf n2 when n1 <> n2 -> invalid fmt
        | FInf n, _ | _, FInf n -> exact fmt (infBits fmt n)
        | FZero n1, FZero n2 -> exact fmt (zeroBits fmt (if n1 = n2 then n1 else ctl.Mode = RM))
        | FZero _, FFin(n, m, e) | FFin(n, m, e), FZero _ -> ofFin ctl fmt (n, m, e)
        | FFin(n1, m1, e1), FFin(n2, m2, e2) -> sumFinite ctl fmt (n1, m1, e1) (n2, m2, e2)
        | _ -> invalid fmt
        |> addFlags (fa ||| fb)

let sub ctl fmt a b =
    match processNaNs ctl fmt [ a; b ] with
    | Some r -> r
    | None -> add ctl fmt a (negBits fmt b)

let mul ctl fmt a b =
    match processNaNs ctl fmt [ a; b ] with
    | Some r -> r
    | None ->
        let va, fa = decode ctl fmt a
        let vb, fb = decode ctl fmt b
        let neg = signOf va <> signOf vb
        match va, vb with
        | FInf _, FZero _ | FZero _, FInf _ -> invalid fmt
        | FInf _, _ | _, FInf _ -> exact fmt (infBits fmt neg)
        | FZero _, _ | _, FZero _ -> exact fmt (zeroBits fmt neg)
        | FFin(_, m1, e1), FFin(_, m2, e2) -> round ctl fmt neg (m1 * m2) (e1 + e2) false
        | _ -> invalid fmt
        |> addFlags (fa ||| fb)

let div ctl fmt a b =
    match processNaNs ctl fmt [ a; b ] with
    | Some r -> r
    | None ->
        let va, fa = decode ctl fmt a
        let vb, fb = decode ctl fmt b
        let neg = signOf va <> signOf vb
        match va, vb with
        | FInf _, FInf _ | FZero _, FZero _ -> invalid fmt
        | FInf _, _ -> exact fmt (infBits fmt neg)
        | _, FInf _ | FZero _, _ -> exact fmt (zeroBits fmt neg)
        | FFin _, FZero _ -> { Bits = infBits fmt neg; Flags = DZC }
        | FFin(_, m1, e1), FFin(_, m2, e2) ->
            let k = max 0 (fmt.P + 2 + bitLength m2 - bitLength m1)
            let num = m1 <<< k
            let q = num / m2
            round ctl fmt neg q (e1 - e2 - k) (num - q * m2 <> 0I)
        | _ -> invalid fmt
        |> addFlags (fa ||| fb)

let sqrt ctl fmt a =
    match processNaNs ctl fmt [ a ] with
    | Some r -> r
    | None ->
        let va, fa = decode ctl fmt a
        match va with
        | FZero n -> exact fmt (zeroBits fmt n)
        | FInf false -> exact fmt (infBits fmt false)
        | FFin(false, m, e) ->
            let m, e = if e % 2 <> 0 then m <<< 1, e - 1 else m, e
            let k = max 0 (fmt.P + 3 - bitLength m / 2)
            let n = m <<< (2 * k)
            let s = isqrt n
            round ctl fmt false s ((e - 2 * k) / 2) (s * s <> n)
        | _ -> invalid fmt
        |> addFlags fa

/// fused addend + op1 * op2 with one rounding (ARM FPMulAdd)
let mulAdd ctl fmt addend op1 op2 =
    let vA, fA = decode ctl fmt addend
    let v1, f1 = decode ctl fmt op1
    let v2, f2 = decode ctl fmt op2
    let isInf = function | FInf _ -> true | _ -> false
    let isZero = function | FZero _ -> true | _ -> false
    let infZero = (isInf v1 && isZero v2) || (isZero v1 && isInf v2)
    let nanRes =
        match processNaNs ctl fmt [ addend; op1; op2 ] with
        | Some _ when isQNaN fmt addend && infZero -> Some(invalid fmt)
        | r -> r
    match nanRes with
    | Some r -> r
    | None ->
        let signP = signOf v1 <> signOf v2
        let infP = isInf v1 || isInf v2
        let zeroP = isZero v1 || isZero v2
        let signA = signOf vA
        match vA with
        | _ when infZero || (isInf vA && infP && signA <> signP) -> invalid fmt
        | FInf n -> exact fmt (infBits fmt n)
        | _ when infP -> exact fmt (infBits fmt signP)
        | FZero _ when zeroP -> exact fmt (zeroBits fmt (if signA = signP then signA else ctl.Mode = RM))
        | _ ->
            let product = match v1, v2 with | FFin(_, m1, e1), FFin(_, m2, e2) -> Some(signP, m1 * m2, e1 + e2) | _ -> None
            let addv = match vA with | FFin(n, m, e) -> Some(n, m, e) | _ -> None
            match product, addv with
            | Some p, Some a -> sumFinite ctl fmt a p
            | Some p, None | None, Some p -> ofFin ctl fmt p
            | None, None -> invalid fmt
        |> addFlags (fA ||| f1 ||| f2)

/// FPSCR NZCV for a compare: equal 0110, less 1000, greater 0010, unordered 0011.
/// VCMPE (quietNaNsSignal) also raises IOC for quiet NaNs.
let compare ctl fmt (quietNaNsSignal : bool) a b : uint32 * uint32 =
    if isNaN fmt a || isNaN fmt b then
        0x30000000u, (if isSNaN fmt a || isSNaN fmt b || quietNaNsSignal then IOC else 0u)
    else
        let va, fa = decode ctl fmt a
        let vb, fb = decode ctl fmt b
        /// -2 = -inf, -1 negative, 0 zero, 1 positive, 2 = +inf
        let rank = function
            | FZero _ -> 0 | FInf n -> (if n then -2 else 2) | FFin(n, _, _) -> (if n then -1 else 1) | FNaN -> 0
        let order =
            match va, vb with
            | FFin(n1, m1, e1), FFin(n2, m2, e2) when n1 = n2 ->
                let e = min e1 e2
                compare (scaledTo e (n1, m1, e1)) (scaledTo e (n2, m2, e2))
            | _ -> compare (rank va) (rank vb)
        let nzcv = if order = 0 then 0x60000000u elif order < 0 then 0x80000000u else 0x20000000u
        nzcv, fa ||| fb

// ---------------------------------------------------------------- conversions

/// convert between formats (vcvt.f64.f32, vcvt.f32.f64, vcvtb/vcvtt)
let convert ctl (src : FpFormat) (dst : FpFormat) (raw : bigint) : FpRes =
    // flush-to-zero does not apply to half precision
    let ctl = if src = F16 || dst = F16 then { ctl with FZ = false } else ctl
    if isNaN src raw then
        let fl = if isSNaN src raw then IOC else 0u
        if ctl.DN then { Bits = defaultNaN dst; Flags = fl }
        else
            let neg, _, frac = fields src raw
            let frac' =
                if dst.FracBits >= src.FracBits then frac <<< (dst.FracBits - src.FracBits)
                else frac >>> (src.FracBits - dst.FracBits)
            { Bits = quietNaN dst (withSign dst neg (expAllOnes dst + frac')); Flags = fl }
    else
        let v, fl = decode ctl src raw
        match v with
        | FZero n -> exact dst (zeroBits dst n)
        | FInf n -> exact dst (infBits dst n)
        | FFin(n, m, e) -> round ctl dst n m e false
        | FNaN -> invalid dst
        |> addFlags fl

/// float to a 16- or 32-bit integer with fbits fraction bits (vcvt, vcvtr): rounds toward
/// zero unless useMode, saturates out-of-range values and gives 0 for NaN (both raise IOC).
/// The result is sign- or zero-extended to 32 bits.
let toFixed ctl fmt (signed : bool) (size : int) (fbits : int) (useMode : bool) (raw : bigint) : uint32 * uint32 =
    let lo, hi = (if signed then -(pow2 (size - 1)) else 0I), (if signed then pow2 (size - 1) - 1I else pow2 size - 1I)
    let word (v : bigint) = uint32 ((if v < 0I then v + two32 else v) &&& mask32)
    if isNaN fmt raw then 0u, IOC
    else
        let v, fl = decode ctl fmt raw
        let mode = if useMode then ctl.Mode else RZ
        let value, inexact =
            match v with
            | FZero _ | FNaN -> 0I, false
            | FInf n -> (if n then lo - 1I else hi + 1I), false
            | FFin(n, m, e) ->
                let e' = e + fbits
                let mag, inexact =
                    if e' >= 0 then m <<< e', false
                    else
                        let q0 = m >>> -e'
                        let rem = m - (q0 <<< -e')
                        if rem = 0I then q0, false
                        else
                            let half = pow2 (-e' - 1)
                            let up =
                                match mode with
                                | RN -> rem > half || (rem = half && q0 % 2I = 1I)
                                | RP -> not n
                                | RM -> n
                                | RZ -> false
                            (if up then q0 + 1I else q0), true
                (if n then -mag else mag), inexact
        let ext (v : bigint) = if size = 16 && signed && v < 0I then v + two32 else v
        if value < lo then word (ext lo), fl ||| IOC
        elif value > hi then word (ext hi), fl ||| IOC
        else word (ext value), fl ||| (if inexact then IXC else 0u)

/// a 16- or 32-bit integer with fbits fraction bits to float (vcvt), using the FPSCR rounding mode
let ofFixed ctl fmt (signed : bool) (size : int) (fbits : int) (w : uint32) : FpRes =
    let raw = rawOfWord w &&& (pow2 size - 1I)
    let v = if signed && raw >= pow2 (size - 1) then raw - pow2 size else raw
    if v = 0I then exact fmt (zeroBits fmt false)
    else round { ctl with FZ = false } fmt (v < 0I) (abs v) (-fbits) false

// ---------------------------------------------------------------- literals and display

/// VFP immediates for vmov: ±(16..31)/16 * 2^(-3..4)
let isVfpImmediate fmt (raw : bigint) =
    let rec oddPart (m : bigint) = if m % 2I = 0I then oddPart (m / 2I) else m
    match decode defaultCtl fmt raw with
    | FFin(_, m, e), _ ->
        let exp = e + bitLength m - 1
        exp >= -3 && exp <= 4 && bitLength (oddPart m) <= 5
    | _ -> false

/// parse a GNU float literal exactly (round to nearest): 1.5, -2e-3, .5, 7, 0x1.8p3, inf, nan
let parseFloat fmt (txt : string) : Result<bigint, string> =
    let t = txt.Trim().ToLower()
    let neg, body =
        if t.StartsWith "-" then true, t.Substring(1).Trim()
        elif t.StartsWith "+" then false, t.Substring(1).Trim()
        else false, t
    let fromDigits (d : bigint) (exp2 : int) (exp10 : int) =
        if d = 0I then Ok(zeroBits fmt neg)
        elif exp10 > 5000 then Ok(infBits fmt neg)
        elif exp10 < -5000 then Ok(zeroBits fmt neg)
        elif exp10 >= 0 then Ok (round defaultCtl fmt neg (d * bigint.Pow(10I, exp10)) exp2 false).Bits
        else
            let den = bigint.Pow(10I, -exp10)
            let k = max 0 (fmt.P + 3 + bitLength den - bitLength d)
            let num = d <<< k
            let q = num / den
            Ok (round defaultCtl fmt neg q (exp2 - k) (num - q * den <> 0I)).Bits
    let isDigits (s : string) (hex : bool) =
        s <> "" && s |> Seq.forall (fun c -> System.Char.IsDigit c || (hex && "abcdef".Contains(string c)))
    match body with
    | "inf" | "infinity" -> Ok(infBits fmt neg)
    | "nan" -> Ok(withSign fmt neg (defaultNaN fmt))
    | b when b.StartsWith "0x" ->
        let b = b.Substring 2
        let mant, exp2 = match b.Split('p') with | [| m; e |] -> m, Some e | [| m |] -> m, Some "0" | _ -> "", None
        let ip, fp = match mant.Split('.') with | [| i; f |] -> i, f | [| i |] -> i, "" | _ -> "?", "?"
        match exp2 with
        | Some e when isDigits (ip + fp) true && (ip = "" || isDigits ip true) && (fp = "" || isDigits fp true)
                      && (let e = e.TrimStart('+', '-') in isDigits e false) ->
            let d = (ip + fp) |> Seq.fold (fun acc c -> acc * 16I + bigint (System.Convert.ToInt32(string c, 16))) 0I
            fromDigits d (int e - 4 * fp.Length) 0
        | _ -> Error(sprintf "'%s' is not a valid hexadecimal float" txt)
    | b ->
        let mant, exp10 =
            match b.Split('e') with
            | [| m; e |] -> m, e
            | [| m |] -> m, "0"
            | _ -> "", ""
        let ip, fp = match mant.Split('.') with | [| i; f |] -> i, f | [| i |] -> i, "" | _ -> "?", "?"
        if isDigits (ip + fp) false && (ip = "" || isDigits ip false) && (fp = "" || isDigits fp false)
           && isDigits (exp10.TrimStart('+', '-')) false && exp10.Length < 7 then
            let d = (ip + fp) |> Seq.fold (fun acc c -> acc * 10I + bigint (int c - int '0')) 0I
            fromDigits d 0 (int exp10 - fp.Length)
        else Error(sprintf "'%s' is not a valid floating point number" txt)

#if FABLE_COMPILER
[<Emit("new DataView(new ArrayBuffer(8))")>]
let private makeView () : obj = jsNative
let private view = makeView ()
[<Emit("($0.setUint32(0, $1, true), $0.setUint32(4, $2, true), $0.getFloat64(0, true))")>]
let private f64OfWords (v : obj) (lo : uint32) (hi : uint32) : float = jsNative
[<Emit("$0.toExponential($1)")>]
let private toExponential (x : float) (digits : int) : string = jsNative
let private toDouble (lo : uint32) (hi : uint32) = f64OfWords view lo hi
#else
let private toDouble (lo : uint32) (hi : uint32) =
    System.BitConverter.Int64BitsToDouble(int64 lo ||| (int64 hi <<< 32))
let private toExponential (x : float) (digits : int) =
    x.ToString("E" + string digits, System.Globalization.CultureInfo.InvariantCulture)
#endif

/// the value as a double (exact for every format here)
let toFloat fmt (raw : bigint) =
    let lo, hi = wordsOfRaw (convert defaultCtl fmt F64 raw).Bits
    toDouble lo hi

/// shortest decimal text that reads back as the same value: "0.1", "-2.5e-7", "3.0", "inf", "nan"
let format fmt (raw : bigint) =
    let neg = isNeg fmt raw
    let sign = if neg then "-" else ""
    match decode defaultCtl fmt raw with
    | FNaN, _ -> "nan"
    | FInf _, _ -> sign + "inf"
    | FZero _, _ -> sign + "0.0"
    | FFin(n, m, e), _ ->
        // the value as a double (exact: every format here fits in one)
        let d = (convert defaultCtl fmt F64 raw).Bits
        let x = let lo, hi = wordsOfRaw d in toDouble lo hi
        let digitsOf n =
            // "d.ddddE+xxx" or "d.dddde+x" -> digit string and decimal exponent
            let s = (toExponential (abs x) (n - 1)).ToLower()
            let mant, ex = match s.Split('e') with | [| m; e |] -> m, int e | _ -> s, 0
            mant.Replace(".", ""), ex
        let roundTrips (digits : string, ex) =
            parseFloat fmt (sprintf "%s%s.%se%d" sign (digits.Substring(0, 1)) (digits.Substring 1) ex) = Ok raw
        let digits, ex = [ 1 .. 17 ] |> List.map digitsOf |> List.tryFind roundTrips |> Option.defaultValue (digitsOf 17)
        let digits = digits.TrimEnd('0') |> fun s -> if s = "" then "0" else s
        let body =
            if ex >= -5 && ex < 16 then
                if ex >= digits.Length - 1 then digits + String.replicate (ex - digits.Length + 1) "0" + ".0"
                elif ex >= 0 then digits.Substring(0, ex + 1) + "." + digits.Substring(ex + 1)
                else "0." + String.replicate (-ex - 1) "0" + digits
            else
                let frac = if digits.Length > 1 then "." + digits.Substring 1 else ""
                sprintf "%s%se%d" (digits.Substring(0, 1)) frac ex
        sign + body
