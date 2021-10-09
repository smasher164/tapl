fun eprint s = TextIO.output (TextIO.stdErr, s ^ "\n")
fun errExit (z, st) = (eprint z; Posix.Process.exit(Word8.fromInt(st)))

fun usage() = errExit ("usage: arith ( -small-step | -big-step ) file\n\n"
            ^ "arith is an implementation of the untyped calculus\n"
            ^ "of booleans and numbers (TAPL chapter 3 & 4).", 2)


structure Token =
struct
    datatype token = EOF | True | False | If | Then | Else | Zero | Succ | Pred | IsZero
    fun toString tok = case tok of
                EOF => "EOF"
            | True => "true"
            | False => "false"
            | If => "if"
            | Then => "then"
            | Else => "else"
            | Zero => "0"
            | Succ => "succ"
            | Pred => "pred"
            | IsZero => "iszero"
    fun scanFn reader src =
            let
                val src = StringCvt.skipWS reader src
                fun notSpace c = not (Char.isSpace c)
                val (pref, src') = StringCvt.splitl notSpace reader src
            in
                case (pref, src') of    
                    ("", ns) => SOME(EOF, ns)
                | ("true", ns) => SOME(True, ns)
                | ("false", ns) => SOME(False, ns)
                | ("if", ns) => SOME(If, ns)
                | ("then", ns) => SOME(Then, ns)
                | ("else", ns) => SOME(Else, ns)
                | ("0", ns) => SOME(Zero, ns)
                | ("succ", ns) => SOME(Succ, ns)
                | ("pred", ns) => SOME(Pred, ns)
                | ("iszero", ns) => SOME(IsZero, ns)
                | (w, ns) => errExit ("unexpected token \"" ^ w ^ "\"", 1)
            end
    val scan = Option.valOf o TextIO.scanStream scanFn
    fun expect f want =
        let
            val got = scan f
        in
            if got <> want
            then errExit("expected token \"" ^ toString(want) ^ "\", got \"" ^ toString(got) ^ "\"", 1)
            else ()
        end
end

structure Term =
struct
    datatype term = True | False | Zero | Succ of term | Pred of term | IsZero of term | If of term*term*term
    fun name tm = case tm of
                True => "true"
            | False => "false"
            | Zero => "0"
            | Succ _ => "succ"
            | Pred _ => "pred"
            | IsZero _ => "iszero"
            | If _ => "if"
    fun childrenString (indent, tm) = case tm of
                Succ t | Pred t | IsZero t => indent ^ "└─" ^ name(t) ^ "\n" ^ childrenString(indent^"  ", t)
            | If(t1, t2, t3) => indent ^ "├─" ^ name(t1) ^ "\n" ^ childrenString(indent^"│ ", t1)
                ^ indent ^ "├─" ^ name(t2) ^ "\n" ^ childrenString(indent^"│ ", t2)
                ^ indent ^ "└─" ^ name(t3) ^ "\n" ^ childrenString(indent^"  ", t2)
            | _ => ""
    fun toString tm = (name tm) ^ "\n" ^ childrenString("", tm)
    fun parse f = 
            case Token.scan f
            of Token.True => True
            | Token.False => False
            | Token.Zero => Zero
            | Token.Succ => Succ(parse(f))
            | Token.Pred => Pred(parse(f))
            | Token.IsZero => IsZero(parse(f))
            | Token.If => 
                let
                    val t1 = parse(f)
                    val _ = Token.expect f Token.Then
                    val t2 = parse(f)
                    val _ = Token.expect f Token.Else
                    val t3 = parse(f)
                in
                    If(t1, t2, t3)
                end
            | word => errExit ("unexpected token \"" ^ Token.toString(word) ^ "\"", 1)
end

fun isnumericval (Term.Zero) = true
    | isnumericval (Term.Succ t1) = isnumericval t1
    | isnumericval _ = false

fun isval (Term.True | Term.False) = true
    | isval t = isnumericval t

exception NoRuleApplies

fun eval1 t = case t of
    Term.If(Term.True,t2,t3) => t2
    | Term.If(Term.False,t2,t3) => t3
    | Term.If(t1,t2,t3) => Term.If(eval1 t1,t2, t3)
    | Term.Succ(t1) => Term.Succ(eval1 t1)
    | Term.Pred(Term.Zero) => Term.Zero
    | Term.Pred(Term.Succ(nv1)) => 
        if isnumericval nv1
        then nv1 
        else Term.Pred(eval1(Term.Succ(nv1)))
    | Term.Pred(t1) => Term.Pred(eval1 t1)
    | Term.IsZero(Term.Zero) => Term.True
    | Term.IsZero(Term.Succ(nv1)) =>
        if isnumericval nv1
        then Term.False
        else Term.IsZero(eval1(Term.Succ(nv1)))
    | Term.IsZero(t1) => Term.IsZero(eval1 t1)
    | _ => raise NoRuleApplies

fun smallStep t = let
    val t' = eval1 t
in
    smallStep t'
end handle NoRuleApplies => t

fun bigStep t = 
    if isval t 
    then t 
    else case t of
        Term.True | Term.False | Term.Zero => t
        | Term.If(t1,t2,t3) => (case (bigStep t1) of
            Term.True => bigStep t2
            | Term.False => bigStep t3
            | _ => t)
        | Term.Succ t1 =>
            let val v1 = bigStep t1 in 
                if isnumericval v1
                then Term.Succ(v1) 
                else t
            end
        | Term.Pred t1 =>
            let val v1 = bigStep t1 in
                case v1 of
                Term.Zero => v1
                | Term.Succ nv1 => nv1
                | _ => t
            end
        | Term.IsZero t1 => (case (bigStep t1) of
            Term.Zero => Term.True
            | Term.Succ _ => Term.False
            | _ => t)

val _ = let
    val (eval, filename) = case CommandLine.arguments() of
        ["-small-step",filename] => (smallStep, filename) 
    |   ["-big-step",filename] => (bigStep, filename)
    | _ => usage()
    val f = TextIO.openIn filename handle e => (eprint (exnMessage e); usage())
    val ast = Term.parse f
    val _ = Token.expect f Token.EOF
in
    print(Term.toString(eval(ast)))
end