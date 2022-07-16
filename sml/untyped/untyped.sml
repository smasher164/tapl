fun eprint s = TextIO.output (TextIO.stdErr, s ^ "\n")
fun errExit (z, st) = (eprint z; Posix.Process.exit(Word8.fromInt(st)))

fun usage() = errExit ("usage: untyped ( -small-step | -big-step ) file\n\n"
            ^ "untyped is an implementation of the untyped lambda calculus (TAPL chapters 5-7).", 2)

structure Term =
struct
    datatype term = Var of int | Abs of string * term | App of term * term
    type context = string list
    fun name t = case t of
        Var _ => "Var"
        | Abs _ => "Abs"
        | App _ => "App"
    fun pickFreshName ctx s =
        if List.exists (fn v => v = s) ctx
        then pickFreshName ctx (s ^ "'")
        else (s::ctx,s)
    (* assumes v is inside list. *)
    fun indexOf v (hd::tl) = 
        if v = hd then
            0
        else
            1 + (indexOf v tl)
        | indexOf v [] = 0
    fun toString ctx t = case t of
        Abs (s,t1) =>
            let val (ctx,s) = pickFreshName ctx s in
                "(λ" ^ s ^ "." ^ (toString ctx t1) ^ ")"
            end
        | App(t1,t2) =>
            "(" ^ (toString ctx t1) ^ " " ^ (toString ctx t2) ^ ")"
        | Var(x) =>
            List.nth (ctx, x)
    fun toDeBruijnString t = case t of
        Abs (s,t1) =>
            "(λ." ^ (toDeBruijnString t1) ^ ")"
        | App(t1,t2) =>
            "(" ^ (toDeBruijnString t1) ^ " " ^ (toDeBruijnString t2) ^ ")"
        | Var(x) => Int.toString x
    fun shift d c t = case t of
        Var(k) => if k < c then Var(k) else Var(k+d)
        | Abs(s,t1) => Abs(s, shift d (c+1) t1)
        | App(t1,t2) => App(shift d c t1, shift d c t2)
    fun subst j s t = case t of
        Var(k) => if k = j then s else Var(k)
        | Abs(x,t1) => Abs(x, subst (j+1) (shift 1 0 s) t1)
        | App(t1,t2) => App(subst j s t1, subst j s t2)
    fun substStop s t = shift ~1 0 (subst 0 (shift 1 0 s) t)
    fun isVal t = case t of
        Abs _ => true
        | _ => false
    exception NoRuleApplies
    fun eval1 t = case t of
        App(Abs(x,t12),t2) => 
            if isVal t2 then 
                substStop t2 t12
            else
                App(Abs(x,t12), eval1 t2)
        | App(t1,t2) => App(eval1 t1, t2)
        | _ => raise NoRuleApplies
    fun smallStep t = let
        val t' = eval1 t
    in
        smallStep t'
    end handle NoRuleApplies => t
    fun bigStep t = case t of
        App(t1,t2) =>
            let val v1 = bigStep t1 in case v1 of
            Abs(x,t12) =>
                let 
                    val v2 = bigStep t2
                in
                    if isVal v2 then
                        bigStep (substStop v2 t12)
                    else
                        t
                end
            | _ => t
            end
        | _ => t
    fun split (hay:char VectorSlice.slice) (nee:Char.char) =
        if Substring.isEmpty hay then
            []
        else
            let
                val (a,b) = Substring.splitl (fn c => c <> nee ) hay
            in
                if (Substring.size b) = 0 then
                    [Substring.string a]
                else
                    (Substring.string a) :: (String.str nee) :: (split (Substring.triml 1 b) nee)
            end
    fun isIdent (hd::tl) =
        if tl = nil then
            true
        else
             (hd <> #"'") andalso (Char.isAlpha hd) andalso isIdent tl
        | isIdent nil = false
    fun scan f = let
        val s = TextIO.inputAll f
        val _ = TextIO.closeIn f
        val tokens = String.tokens Char.isSpace s
        fun flatten (l: string list list) = List.foldr op@ [] l
        fun filter t l = flatten (List.map (fn s => split (Substring.full s) t) l)
        val tokens = (filter #"(" o filter #")" o filter #".") tokens
        val tokens = flatten (List.map (fn s =>
            let
                fun self cur s = 
                    if s = "" then
                        cur :: []
                    else if String.isPrefix "λ" s then
                        cur :: "λ" :: (self "" (String.extract(s,2,NONE)))
                    else
                        self (cur^String.str(String.sub(s,0))) (String.extract(s,1,NONE))
            in
                self "" s
            end
        ) tokens)
        val tokens = List.filter (fn s => s <> "") tokens
    in
        List.map (fn s => case s of
            "(" | ")" | "λ" | "." => s
            | _ =>
                if isIdent (String.explode s) then
                    s
                else
                    errExit ("unexpected token \"" ^ s ^ "\"", 1)
        ) tokens
    end
    fun expect t tokens : string list = case tokens of
        (hd::tl) =>
            if hd <> t then
                errExit("expected token \"" ^ t ^ "\", got \"" ^ hd ^ "\"", 1)
            else
                tl
        | _ => errExit("expected token \"" ^ t ^ "\", got \"" ^ "EOF" ^ "\"", 1)
    fun parseLambda ctx (tokens: string list) : term * string list =
        case tokens of
        (tok::tokens) =>
            let
                val _ = print "parseLambda\n"
                val tokens = expect "." tokens
                val (body,tokens) = parseSingle (tok::ctx) tokens
            in
                (Abs(tok,body),tokens)
            end
        | _ => errExit("expected identifier, got \"" ^ "EOF" ^ "\"", 1) 
    and parseParenExpr ctx (tok::tokens) : term*string list =
        let
            val _ = print "parseParenExpr\n"
            val (t,tokens) = parse ctx (tok::tokens)
        in
            (t, expect ")" tokens)
        end
        | parseParenExpr _ _ = errExit ("unexpected token \"" ^ "EOF" ^ "\"", 1)
    and parseSingle ctx (tokens: string list) : term * string list =
        let
            val _ = print "parseSingle\n"
        in
            case tokens of
            (tok::tokens) => (case tok of
                ")" | "." => errExit ("unexpected token \"" ^ tok ^ "\"", 1)
                | "(" => parseParenExpr ctx tokens
                | "λ" => parseLambda ctx tokens
                | id => (Var(indexOf id ctx),tokens)) (* TODO: How to store ID for Var? *)
            | _ => errExit ("unexpected token \"" ^ "EOF" ^ "\"", 1)
        end
    and parse ctx (tokens: string list) =
        let
            val _ = print "parse\n"
            val (a,tokens) = parseSingle ctx tokens
        in
            if (List.length tokens) = 0 orelse ((List.hd tokens) = ")") then
                (a,tokens)
            else
                let
                    val (b,tokens) = parseSingle ctx tokens
                in
                    (App(a,b),tokens)
                end
        end
end

val _ = let
    val (eval, filename) = case CommandLine.arguments() of
        ["-small-step",filename] => (Term.smallStep, filename) 
    |   ["-big-step",filename] => (Term.bigStep, filename)
    | _ => usage()
    val f = TextIO.openIn filename handle e => (eprint (exnMessage e); usage())
    val tokens = Term.scan f
    val ctx = ["y", "z"]
    val (ast,_) = Term.parse ctx tokens
    val ast = eval ast
    val s = Term.toString ctx ast
    (*val _ = Token.expect f Token.EOF*)
    (*val s = Term.toDeBruijnString ast*)
in
    (*List.map (fn (a,b) => print ("\n" ^ a ^ "\n" ^ b ^ "\n")) tok2*)
    (*List.map (fn s => print ("TOKEN: "^s^"\n")) tokens;*)
    print s
end