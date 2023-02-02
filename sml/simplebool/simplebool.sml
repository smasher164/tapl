fun eprint s = TextIO.output (TextIO.stdErr, s ^ "\n")
fun errExit (z, st) = (eprint z; Posix.Process.exit(Word8.fromInt(st)))
fun unexpected s = errExit ("unexpected token \"" ^ s ^ "\"", 1)

fun usage() = errExit ("usage: simplebool ( -small-step | -big-step ) file\n\n"
            ^ "simplebool is an implementation of the simply-typed lambda calculus with booleans (TAPL chapter 9-10).", 2)

datatype ty =
    Bool
    | Arr of ty * ty
datatype term = 
    Var of int
    | True
    | False
    | Abs of string * ty * term
    | App of term * term
    | If of term * term * term
fun tyToString (t: ty): string = case t of
    Bool => "Bool"
    | Arr(from,to) => (tyToString from) ^ "->" ^ (tyToString to)
datatype binding =
    NameBind
    | VarBind of ty
fun getTypeFromContext (ctx: (string * binding) list) i =
    let
        val elem = List.nth (ctx, i)
        val b = #2 elem
    in
        case b of
        VarBind(typ) => typ
        | NameBind => errExit("getTypeFromContext: Wrong kind of binding for variable " ^ (#1 elem) , 1)
    end
fun typeOf (ctx: (string * binding) list) t = case t of
    Var i => getTypeFromContext ctx i
    | Abs(x,tyT1,t2) =>
        let
            val ctx' = (x,VarBind(tyT1))::ctx
            val tyT2 = typeOf ctx' t2
        in
            Arr(tyT1,tyT2)
        end
    | App(t1,t2) =>
        let
            val tyT1 = typeOf ctx t1
            val tyT2 = typeOf ctx t2
        in
            case tyT1 of
            Arr(tyT11,tyT12) =>
                if tyT2 = tyT11 then tyT12
                else errExit("parameter type mismatch", 1)
            | _ => errExit("arrow type expected", 1)
        end
    | True | False => Bool
    | If(t1,t2,t3) =>
        if (typeOf ctx t1) = Bool then
            let
                val tyT2 = typeOf ctx t2
            in
                if tyT2 = (typeOf ctx t3) then tyT2
                else errExit("arms of conditional have different types", 1)
            end
        else errExit("guard of conditional not a boolean", 1)
fun name t = case t of
    Var _ => "Var"
    | True => "true"
    | False => "false"
    | Abs _ => "Abs"
    | App _ => "App"
    | If _ => "if"
fun pickFreshName ctx s =
    if List.exists (fn v => (#1 v) = s) ctx
    then pickFreshName ctx (s ^ "'")
    else ((s,NameBind)::ctx,s)
(* assumes v is inside list. *)
fun indexOf (v: string) ((hd::tl) : (string*binding) list) : int option = 
    if v = (#1 hd) then
        SOME(0)
    else
        (case (indexOf v tl) of
        SOME(i) => SOME(1+i)
        | NONE => NONE
        )
    | indexOf v [] = NONE
fun toDeBruijnString t = case t of
    Abs (s,ty,t1) =>
        "(λ:" ^ (tyToString ty) ^ "." ^ (toDeBruijnString t1) ^ ")"
    | App(t1,t2) =>
        "(" ^ (toDeBruijnString t1) ^ " " ^ (toDeBruijnString t2) ^ ")"
    | Var(x) => Int.toString x
    | True | False => name t
    | If(cond, body, elseBody) => "if " ^ (toDeBruijnString cond) ^ " then " ^ (toDeBruijnString body) ^ " else " ^ (toDeBruijnString elseBody)
fun toString (ctx: (string*binding) list) t = case t of
    Abs (s,ty,t1) =>
        let
            (* val _ = print ("Abs " ^ s ^ "\n") *)
            val (ctx,s) = pickFreshName ctx s
        in
            "(λ" ^ s ^ ":" ^ (tyToString ty) ^ "." ^ (toString ctx t1) ^ ")"
        end
    | App(t1,t2) =>
        let
            (* val _ = print ("App t1: " ^ (toDeBruijnString t1) ^ " t2: " ^ (toDeBruijnString t2) ^ "\n") *)
        in
            "(" ^ (toString ctx t1) ^ " " ^ (toString ctx t2) ^ ")"
        end
    | Var(x) =>
        let
            (* val _ = print ("Var: " ^ (Int.toString x) ^ " Len: " ^ (Int.toString (List.length ctx)) ^ "\n") *)
        in
            #1 (List.nth (ctx, x))
        end
    | True | False => name t
    | If(cond, body, elseBody) => "if " ^ (toString ctx cond) ^ " then " ^ (toString ctx body) ^ " else " ^ (toString ctx elseBody)
fun shift d c t = case t of
    Var(k) => if k < c then Var(k) else Var(k+d)
    | Abs(s,ty,t1) => Abs(s, ty, shift d (c+1) t1)
    | App(t1,t2) => App(shift d c t1, shift d c t2)
    | True | False => t
    | If(cond, body, elseBody) => If(shift d c cond, shift d c body, shift d c elseBody)
fun subst j s t = case t of
    Var(k) => if k = j then s else t
    | Abs(x,ty,t1) => Abs(x, ty, subst (j+1) (shift 1 0 s) t1)
    | App(t1,t2) => App(subst j s t1, subst j s t2)
    | True | False => t
    | If(cond, body, elseBody) => If(subst j s cond, subst j s body, subst j s elseBody)
fun substStop s t = shift ~1 0 (subst 0 (shift 1 0 s) t)
fun isVal t = case t of
    Abs _ => true
    | _ => false
exception NoRuleApplies
fun eval1 t = case t of
    If(True,body,_) => body
    | If(False,_,elseBody) => elseBody
    | If(cond,body,elseBody) => If(eval1 cond, body, elseBody)
    | App(Abs(x,ty,t12),t2) => 
        if isVal t2 then 
            substStop t2 t12
        else
            App(Abs(x,ty,t12), eval1 t2)
    | App(t1,t2) => App(eval1 t1, t2)
    | _ => raise NoRuleApplies
fun smallStep t = let
    val t' = eval1 t
in
    smallStep t'
end handle NoRuleApplies => t
fun bigStep t = case t of
    If(cond,body,elseBody) =>
        (case (bigStep cond) of
        True => bigStep body
        | False => bigStep elseBody
        | _ => t)
    | App(t1,t2) =>
        let val v1 = bigStep t1 in case v1 of
        Abs(_,_,t12) =>
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
fun isIdent (hd::tl) =
    if tl = nil then
        Char.isAlpha hd
    else
            (hd <> #"'") andalso (Char.isAlpha hd) andalso isIdent tl
    | isIdent nil = false
fun scan f = let
    val s = TextIO.inputAll f
    val _ = TextIO.closeIn f
    val tokens = String.tokens Char.isSpace s
    fun flatten (l: string list list) = List.foldr op@ [] l
    fun split_once del str =
        let 
            val (pref, suff) = Substring.position del (Substring.full str)
            val (_, i, _) = Substring.base suff
        in
            if i = size str then
                NONE
            else 
                SOME (Substring.string pref, Substring.string (Substring.slice (suff, size del, NONE)))
        end
    fun split_all del str =
        let
            val res = split_once del str
        in
            (case res of
            SOME(bef,aft) => bef::del::(split_all del aft)
            | NONE => [str])
        end
    fun filter (del: string) (ss: string list) = flatten (List.map (fn s => split_all del s) ss)
    val tokens = (filter "(" o filter ")" o filter "." o filter ":" o filter "->") tokens
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
        "(" | ")" | "λ" | "." | ":" | "->" => s
        | _ =>
            if isIdent (String.explode s) then
                s
            else
                unexpected s
    ) tokens
end
fun expect t tokens : string list = case tokens of
    (hd::tl) =>
        if hd <> t then
            errExit("expected token \"" ^ t ^ "\", got \"" ^ hd ^ "\"", 1)
        else
            tl
    | _ => errExit("expected token \"" ^ t ^ "\", got \"" ^ "EOF" ^ "\"", 1)
fun parseType ctx ("Bool"::tokens) : ty * string list = parseArrowType Bool ctx tokens
    | parseType ctx ("("::tokens) =
        let
            val (from,tokens) = parseParenType ctx tokens
        in
            parseArrowType from ctx tokens
        end
    | parseType _ (tok::_) = unexpected tok
    | parseType _ _ = unexpected "EOF"
and parseParenType ctx (("Bool" | "(") :: tokens) =
    let
        val (t,tokens) = parseType ctx tokens
        val tokens = expect ")" tokens
    in
        (t,tokens)
    end
|   parseParenType _ _ = errExit("expected identifier or \"(\", got \"EOF\"", 1) 
and parseArrowType from ctx ("->" :: tokens) =
    let
        val (to,tokens) = parseType ctx tokens
    in
        (Arr(from, to), tokens)
    end
|   parseArrowType from _ tokens = (from, tokens)
and parseLambda (ctx: (string * binding) list) (tokens: string list) : term * string list =
    case tokens of
    (id::tokens) =>
        let
            (* val _ = print "parseLambda\n" *)
            val tokens = expect ":" tokens
            val (ty,tokens) = parseType ctx tokens
            val tokens = expect "." tokens
            val (body,tokens) = parse ((id,NameBind)::ctx) tokens
        in
            (Abs(id,ty,body),tokens)
        end
    | _ => errExit("expected identifier, got \"" ^ "EOF" ^ "\"", 1) 
and parseIf ctx tokens =
    let
        val (cond,tokens) = parse ctx tokens
        val tokens = expect "then" tokens
        val (body,tokens) = parse ctx tokens
        val tokens = expect "else" tokens
        val (elseBody,tokens) = parse ctx tokens
    in
        (If(cond,body,elseBody), tokens)
    end
and parseParenExpr ctx (tok::tokens) : term * string list =
    let
        (* val _ = print "parseParenExpr\n" *)
        val (t,tokens) = parse ctx (tok::tokens)
    in
        (t, expect ")" tokens)
    end
    | parseParenExpr _ _ = unexpected "EOF"
and parseSingle ctx (tokens: string list) : term * string list =
    let
        (* val _ = print "parseSingle\n" *)
    in
        case tokens of
        (tok::tokens) => (
            case tok of
            ")" | "." => unexpected tok
            | "(" => parseParenExpr ctx tokens
            | "λ" => parseLambda ctx tokens
            | "if" => parseIf ctx tokens
            | "true" => (True,tokens)
            | "false" => (False,tokens)
            | id => (
                case (indexOf id ctx) of
                SOME(i) => (Var(i),tokens)
                | NONE => let
                    in
                    errExit("undefined variable \"" ^ id ^ "\"", 1)
                    end
                )
            )
        | _ => unexpected "EOF"
    end
and parse ctx (tokens: string list) =
    let
        (* val _ = print "parse\n" *)
        val (a,tokens) = parseSingle ctx tokens
    in
        if (List.length tokens) = 0 then
            (a,tokens)
        else
            case List.hd tokens of
            ")" | "then" | "else" => (a,tokens)
            | _ =>
                let
                    val (b,tokens) = parseSingle ctx tokens
                in
                    (App(a,b),tokens)
                end
    end

val _ = let
    val (eval, filename) = case CommandLine.arguments() of
        ["-small-step",filename] => (smallStep, filename) 
    |   ["-big-step",filename] => (bigStep, filename)
    | _ => usage()
    val f = TextIO.openIn filename handle e => (eprint (exnMessage e); usage())
    val tokens = scan f
    val (ast,tokens) = parse [] tokens
    val _ = if (List.length tokens) <> 0 then
        errExit("expected token \"" ^ "EOF" ^ "\", got \"" ^ (List.hd tokens) ^ "\"", 1)
    else
        ()
    val _ = typeOf [] ast
    val ast = eval ast
    val s = toString [] ast
in
    (* List.map (fn s => print ("TOKEN: "^s^"\n")) tokens; *)
    (* List.map (fn s => print ("CTX: "^s^"\n")) ctx; *)
    print (s ^ "\n")
end