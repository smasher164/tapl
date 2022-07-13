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
    fun toString ctx t = case t of
        Abs (s,t1) =>
            let val (ctx,s) = pickFreshName ctx s in
                "(λ" ^ s ^ "." ^ (toString ctx t1) ^ ")"
            end
        | App(t1,t2) =>
            "(" ^ (toString ctx t1) ^ " " ^ (toString ctx t2) ^ ")"
        | Var(x) =>
            List.nth (ctx, x)
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
end


val prog = Term.App(
    Term.Abs("x",Term.App(
        Term.App(Term.Var(1),Term.Var(0)),
        Term.Var(2)
    )),
    Term.Abs("x",Term.Var(0))
)
(*val t = Term.Abs("a'",Term.Var(0,4))*)
val c = ["a'", "b"]
val s = Term.toString c prog
val _ = print s
val r = Term.smallStep prog
val _ = print "\n"
val _ = print (Term.toString c r)
val c2 = ["a'", "b"]
val r2 = Term.bigStep prog
val _ = print "\n"
val _ = print (Term.toString c2 r2)
(* val _ = (print o Term.name o Term.Var) 0 *)
(* TODO: pickFreshName should just generate new name? *)
(* TODO: just return an answer in de bruijn indexes? *)