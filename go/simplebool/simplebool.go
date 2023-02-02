package main

import (
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/samber/lo"
	"golang.org/x/exp/slices"
)

var (
	smallStep = flag.Bool("small-step", false, "run small-step evaluator")
	bigStep   = flag.Bool("big-step", false, "run small-step evaluator")
)

func usage() {
	fmt.Fprint(os.Stderr, "usage: simplebool ( -small-step | -big-step ) file\n\n")
	fmt.Fprint(os.Stderr, "simplebool is an implementation of the simply-typed lambda calculus with booleans (TAPL chapter 9-10).\n")
	os.Exit(2)
}

func errExit(err error) {
	fmt.Fprintln(os.Stderr, err)
	os.Exit(1)
}

func unexpected(s string) {
	errExit(fmt.Errorf("unexpected token %q", s))
}

func validateToken(s string) {
	switch s {
	case "(", ")", "λ", ".", ":", "->":
	default:
		if strings.IndexFunc(s, func(r rune) bool { return r < 'A' || r > 'z' }) >= 0 {
			unexpected(s)
		}
	}
}

func scan(s string) (res []string) {
	res = strings.Fields(s)
	sep := func(c string) []string {
		return lo.FlatMap(res, func(s string, _ int) (ret []string) {
			for {
				before, after, found := strings.Cut(s, c)
				if before != "" {
					ret = append(ret, before)
				}
				s = after
				if !found {
					break
				}
				ret = append(ret, c)
			}
			return ret
		})
	}
	res = sep("(")
	res = sep(")")
	res = sep(".")
	res = sep(":")
	res = sep("->")
	res = sep("λ")
	for _, s := range res {
		validateToken(s)
	}
	return res
}

type Term interface {
	DeBruijnString() string
	ContextString(ctx []Context) string
}

type True struct{}

func (t True) DeBruijnString() string         { return "true" }
func (t True) ContextString([]Context) string { return "true" }

type False struct{}

func (t False) DeBruijnString() string         { return "false" }
func (t False) ContextString([]Context) string { return "false" }

type If struct {
	Cond Term
	Body Term
	Else Term
}

func (i If) DeBruijnString() string {
	return "if " + i.Cond.DeBruijnString() + " then " + i.Body.DeBruijnString() + " else " + i.Else.DeBruijnString()
}

func (i If) ContextString(ctx []Context) string {
	return "if " + i.Cond.ContextString(ctx) + " then " + i.Body.ContextString(ctx) + " else " + i.Else.ContextString(ctx)
}

type Var int

func (v Var) DeBruijnString() string {
	return strconv.Itoa(int(v))
}

func (v Var) ContextString(ctx []Context) string {
	return ctx[v].Name
}

type Abs struct {
	OldBind string
	Type    Ty
	Body    Term
}

func (a Abs) DeBruijnString() string {
	return "(λ:" + a.Type.String() + "." + a.Body.DeBruijnString() + ")"
}

func contains(ctx []Context, s string) bool {
	return slices.IndexFunc(ctx, func(c Context) bool { return c.Name == s }) >= 0
}

func pickFreshName(ctx []Context, s string) ([]Context, string) {
	if contains(ctx, s) {
		return pickFreshName(ctx, s+"'")
	}
	return prepend(Context{Name: s}, ctx), s
}

func (a Abs) ContextString(ctx []Context) string {
	ctx, oldBind := pickFreshName(ctx, a.OldBind)
	return "(λ" + oldBind + ":" + a.Type.String() + "." + a.Body.ContextString(ctx) + ")"
}

type App struct {
	Fn  Term
	Arg Term
}

func (a App) DeBruijnString() string {
	return "(" + a.Fn.DeBruijnString() + " " + a.Arg.DeBruijnString() + ")"
}

func (a App) ContextString(ctx []Context) string {
	return "(" + a.Fn.ContextString(ctx) + " " + a.Arg.ContextString(ctx) + ")"
}

func prepend[T any](v T, from []T) []T {
	return append([]T{v}, from...)
}

func expect(tok string, tokens []string) []string {
	if len(tokens) == 0 {
		errExit(fmt.Errorf("expected token %q, got \"EOF\"", tok))
	}
	hd, tl := tokens[0], tokens[1:]
	if hd != tok {
		errExit(fmt.Errorf("expected token %q, got %q", tok, hd))
	}
	return tl
}

type Ty interface {
	isType()
	fmt.Stringer
}
type TyBool struct{}

func (t TyBool) String() string {
	return "Bool"
}

func (TyBool) isType() {}

type TyArr struct {
	From, To Ty
}

func (t TyArr) String() string {
	return t.From.String() + "->" + t.To.String()
}

func (TyArr) isType() {}

type Context struct {
	Name    string
	Binding Binding
}

type Binding interface {
	isBinding()
}

type NameBind struct{}

func (NameBind) isBinding() {}

type VarBinding struct{ Ty }

func (VarBinding) isBinding() {}

func parseParenType(ctx []Context, tokens []string) (Ty, []string) {
	if len(tokens) == 0 || (tokens[0] != "Bool" && tokens[0] != "(") {
		errExit(fmt.Errorf("expected identifier or \"(\", got \"EOF\""))
	}
	t, tokens := parseType(ctx, tokens)
	tokens = expect(")", tokens)
	return t, tokens
}

func parseArrowType(from Ty, ctx []Context, tokens []string) (Ty, []string) {
	if len(tokens) > 0 && tokens[0] == "->" {
		to, tokens := parseType(ctx, tokens[1:])
		return TyArr{from, to}, tokens
	}
	return from, tokens
}

func parseType(ctx []Context, tokens []string) (Ty, []string) {
	if len(tokens) == 0 {
		errExit(fmt.Errorf("expected identifier or \"(\", got \"EOF\""))
	}
	tok, tokens := tokens[0], tokens[1:]
	if tok == "Bool" {
		return parseArrowType(TyBool{}, ctx, tokens)
	} else if tok == "(" {
		from, tokens := parseParenType(ctx, tokens)
		return parseArrowType(from, ctx, tokens)
	}
	unexpected(tok)
	return nil, nil
}

func parseLambda(ctx []Context, tokens []string) (Term, []string) {
	// Arrow = IDENT "->" IDENT | "(" Arrow ")" | Arrow "->" Arrow
	// Lambda = "λ" IDENT ":" Arrow "."
	if len(tokens) == 0 {
		errExit(fmt.Errorf("expected identifier, got \"EOF\""))
	}
	tok, tokens := tokens[0], tokens[1:]
	tokens = expect(":", tokens)
	ty, tokens := parseType(ctx, tokens)
	// ty, tokens := parseArrow(ctx, tokens)
	tokens = expect(".", tokens)
	body, tokens := parse(prepend(Context{Name: tok}, ctx), tokens)
	return Abs{tok, ty, body}, tokens
}

func parseParenExpr(ctx []Context, tokens []string) (Term, []string) {
	if len(tokens) == 0 {
		unexpected("EOF")
	}
	t, tokens := parse(ctx, tokens)
	return t, expect(")", tokens)
}

func parseIf(ctx []Context, tokens []string) (Term, []string) {
	cond, tokens := parse(ctx, tokens)
	tokens = expect("then", tokens)
	body, tokens := parse(ctx, tokens)
	tokens = expect("else", tokens)
	elseBody, tokens := parse(ctx, tokens)
	return If{cond, body, elseBody}, tokens
}

func parseSingle(ctx []Context, tokens []string) (Term, []string) {
	if len(tokens) == 0 {
		unexpected("EOF")
	}
	tok, tokens := tokens[0], tokens[1:]
	switch tok {
	case ")", ".":
		unexpected(tok)
	case "(":
		return parseParenExpr(ctx, tokens)
	case "λ":
		return parseLambda(ctx, tokens)
	case "if":
		return parseIf(ctx, tokens)
	case "true":
		return True{}, tokens
	case "false":
		return False{}, tokens
	}
	i := slices.IndexFunc(ctx, func(c Context) bool { return c.Name == tok })
	if i < 0 {
		errExit(fmt.Errorf("undefined variable %q", tok))
	}
	return Var(i), tokens
}

func parse(ctx []Context, tokens []string) (Term, []string) {
	a, tokens := parseSingle(ctx, tokens)
	if len(tokens) == 0 || tokens[0] == ")" || tokens[0] == "then" || tokens[0] == "else" {
		return a, tokens
	}
	b, tokens := parseSingle(ctx, tokens)
	return App{a, b}, tokens
}

var noRuleApplies = fmt.Errorf("no rule applies")

func eval1(t Term) (Term, error) {
	switch t := t.(type) {
	case If:
		switch t.Cond.(type) {
		case True:
			return t.Body, nil
		case False:
			return t.Else, nil
		default:
			t1Prime, err := eval1(t.Cond)
			if err != nil {
				return nil, err
			}
			return If{t1Prime, t.Body, t.Else}, nil
		}
	case App:
		if abs, ok := t.Fn.(Abs); ok {
			if isVal(t.Arg) {
				return substStop(t.Arg, abs.Body), nil
			}
			t2Prime, err := eval1(t.Arg)
			if err != nil {
				return nil, err
			}
			return App{abs, t2Prime}, nil
		} else {
			t1Prime, err := eval1(t.Fn)
			if err != nil {
				return nil, err
			}
			return App{t1Prime, t.Arg}, nil
		}
	default:
		return nil, noRuleApplies
	}
}

func evalSmallStep(t Term) Term {
	t1Prime, err := eval1(t)
	if err != nil {
		return t
	}
	return evalSmallStep(t1Prime)
}

func isVal(t Term) bool {
	switch t.(type) {
	case Abs, True, False:
		return true
	default:
		return false
	}
}

func shift(d, c int, t Term) Term {
	switch t := t.(type) {
	case Var:
		if int(t) < c {
			return t
		}
		return t + Var(d)
	case Abs:
		return Abs{t.OldBind, t.Type, shift(d, c+1, t.Body)}
	case App:
		return App{shift(d, c, t.Fn), shift(d, c, t.Arg)}
	case True, False:
		return t
	case If:
		return If{shift(d, c, t.Cond), shift(d, c, t.Body), shift(d, c, t.Else)}
	}
	panic("unreachable")
}

func subst(j int, s, t Term) Term {
	switch t := t.(type) {
	case Var:
		if int(t) == j {
			return s
		}
		return t
	case Abs:
		return Abs{t.OldBind, t.Type, subst(j+1, shift(1, 0, s), t.Body)}
	case App:
		return App{subst(j, s, t.Fn), subst(j, s, t.Arg)}
	case True, False:
		return t
	case If:
		return If{subst(j, s, t.Cond), subst(j, s, t.Body), subst(j, s, t.Else)}
	}
	panic("unreachable")
}

func substStop(s, t Term) Term {
	return shift(-1, 0, subst(0, shift(1, 0, s), t))
}

func evalBigStep(t Term) Term {
	switch t := t.(type) {
	case If:
		condVal := evalBigStep(t.Cond)
		switch condVal.(type) {
		case True:
			return evalBigStep(t.Body)
		case False:
			return evalBigStep(t.Else)
		}
	case App:
		v1 := evalBigStep(t.Fn)
		switch v1 := v1.(type) {
		case Abs:
			v2 := evalBigStep(t.Arg)
			if isVal(v2) {
				return evalBigStep(substStop(v2, v1.Body))
			}
			return t
		default:
			return t
		}
	}
	return t
}

func getTypeFromContext(ctx []Context, i int) Ty {
	return ctx[i].Binding.(VarBinding).Ty
}

func typeOf(ctx []Context, t Term) Ty {
	switch t := t.(type) {
	case Var:
		return getTypeFromContext(ctx, int(t))
	case Abs:
		ctxPrime := prepend(Context{Name: t.OldBind, Binding: VarBinding{t.Type}}, ctx)
		tyT2 := typeOf(ctxPrime, t.Body)
		return TyArr{t.Type, tyT2}
	case App:
		tyT1 := typeOf(ctx, t.Fn)
		tyT2 := typeOf(ctx, t.Arg)
		switch tyArr := tyT1.(type) {
		case TyArr:
			if tyT2 == tyArr.From {
				return tyT2
			}
			errExit(fmt.Errorf("parameter type mismatch"))
		default:
			errExit(fmt.Errorf("arrow type expected"))
		}
	case True:
		return TyBool{}
	case False:
		return TyBool{}
	case If:
		if condType := typeOf(ctx, t.Cond); condType == (TyBool{}) {
			bodyType := typeOf(ctx, t.Body)
			if elseType := typeOf(ctx, t.Else); bodyType == elseType {
				return bodyType
			}
			errExit(fmt.Errorf("arms of conditional have different types"))
		} else {
			errExit(fmt.Errorf("guard of conditional not a boolean"))
		}
	}
	panic("unreachable")
}

func main() {
	flag.Usage = usage
	flag.Parse()
	if *smallStep == *bigStep {
		usage()
	}
	args := flag.Args()
	if len(args) != 1 {
		usage()
	}
	b, err := os.ReadFile(args[0])
	if err != nil {
		fmt.Fprintf(os.Stderr, "%v\n", err)
		usage()
	}
	tokens := scan(string(b))
	ast, tokens := parse(nil, tokens)
	if len(tokens) != 0 {
		errExit(fmt.Errorf("expected token \"EOF\", got %q", tokens[0]))
	}
	typeOf(nil, ast)
	if *smallStep {
		ast = evalSmallStep(ast)
	} else {
		ast = evalBigStep(ast)
	}
	fmt.Println(ast.ContextString(nil))
}
