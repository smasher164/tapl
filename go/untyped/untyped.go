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
	fmt.Fprint(os.Stderr, "usage: unypted ( -small-step | -big-step ) file\n\n")
	fmt.Fprint(os.Stderr, "untyped is an implementation of the untyped lambda calculus (TAPL chapters 5-7).\n")
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
	case "(", ")", "λ", ".":
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
	res = sep("λ")
	for _, s := range res {
		validateToken(s)
	}
	return res
}

type Term interface {
	DeBruijnString() string
	ContextString(ctx []string) string
}

type Var int

func (v Var) DeBruijnString() string {
	return strconv.Itoa(int(v))
}

func (v Var) ContextString(ctx []string) string {
	return ctx[v]
}

type Abs struct {
	OldBind string
	Body    Term
}

func (a Abs) DeBruijnString() string {
	return "(λ." + a.Body.DeBruijnString() + ")"
}

func pickFreshName(ctx []string, s string) ([]string, string) {
	if slices.Contains(ctx, s) {
		return pickFreshName(ctx, s+"'")
	}
	return prepend(s, ctx), s
}

func (a Abs) ContextString(ctx []string) string {
	ctx, oldBind := pickFreshName(ctx, a.OldBind)
	return "(λ" + oldBind + "." + a.Body.ContextString(ctx) + ")"
}

type App struct {
	Fn  Term
	Arg Term
}

func (a App) DeBruijnString() string {
	return "(" + a.Fn.DeBruijnString() + " " + a.Arg.DeBruijnString() + ")"
}

func (a App) ContextString(ctx []string) string {
	return "(" + a.Fn.ContextString(ctx) + " " + a.Arg.ContextString(ctx) + ")"
}

func prepend(v string, from []string) []string {
	return append([]string{v}, from...)
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

func parseLambda(ctx, tokens []string) (Term, []string) {
	if len(tokens) == 0 {
		errExit(fmt.Errorf("expected identifier, got \"EOF\""))
	}
	tok, tokens := tokens[0], tokens[1:]
	tokens = expect(".", tokens)
	body, tokens := parse(prepend(tok, ctx), tokens)
	return Abs{tok, body}, tokens
}

func parseParenExpr(ctx, tokens []string) (Term, []string) {
	if len(tokens) == 0 {
		unexpected("EOF")
	}
	t, tokens := parse(ctx, tokens)
	return t, expect(")", tokens)
}

func parseSingle(ctx, tokens []string) (Term, []string) {
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
	}
	i := slices.Index(ctx, tok)
	if i < 0 {
		errExit(fmt.Errorf("undefined variable %q", tok))
	}
	return Var(i), tokens
}

func parse(ctx, tokens []string) (Term, []string) {
	a, tokens := parseSingle(ctx, tokens)
	if len(tokens) == 0 || tokens[0] == ")" {
		return a, tokens
	}
	b, tokens := parseSingle(ctx, tokens)
	return App{a, b}, tokens
}

var noRuleApplies = fmt.Errorf("no rule applies")

func eval1(t Term) (Term, error) {
	switch t := t.(type) {
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

func isVal(t Term) (isAbs bool) {
	_, isAbs = t.(Abs)
	return
}

func shift(d, c int, t Term) Term {
	switch t := t.(type) {
	case Var:
		if int(t) < c {
			return t
		}
		return t + Var(d)
	case Abs:
		return Abs{t.OldBind, shift(d, c+1, t.Body)}
	case App:
		return App{shift(d, c, t.Fn), shift(d, c, t.Arg)}
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
		return Abs{t.OldBind, subst(j+1, shift(1, 0, s), t.Body)}
	case App:
		return App{subst(j, s, t.Fn), subst(j, s, t.Arg)}
	}
	panic("unreachable")
}

func substStop(s, t Term) Term {
	return shift(-1, 0, subst(0, shift(1, 0, s), t))
}

func evalBigStep(t Term) Term {
	switch t := t.(type) {
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
	default:
		return t
	}
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
	if *smallStep {
		ast = evalSmallStep(ast)
	} else {
		ast = evalBigStep(ast)
	}
	fmt.Println(ast.ContextString(nil))
}
