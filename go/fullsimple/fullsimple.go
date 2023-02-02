package main

import (
	"flag"
	"fmt"
	"os"
	"reflect"
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
	fmt.Fprint(os.Stderr, "usage: fullsimple ( -small-step | -big-step ) file\n\n")
	fmt.Fprint(os.Stderr, "fullsimple is an implementation of the simply-typed lambda calculus with extensions (TAPL chapter 11).\n")
	os.Exit(2)
}

func errExit(err error) {
	fmt.Fprintln(os.Stderr, err)
	os.Exit(1)
}

func unexpected(s string) {
	errExit(fmt.Errorf("unexpected token %q", s))
}

type Term interface {
	isTerm()
	DeBruijnString() string
	ContextString(ctx []Context) string
}

type True struct{}

func (True) isTerm()                          {}
func (t True) DeBruijnString() string         { return "true" }
func (t True) ContextString([]Context) string { return "true" }

type False struct{}

func (False) isTerm()                          {}
func (t False) DeBruijnString() string         { return "false" }
func (t False) ContextString([]Context) string { return "false" }

type If struct {
	Cond Term
	Body Term
	Else Term
}

func (If) isTerm() {}

func (i If) DeBruijnString() string {
	return "if " + i.Cond.DeBruijnString() + " then " + i.Body.DeBruijnString() + " else " + i.Else.DeBruijnString()
}

func (i If) ContextString(ctx []Context) string {
	return "if " + i.Cond.ContextString(ctx) + " then " + i.Body.ContextString(ctx) + " else " + i.Else.ContextString(ctx)
}

type Var int

func (Var) isTerm() {}

func (v Var) DeBruijnString() string {
	return strconv.Itoa(int(v))
}

func (v Var) ContextString(ctx []Context) string {
	return ctx[v].Name
}

type Ident string

func (id Ident) isTerm() {}

func (id Ident) DeBruijnString() string {
	return string(id)
}

func (id Ident) ContextString(ctx []Context) string {
	return string(id)
}

func (id Ident) String() string {
	return string(id)
}

type Abs struct {
	OldBind string
	Type    Ty
	Body    Term
}

func (Abs) isTerm() {}

func (a Abs) DeBruijnString() string {
	return "(λ:" + a.Type.DeBruijnString() + "." + a.Body.DeBruijnString() + ")"
}

func contains(ctx []Context, s string) bool {
	return slices.IndexFunc(ctx, func(c Context) bool { return c.Name == s }) >= 0
}

func pickFreshName(ctx []Context, s string) ([]Context, string) {
	if contains(ctx, s) {
		return pickFreshName(ctx, s+"'")
	}
	return addBinding(ctx, s, NameBind{}), s
}

func (a Abs) ContextString(ctx []Context) string {
	ctx1, oldBind := pickFreshName(ctx, a.OldBind)
	return "(λ" + oldBind + ":" + a.Type.ContextString(ctx) + "." + a.Body.ContextString(ctx1) + ")"
}

type App struct {
	Fn  Term
	Arg Term
}

func (App) isTerm() {}

func (a App) DeBruijnString() string {
	return "(" + a.Fn.DeBruijnString() + " " + a.Arg.DeBruijnString() + ")"
}

func (a App) ContextString(ctx []Context) string {
	return "(" + a.Fn.ContextString(ctx) + " " + a.Arg.ContextString(ctx) + ")"
}

// Leaving out Float, Timesfloat, and String for now.

/*
Currently we have:
True, False, If, Var, Abs, App

We need to add:
Ascribe
Case
Tag
Unit
Let
Record
Proj
Fix
Zero
Succ
Pred
IsZero
*/

type Ascribe struct {
	X    Term
	Type Ty
}

func (Ascribe) isTerm() {}

func (a Ascribe) DeBruijnString() string {
	return a.X.DeBruijnString() + " as " + a.Type.DeBruijnString()
}

func (a Ascribe) ContextString(ctx []Context) string {
	return a.X.ContextString(ctx) + " as " + a.Type.ContextString(ctx)
}

type Case struct {
	X     Term
	Cases []C
}

type C struct {
	L string
	X string
	T Term
}

func (Case) isTerm() {}

func (c Case) DeBruijnString() string {
	return "case " + c.X.DeBruijnString() + " of " + strings.Join(lo.Map(c.Cases, func(c C, _ int) string {
		return "<" + c.L + "=" + c.X + ">" + "=>" + c.T.DeBruijnString()
	}), "| ")
}

func (c Case) ContextString(ctx []Context) string {
	return "case " + c.X.ContextString(ctx) + " of " + strings.Join(lo.Map(c.Cases, func(c C, _ int) string {
		ctx, x := pickFreshName(ctx, c.X)
		return "<" + c.L + "=" + x + ">" + "=>" + c.T.ContextString(ctx)
	}), "| ")
}

type Tag struct {
	L    string
	T    Term
	Type Ty
}

func (Tag) isTerm() {}

func (t Tag) DeBruijnString() string {
	return "<" + t.L + "=" + t.T.DeBruijnString() + "> as " + t.Type.DeBruijnString()
}

func (t Tag) ContextString(ctx []Context) string {
	return "<" + t.L + "=" + t.T.ContextString(ctx) + "> as " + t.Type.ContextString(ctx)
}

type Unit struct{}

func (Unit) isTerm() {}

func (Unit) DeBruijnString() string             { return "Unit" }
func (Unit) ContextString(ctx []Context) string { return "Unit" }

type Let struct {
	X   string
	T   Term
	InT Term
}

func (Let) isTerm() {}

func (l Let) DeBruijnString() string {
	return "let " + l.X + "=" + l.T.DeBruijnString() + " in " + l.InT.DeBruijnString()
}

func (l Let) ContextString(ctx []Context) string {
	ctx1, x := pickFreshName(ctx, l.X)
	return "let " + x + "=" + l.T.ContextString(ctx) + " in " + l.InT.ContextString(ctx1)
}

type Proj struct {
	T Term
	L string
}

func (Proj) isTerm() {}

func (p Proj) DeBruijnString() string {
	return p.T.DeBruijnString() + "." + p.L
}

func (p Proj) ContextString(ctx []Context) string {
	return p.T.ContextString(ctx) + "." + p.L
}

type Fix struct {
	T Term
}

func (Fix) isTerm() {}

func (f Fix) DeBruijnString() string {
	return "fix " + f.T.DeBruijnString()
}

func (f Fix) ContextString(ctx []Context) string {
	return "fix " + f.T.ContextString(ctx)
}

type Zero struct{}

func (Zero) isTerm() {}

func (Zero) DeBruijnString() string             { return "0" }
func (Zero) ContextString(ctx []Context) string { return "0" }

type Succ struct {
	T Term
}

func (Succ) isTerm() {}

func (s Succ) DeBruijnString() string {
	return "succ " + s.T.DeBruijnString()
}

func (s Succ) ContextString(ctx []Context) string {
	return "succ " + s.T.ContextString(ctx)
}

type Pred struct {
	T Term
}

func (Pred) isTerm() {}

func (p Pred) DeBruijnString() string {
	return "pred " + p.T.DeBruijnString()
}

func (p Pred) ContextString(ctx []Context) string {
	return "pred " + p.T.ContextString(ctx)
}

type IsZero struct {
	T Term
}

func (IsZero) isTerm() {}

func (i IsZero) DeBruijnString() string {
	return "iszero " + i.T.DeBruijnString()
}

func (i IsZero) ContextString(ctx []Context) string {
	return "iszero " + i.T.ContextString(ctx)
}

type Record []Field

func (Record) isTerm() {}
func (r Record) DeBruijnString() string {
	var buf strings.Builder
	buf.WriteString("{")
	for i, f := range r {
		if i > 0 {
			buf.WriteString(", ")
		}
		buf.WriteString(f.DeBruijnString())
	}
	buf.WriteString("}")
	return buf.String()
}
func (r Record) ContextString(ctx []Context) string {
	var buf strings.Builder
	buf.WriteString("{")
	for i, f := range r {
		if i > 0 {
			buf.WriteString(", ")
		}
		buf.WriteString(f.ContextString(ctx))
	}
	buf.WriteString("}")
	return buf.String()
}

type Field struct {
	Name string
	Term Term
}

func (f Field) DeBruijnString() string {
	if f.Name == "" {
		return f.Term.DeBruijnString()
	}
	return f.Name + "=" + f.Term.DeBruijnString()
}
func (f Field) ContextString(ctx []Context) string {
	if f.Name == "" {
		return f.Term.ContextString(ctx)
	}
	return f.Name + "=" + f.Term.ContextString(ctx)
}

func addBinding(ctx []Context, name string, bind Binding) []Context {
	return prepend(Context{name, bind}, ctx)
}

func prepend[T any](v T, from []T) []T {
	return append([]T{v}, from...)
}

type Ty interface {
	isType()
	DeBruijnString() string
	ContextString(ctx []Context) string
}

func typeEquals(ctx []Context, l, r Ty) bool {
	l = simplifyTy(ctx, l)
	r = simplifyTy(ctx, r)
	// fmt.Printf("%T == %T\n", l, r)
	kindEquals := reflect.TypeOf(l) == reflect.TypeOf(r)
	if tvar, ok1 := l.(TyVar); ok1 {
		if tyAbb, ok2 := ctx[tvar].Binding.(TyAbbBind); ok2 {
			return typeEquals(ctx, tyAbb.Ty, r)
		}
	}
	switch r := r.(type) {
	case TyBool:
		return kindEquals
	case TyUnit:
		return kindEquals
	case TyNat:
		return kindEquals
	case TyId:
		return kindEquals && l.(TyId) == r
	case TyVar:
		if kindEquals {
			return l.(TyVar) == r
		} else if tyAbb, ok := ctx[r].Binding.(TyAbbBind); ok {
			return typeEquals(ctx, tyAbb.Ty, r)
		}
	case TyRecord:
		if !kindEquals {
			return false
		}
		l := l.(TyRecord)
		if len(l) != len(r) {
			return false
		}
		for _, lf := range l {
			i := slices.IndexFunc(r, func(rf TyField) bool {
				return lf.Name == rf.Name
			})
			if i < 0 {
				return false
			}
			if !typeEquals(ctx, lf.Type, r[i].Type) {
				return false
			}
		}
		return true
	case TyVariant:
		if !kindEquals {
			return false
		}
		l := l.(TyVariant)
		if len(l) != len(r) {
			return false
		}
		for i := range l {
			if l[i].Name != r[i].Name {
				return false
			}
			if !typeEquals(ctx, l[i].Type, r[i].Type) {
				return false
			}
		}
		return true
	case TyArr:
		return kindEquals &&
			typeEquals(ctx, l.(TyArr).From, r.From) &&
			typeEquals(ctx, l.(TyArr).To, r.To)
		// case TyList:
	}
	return false
}

type TyId string

func (t TyId) isType() {}
func (t TyId) DeBruijnString() string {
	return string(t)
}
func (t TyId) ContextString(ctx []Context) string {
	return string(t)
}

type TyVar int

func (t TyVar) isType() {}
func (t TyVar) DeBruijnString() string {
	return strconv.Itoa(int(t))
}
func (t TyVar) ContextString(ctx []Context) string {
	return ctx[t].Name
}

type TyUnit struct{}

func (t TyUnit) isType() {}
func (TyUnit) DeBruijnString() string {
	return "Unit"
}
func (t TyUnit) ContextString(ctx []Context) string {
	return "Unit"
}

type TyRecord []TyField

func (t TyRecord) isType() {}
func (t TyRecord) DeBruijnString() string {
	return "{" + strings.Join(lo.Map(t, func(f TyField, i int) string { return f.DeBruijnString() }), ", ") + "}"
}
func (t TyRecord) ContextString(ctx []Context) string {
	return "{" + strings.Join(lo.Map(t, func(f TyField, i int) string { return f.ContextString(ctx) }), ", ") + "}"
}

type TyField struct {
	Name string
	Type Ty
}

func (t TyField) DeBruijnString() string {
	if t.Name == "" {
		return t.Type.DeBruijnString()
	}
	return t.Name + ":" + t.Type.DeBruijnString()
}
func (t TyField) ContextString(ctx []Context) string {
	if t.Name == "" {
		return t.Type.ContextString(ctx)
	}
	return t.Name + ":" + t.Type.ContextString(ctx)
}

type TyVariant []TyField

func (t TyVariant) isType() {}
func (t TyVariant) DeBruijnString() string {
	return "<" + strings.Join(lo.Map(t, func(f TyField, i int) string { return f.DeBruijnString() }), ", ") + ">"
}
func (t TyVariant) ContextString(ctx []Context) string {
	return "<" + strings.Join(lo.Map(t, func(f TyField, i int) string { return f.ContextString(ctx) }), ", ") + ">"
}

type TyBool struct{}

func (TyBool) isType() {}
func (t TyBool) DeBruijnString() string {
	return "Bool"
}
func (t TyBool) ContextString(ctx []Context) string {
	return "Bool"
}

type TyArr struct {
	From, To Ty
}

func (TyArr) isType() {}
func (t TyArr) DeBruijnString() string {
	return t.From.DeBruijnString() + "->" + t.To.DeBruijnString()
}
func (t TyArr) ContextString(ctx []Context) string {
	return t.From.ContextString(ctx) + "->" + t.To.ContextString(ctx)
}

type TyNat struct{}

func (TyNat) isType() {}

func (t TyNat) DeBruijnString() string {
	return "Nat"
}
func (t TyNat) ContextString(ctx []Context) string {
	return "Nat"
}

type TyList struct {
	Of Ty
}

func (TyList) isType() {}

func (t TyList) DeBruijnString() string {
	return "List[" + t.Of.DeBruijnString() + "]"
}
func (t TyList) ContextString(ctx []Context) string {
	return "List[" + t.Of.ContextString(ctx) + "]"
}

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

type TyVarBind struct{}

func (TyVarBind) isBinding() {}

type TyAbbBind struct{ Ty }

func (TyAbbBind) isBinding() {}

type TmAbbBind struct {
	Term Term
	Type Ty
}

func (TmAbbBind) isBinding() {}

type Command interface {
	isCommand()
}

type Eval struct {
	Term Term
}

func (Eval) isCommand() {}

type Bind struct {
	Name    string
	Binding Binding
}

func (Bind) isCommand() {}

var noRuleApplies = fmt.Errorf("no rule applies")

func eval2(ctx []Context, t Term) (Term, error) {
	switch t := t.(type) {
	case Var:
		if bind, ok := ctx[int(t)].Binding.(TmAbbBind); ok {
			return bind.Term, nil
		}
		return nil, noRuleApplies
	case If:
		switch t.Cond.(type) {
		case True:
			return t.Body, nil
		case False:
			return t.Else, nil
		default:
			t1Prime, err := eval2(ctx, t.Cond)
			if err != nil {
				return nil, err
			}
			return If{t1Prime, t.Body, t.Else}, nil
		}
	case App:
		if abs, ok := t.Fn.(Abs); ok {
			if isVal(t.Arg) {
				return substTop(t.Arg, abs.Body), nil
			}
			t2Prime, err := eval2(ctx, t.Arg)
			if err != nil {
				return nil, err
			}
			return App{abs, t2Prime}, nil
		} else {
			t1Prime, err := eval2(ctx, t.Fn)
			if err != nil {
				return nil, err
			}
			return App{t1Prime, t.Arg}, nil
		}
	case Ascribe:
		if isVal(t.X) {
			return t.X, nil
		}
		t1, err := eval2(ctx, t.X)
		if err != nil {
			return nil, err
		}
		return Ascribe{t1, t.Type}, nil
	case Case:
		if tag, ok := t.X.(Tag); ok && isVal(tag) {
			i := slices.IndexFunc(t.Cases, func(c C) bool { return c.L == tag.L })
			if i < 0 {
				return nil, noRuleApplies
			}
			return substTop(tag.T, t.Cases[i].T), nil
		}
		t0, err := eval2(ctx, t.X)
		if err != nil {
			return nil, err
		}
		return Case{t0, t.Cases}, nil
	case Tag:
		t1, err := eval2(ctx, t.T)
		if err != nil {
			return nil, err
		}
		return Tag{t.L, t1, t.Type}, nil
	case Let:
		if isVal(t.T) {
			return substTop(t.T, t.InT), nil
		}
		t1, err := eval2(ctx, t.T)
		if err != nil {
			return nil, err
		}
		return Let{t.X, t1, t.InT}, nil
	case Proj:
		if r, ok := t.T.(Record); ok {
			i := slices.IndexFunc(r, func(f Field) bool { return f.Name == t.L })
			if i < 0 {
				return nil, noRuleApplies
			}
			return r[i].Term, nil
		}
		t1, err := eval2(ctx, t.T)
		if err != nil {
			return nil, err
		}
		return Proj{t1, t.L}, nil
	case Fix:
		if abs, ok := t.T.(Abs); ok {
			return substTop(t, abs.Body), nil
		}
		if isVal(t.T) {
			return nil, noRuleApplies
		}
		t1, err := eval2(ctx, t.T)
		if err != nil {
			return nil, err
		}
		return Fix{t1}, nil
	case Succ:
		t1, err := eval2(ctx, t.T)
		if err != nil {
			return nil, err
		}
		return Succ{t1}, nil
	case Pred:
		if zero, ok := t.T.(Zero); ok {
			return zero, nil
		}
		if succ, ok := t.T.(Succ); ok && isNumericVal(succ.T) {
			return succ.T, nil
		}
		t1, err := eval2(ctx, t.T)
		if err != nil {
			return nil, err
		}
		return Pred{t1}, nil
	case IsZero:
		if _, ok := t.T.(Zero); ok {
			return True{}, nil
		}
		if succ, ok := t.T.(Succ); ok && isNumericVal(succ.T) {
			return False{}, nil
		}
		t1, err := eval2(ctx, t.T)
		if err != nil {
			return nil, err
		}
		return IsZero{t1}, nil
	case Record:
		// using mutation here. is that alright?
		for i := range t {
			if !isVal(t[i].Term) {
				t1, err := eval2(ctx, t[i].Term)
				if err != nil {
					return nil, err
				}
				t[i].Term = t1
				return t, nil
			}
		}
		return nil, noRuleApplies
	}
	return nil, noRuleApplies
}

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
				return substTop(t.Arg, abs.Body), nil
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

func evalSmallStep2(ctx []Context, t Term) Term {
	t1Prime, err := eval2(ctx, t)
	if err != nil {
		return t
	}
	return evalSmallStep2(ctx, t1Prime)
}

func isNumericVal(t Term) bool {
	switch t := t.(type) {
	case Zero:
		return true
	case Succ:
		return isNumericVal(t.T)
	default:
		return false
	}
}

func isVal(t Term) bool {
	switch t := t.(type) {
	case Abs, True, False, Unit:
		return true
	case Tag:
		return isVal(t.T)
	case Record:
		for i := range t {
			if !isVal(t[i].Term) {
				return false
			}
		}
		return true
	default:
		return isNumericVal(t)
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

func subst2(j int, s, t Term) Term {
	switch t := t.(type) {
	case True:
		return t
	case False:
		return t
	case If:
		return If{subst2(j, s, t.Cond), subst2(j, s, t.Body), subst2(j, s, t.Else)}
	case Var:
		if int(t) == j {
			return s
		}
		return t
	// case Ident:
	case Abs:
		return Abs{t.OldBind, t.Type, subst2(j+1, shift2(1, 0, s), t.Body)}
	case App:
		return App{subst2(j, s, t.Fn), subst2(j, s, t.Arg)}
	case Ascribe:
		return Ascribe{subst2(j, s, t.X), t.Type}
	case Case:
		return Case{subst2(j, s, t.X), lo.Map(t.Cases, func(cs C, i int) C {
			return C{cs.L, cs.X, subst2(j+1, shift2(1, 0, s), cs.T)} // Is this correct?
		})}
	case Tag:
		return Tag{t.L, subst2(j, s, t.T), t.Type}
	case Unit:
		return t
	case Let:
		return Let{t.X, subst2(j, s, t.T), subst2(j+1, shift2(1, 0, s), t.InT)}
	case Proj:
		return Proj{subst2(j, s, t.T), t.L}
	case Fix:
		return Fix{subst2(j, s, t.T)}
	case Zero:
		return t
	case Succ:
		return Succ{subst2(j, s, t.T)}
	case Pred:
		return Pred{subst2(j, s, t.T)}
	case IsZero:
		return IsZero{subst2(j, s, t.T)}
	case Record:
		return Record(lo.Map(t, func(f Field, i int) Field {
			return Field{f.Name, subst2(j, s, f.Term)}
		}))
	}
	panic("unreachable")
}

func typeShift(d, c int, ty Ty) Ty {
	switch ty := ty.(type) {
	case TyId:
		return ty
	case TyVar:
		if int(ty) < c {
			return ty
		}
		return ty + TyVar(d)
	case TyUnit:
		return ty
	case TyRecord:
		return TyRecord(lo.Map(ty, func(f TyField, i int) TyField {
			return TyField{f.Name, typeShift(d, c, f.Type)}
		}))
	case TyVariant:
		return TyVariant(lo.Map(ty, func(f TyField, i int) TyField {
			return TyField{f.Name, typeShift(d, c, f.Type)}
		}))
	case TyBool:
		return ty
	case TyArr:
		return TyArr{typeShift(d, c, ty.From), typeShift(d, c, ty.To)}
	case TyNat:
		return ty
		// case TyList:
	}
	panic("unreachable")
}

func shift2(d, c int, t Term) Term {
	switch t := t.(type) {
	case True:
		return t
	case False:
		return t
	case If:
		return If{shift2(d, c, t.Cond), shift2(d, c, t.Body), shift2(d, c, t.Else)}
	case Var:
		if int(t) < c {
			return t
		}
		return t + Var(d)
	// case Ident:
	case Abs:
		return Abs{t.OldBind, typeShift(d, c, t.Type), shift2(d, c+1, t.Body)}
	case App:
		return App{shift2(d, c, t.Fn), shift2(d, c, t.Arg)}
	case Ascribe:
		return Ascribe{shift2(d, c, t.X), typeShift(d, c, t.Type)}
	case Case:
		return Case{shift2(d, c, t.X), lo.Map(t.Cases, func(cs C, i int) C {
			return C{cs.L, cs.X, shift2(d, c+1, cs.T)}
		})}
	case Tag:
		return Tag{t.L, shift2(d, c, t.T), typeShift(d, c, t.Type)}
	case Unit:
		return t
	case Let:
		return Let{t.X, shift2(d, c, t.T), shift2(d, c+1, t.InT)}
	case Proj:
		return Proj{shift2(d, c, t.T), t.L}
	case Fix:
		return Fix{shift2(d, c, t.T)}
	case Zero:
		return t
	case Succ:
		return Succ{shift2(d, c, t.T)}
	case Pred:
		return Pred{shift2(d, c, t.T)}
	case IsZero:
		return IsZero{shift2(d, c, t.T)}
	case Record:
		return Record(lo.Map(t, func(f Field, i int) Field {
			return Field{f.Name, shift2(d, c, f.Term)}
		}))
	}
	panic("unreachable")
}

func oldSubstStop(s, t Term) Term {
	return shift(-1, 0, subst(0, shift(1, 0, s), t))
}

func substTop(s, t Term) Term {
	return shift2(-1, 0, subst2(0, shift2(1, 0, s), t))
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
				return evalBigStep(substTop(v2, v1.Body))
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

func computeTy(ctx []Context, ty Ty) (Ty, error) {
	if tyVar, isTyVar := ty.(TyVar); isTyVar {
		if tyAbb, isTyAbb := ctx[int(tyVar)].Binding.(TyAbbBind); isTyAbb {
			return tyAbb.Ty, nil
		}
	}
	return nil, noRuleApplies
}

func simplifyTy(ctx []Context, ty Ty) Ty {
	tyPrime, err := computeTy(ctx, ty)
	if err != nil {
		return ty
	}
	return simplifyTy(ctx, tyPrime)
}

func typeOf(ctx []Context, t Term) Ty {
	switch t := t.(type) {
	case True, False:
		return TyBool{}
	case Unit:
		return TyUnit{}
	case Zero:
		return TyNat{}
	case If:
		if condType := typeOf(ctx, t.Cond); typeEquals(ctx, condType, TyBool{}) {
			bodyType := typeOf(ctx, t.Body)
			if elseType := typeOf(ctx, t.Else); typeEquals(ctx, bodyType, elseType) {
				return bodyType
			}
			errExit(fmt.Errorf("arms of conditional have different types"))
		}
		errExit(fmt.Errorf("guard of conditional not a boolean"))
	case Var:
		return getTypeFromContext(ctx, int(t))
	// case Ident:
	case Abs:
		// fmt.Printf("%#v\n", t.Type)
		ctxPrime := addBinding(ctx, t.OldBind, VarBinding{t.Type})
		tyT2 := typeOf(ctxPrime, t.Body)
		return TyArr{t.Type, typeShift(-1, 0, tyT2)}
	case App:
		tof := typeOf(ctx, t.Fn)
		// fmt.Printf("tof: %#v\n", tof)
		tyT1 := simplifyTy(ctx, tof)
		tyT2 := typeOf(ctx, t.Arg)
		// fmt.Printf("%#v  (    %#v   )\n", tyT1, tyT2)
		switch tyArr := tyT1.(type) {
		case TyArr:
			// fmt.Printf("typeEquals(%#v, %#v)\n", tyT2, tyArr.From)
			if typeEquals(ctx, tyT2, tyArr.From) {
				return tyArr.To
			}
			errExit(fmt.Errorf("parameter type mismatch"))
		}
		errExit(fmt.Errorf("arrow type expected"))
	case Ascribe:
		if !typeEquals(ctx, typeOf(ctx, t.X), t.Type) {
			errExit(fmt.Errorf("cannot ascribe term"))
		}
		return t.Type
	case Case:
		if tyVar, isTyVar := simplifyTy(ctx, typeOf(ctx, t.X)).(TyVariant); isTyVar {
			caseTypes := lo.Map(t.Cases, func(c C, _ int) Ty {
				i := slices.IndexFunc(tyVar, func(f TyField) bool {
					return f.Name == c.L
				})
				if i < 0 {
					errExit(fmt.Errorf("label %q not in type", c.L))
				}
				ctxPrime := addBinding(ctx, c.X, VarBinding{tyVar[i].Type})
				return typeShift(-1, 0, typeOf(ctxPrime, c.T))
			})
			for _, c := range t.Cases {
				if !lo.ContainsBy(tyVar, func(f TyField) bool { return f.Name == c.L }) {
					errExit(fmt.Errorf("label %q not in type", c.L))
				}
			}
			for _, ct := range caseTypes {
				if !typeEquals(ctx, caseTypes[0], ct) {
					errExit(fmt.Errorf("fields do not have the same type"))
				}
			}
			return caseTypes[0]
		}
		errExit(fmt.Errorf("expected variant type"))
	case Tag:
		if tyVar, isTyVar := simplifyTy(ctx, t.Type).(TyVariant); isTyVar {
			i := slices.IndexFunc(tyVar, func(f TyField) bool { return f.Name == t.L })
			if i < 0 {
				errExit(fmt.Errorf("label %q not found", t.L))
			}
			tyTiExpected := tyVar[i].Type
			tyTi := typeOf(ctx, t.T)
			if typeEquals(ctx, tyTi, tyTiExpected) {
				return t.Type
			}
			errExit(fmt.Errorf("field does not have expected type"))
		}
		errExit(fmt.Errorf("annotation is not a variant type"))
	case Let:
		T1 := typeOf(ctx, t.T)
		ctxPrime := addBinding(ctx, t.X, VarBinding{T1})
		T2 := typeOf(ctxPrime, t.InT)
		return typeShift(-1, 0, T2)
	case Proj:
		T1 := simplifyTy(ctx, typeOf(ctx, t.T))
		if tyRec, isRec := T1.(TyRecord); isRec {
			i := slices.IndexFunc(tyRec, func(f TyField) bool { return f.Name == t.L })
			if i < 0 {
				errExit(fmt.Errorf("label %q not found", t.L))
			}
			return tyRec[i].Type
		}
		errExit(fmt.Errorf("expected record type"))
	case Fix:
		if tyArr, isTyArr := simplifyTy(ctx, typeOf(ctx, t.T)).(TyArr); isTyArr {
			if typeEquals(ctx, tyArr.From, tyArr.To) {
				return tyArr.To
			}
			errExit(fmt.Errorf("result of body not compatible with domain"))
		}
		errExit(fmt.Errorf("arrow type expected"))
	case Succ:
		if _, isNat := typeOf(ctx, t.T).(TyNat); !isNat {
			errExit(fmt.Errorf("argument of succ is not a number"))
		}
		return TyNat{}
	case Pred:
		if _, isNat := typeOf(ctx, t.T).(TyNat); !isNat {
			errExit(fmt.Errorf("argument of pred is not a number"))
		}
		return TyNat{}
	case IsZero:
		if _, isNat := typeOf(ctx, t.T).(TyNat); !isNat {
			errExit(fmt.Errorf("argument of iszero is not a number"))
		}
		return TyBool{}
	case Record:
		return TyRecord(lo.Map(t, func(f Field, i int) TyField {
			return TyField{f.Name, typeOf(ctx, f.Term)}
		}))
	}
	panic("unreachable")
}

func resolveIdentifiersInType(ctx []Context, ty Ty) Ty {
	switch ty := ty.(type) {
	case TyVar, TyUnit, TyBool, TyNat:
		return ty
	case TyId:
		i := slices.IndexFunc(ctx, func(c Context) bool { return c.Name == string(ty) })
		if i < 0 {
			errExit(fmt.Errorf("undefined type %q", ty))
		}
		return TyVar(i)
	case TyRecord:
		var r TyRecord
		for _, ff := range ty {
			r = append(r, TyField{ff.Name, resolveIdentifiersInType(ctx, ff.Type)})
		}
		return r
	case TyVariant:
		var r TyVariant
		for _, ff := range ty {
			r = append(r, TyField{ff.Name, resolveIdentifiersInType(ctx, ff.Type)})
		}
		return r
	case TyArr:
		return TyArr{resolveIdentifiersInType(ctx, ty.From), resolveIdentifiersInType(ctx, ty.To)}
		// case TyList:
	}
	panic("unreachable")
}

func resolveIdentifiersInTerm(ctx []Context, t Term) Term {
	switch t := t.(type) {
	case True, False, Var, Unit, Zero:
		return t
	case If:
		cond := resolveIdentifiersInTerm(ctx, t.Cond)
		body := resolveIdentifiersInTerm(ctx, t.Body)
		els := resolveIdentifiersInTerm(ctx, t.Else)
		return If{cond, body, els}
	case Ident:
		i := slices.IndexFunc(ctx, func(c Context) bool { return c.Name == string(t) })
		if i < 0 {
			errExit(fmt.Errorf("undefined variable %q", t))
		}
		return Var(i)
	case Abs:
		ctx1 := addBinding(ctx, t.OldBind, NameBind{})
		body := resolveIdentifiersInTerm(ctx1, t.Body)
		return Abs{t.OldBind, resolveIdentifiersInType(ctx, t.Type), body}
	case Let:
		if fix, ok1 := t.T.(Fix); ok1 {
			if abs, ok2 := fix.T.(Abs); ok2 {
				ctx1 := addBinding(ctx, t.X, NameBind{})
				return Let{
					X: t.X,
					T: Fix{
						T: Abs{
							OldBind: abs.OldBind,
							Type:    resolveIdentifiersInType(ctx, abs.Type),
							Body:    resolveIdentifiersInTerm(ctx1, abs.Body),
						},
					},
					InT: resolveIdentifiersInTerm(ctx1, t.InT),
				}
			}
		}
		x := resolveIdentifiersInTerm(ctx, t.T)
		ctx1 := addBinding(ctx, t.X, NameBind{})
		inT := resolveIdentifiersInTerm(ctx1, t.InT)
		return Let{t.X, x, inT}
	case App:
		return App{resolveIdentifiersInTerm(ctx, t.Fn), resolveIdentifiersInTerm(ctx, t.Arg)}
	case Ascribe:
		return Ascribe{resolveIdentifiersInTerm(ctx, t.X), resolveIdentifiersInType(ctx, t.Type)}
	case Case:
		var c Case
		c.X = resolveIdentifiersInTerm(ctx, t.X)
		for _, cc := range t.Cases {
			ctx1 := addBinding(ctx, cc.X, NameBind{})
			t1 := resolveIdentifiersInTerm(ctx1, cc.T)
			c.Cases = append(c.Cases, C{cc.L, cc.X, t1})
		}
		return c
	case Tag:
		return Tag{
			t.L,
			resolveIdentifiersInTerm(ctx, t.T),
			resolveIdentifiersInType(ctx, t.Type),
		}
	case Proj:
		return Proj{resolveIdentifiersInTerm(ctx, t.T), t.L}
	case Fix:
		return Fix{resolveIdentifiersInTerm(ctx, t.T)}
	case Succ:
		return Succ{resolveIdentifiersInTerm(ctx, t.T)}
	case Pred:
		return Pred{resolveIdentifiersInTerm(ctx, t.T)}
	case IsZero:
		return IsZero{resolveIdentifiersInTerm(ctx, t.T)}
	case Record:
		var r Record
		for _, ff := range t {
			r = append(r, Field{ff.Name, resolveIdentifiersInTerm(ctx, ff.Term)})
		}
		return r
	}
	panic(fmt.Sprintf("unreachable: %T", t))
}

func resolveIdentifiersInCommand(ctx []Context, cmd Command) ([]Context, Command) {
	switch cmd := cmd.(type) {
	case Eval:
		return ctx, Eval{resolveIdentifiersInTerm(ctx, cmd.Term)}
	case Bind:
		bind := cmd.Binding
		if abb, ok := bind.(TmAbbBind); ok {
			if *smallStep {
				tPrime := evalSmallStep2(ctx, abb.Term)
				bind = TmAbbBind{tPrime, abb.Type}
			} else {
				// TODO: bigStep
			}
		}
		return addBinding(ctx, cmd.Name, bind), cmd
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
	lexer := &stlcLex{
		limit: len(b),
		b:     b,
	}
	stlcParse(lexer)
	var ctx []Context
	for _, cmd := range lexer.cmds {
		cmd := cmd
		ctx, cmd = resolveIdentifiersInCommand(ctx, cmd)
		if cmd, ok := cmd.(Eval); ok {
			typeOf(ctx, cmd.Term)
			if *smallStep {
				fmt.Println(evalSmallStep2(ctx, cmd.Term).ContextString(ctx))
			} else {
				// TODO: bigStep
				// fmt.Println(evalBigStep(ctx, cmd.Term).ContextString(ctx))
			}
		}
	}
}
