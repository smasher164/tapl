package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"strings"
)

var (
	smallStep = flag.Bool("small-step", false, "run small-step evaluator")
	bigStep   = flag.Bool("big-step", false, "run small-step evaluator")
)

func usage() {
	fmt.Fprint(os.Stderr, "usage: arith ( -small-step | -big-step ) [file]\n\n")
	fmt.Fprint(os.Stderr, "arith is an implementation of the untyped calculus\n")
	fmt.Fprint(os.Stderr, "of booleans and numbers (TAPL chapter 3 & 4).\n")
	os.Exit(2)
}

func errExit(err error) {
	fmt.Fprintln(os.Stderr, err)
	os.Exit(1)
}

type token uint8

const (
	tEOF token = iota
	tTrue
	tFalse
	tIf
	tThen
	tElse
	tZero
	tSucc
	tPred
	tIsZero
)

func (t token) String() string {
	switch t {
	case tEOF:
		return "EOF"
	case tTrue:
		return "true"
	case tFalse:
		return "false"
	case tIf:
		return "if"
	case tThen:
		return "then"
	case tElse:
		return "else"
	case tZero:
		return "0"
	case tSucc:
		return "succ"
	case tPred:
		return "pred"
	case tIsZero:
		return "iszero"
	}
	panic("unreachable")
}

func scan(tokens chan<- token, input *bufio.Scanner) {
	input.Split(bufio.ScanWords)
	for input.Scan() {
		switch word := input.Text(); word {
		case "true":
			tokens <- tTrue
		case "false":
			tokens <- tFalse
		case "if":
			tokens <- tIf
		case "then":
			tokens <- tThen
		case "else":
			tokens <- tElse
		case "0":
			tokens <- tZero
		case "succ":
			tokens <- tSucc
		case "pred":
			tokens <- tPred
		case "iszero":
			tokens <- tIsZero
		default:
			errExit(fmt.Errorf("%q is not a valid token", word))
		}
	}
	if err := input.Err(); err != nil {
		errExit(err)
	}
	close(tokens)
}

type tmType uint8

const (
	tmTrue tmType = iota
	tmFalse
	tmZero
	tmSucc
	tmPred
	tmIsZero
	tmIf
)

func (t tmType) String() string {
	switch t {
	case tmTrue:
		return "true"
	case tmFalse:
		return "false"
	case tmZero:
		return "0"
	case tmSucc:
		return "succ"
	case tmPred:
		return "pred"
	case tmIsZero:
		return "iszero"
	case tmIf:
		return "if"
	}
	panic("unreachable")
}

type term struct {
	tmType tmType
	// tmTrue
	// tmFalse
	// tmZero
	// tmSucc   t1
	// tmPred   t1
	// tmIsZero t1
	// tmIf     t1 t2 t3
	children []term
}

func printChildren(buf *strings.Builder, indent string, children []term) {
	for i, t := range children {
		switch i {
		case len(children) - 1:
			fmt.Fprintf(buf, "%s└─%s\n", indent, t.tmType)
			printChildren(buf, indent+"  ", t.children)
		default:
			fmt.Fprintf(buf, "%s├─%s\n", indent, t.tmType)
			printChildren(buf, indent+"│ ", t.children)
		}
	}
}

func (t term) String() string {
	buf := new(strings.Builder)
	fmt.Fprintln(buf, t.tmType)
	printChildren(buf, "", t.children)
	return buf.String()
}

func expect(tokens <-chan token, want token) {
	if got := <-tokens; got != want {
		errExit(fmt.Errorf("expected token %q, got %q", want, got))
	}
}

func parse(tokens <-chan token) term {
	for tok := range tokens {
		switch tok {
		case tTrue:
			return term{tmType: tmTrue}
		case tFalse:
			return term{tmType: tmFalse}
		case tZero:
			return term{tmType: tmZero}
		case tSucc:
			return term{tmType: tmSucc, children: []term{parse(tokens)}}
		case tPred:
			return term{tmType: tmPred, children: []term{parse(tokens)}}
		case tIsZero:
			return term{tmType: tmIsZero, children: []term{parse(tokens)}}
		case tIf:
			t1 := parse(tokens)
			expect(tokens, tThen)
			t2 := parse(tokens)
			expect(tokens, tElse)
			t3 := parse(tokens)
			return term{tmType: tmIf, children: []term{t1, t2, t3}}
		}
	}
	panic("unreachable")
}

func isNumericVal(t term) bool {
	switch t.tmType {
	case tmZero:
		return true
	case tmSucc:
		return isNumericVal(t.children[0])
	default:
		return false
	}
}

func isVal(t term) bool {
	switch t.tmType {
	case tmTrue, tmFalse:
		return true
	default:
		return isNumericVal(t)
	}
}

var noRuleApplies = fmt.Errorf("no rule applies")

func eval1(t term) (res term, err error) {
	switch t.tmType {
	case tmIf:
		switch t1 := t.children[0]; t1.tmType {
		case tmTrue:
			return t.children[1], nil
		case tmFalse:
			return t.children[2], nil
		default:
			t1Prime, err := eval1(t1)
			if err != nil {
				return res, err
			}
			return term{tmType: tmIf, children: []term{t1Prime, t.children[1], t.children[2]}}, nil
		}
	case tmSucc:
		t1Prime, err := eval1(t.children[0])
		if err != nil {
			return res, err
		}
		return term{tmType: tmSucc, children: []term{t1Prime}}, nil
	case tmPred:
		switch t1 := t.children[0]; {
		case t1.tmType == tmZero:
			return term{tmType: tmZero}, nil
		case t1.tmType == tmSucc && isNumericVal(t1.children[0]):
			return t1.children[0], nil
		default:
			t1Prime, err := eval1(t1)
			if err != nil {
				return res, err
			}
			return term{tmType: tmPred, children: []term{t1Prime}}, nil
		}
	case tmIsZero:
		switch t1 := t.children[0]; {
		case t1.tmType == tmZero:
			return term{tmType: tmTrue}, nil
		case t1.tmType == tmSucc && isNumericVal(t1.children[0]):
			return term{tmType: tmFalse}, nil
		default:
			t1Prime, err := eval1(t1)
			if err != nil {
				return res, err
			}
			return term{tmType: tmIsZero, children: []term{t1Prime}}, nil
		}
	}
	return res, noRuleApplies
}

func evalSmallStep(t term) term {
	t1Prime, err := eval1(t)
	if err != nil {
		return t
	}
	return evalSmallStep(t1Prime)
}

func evalBigStep(t term) term {
	if isVal(t) {
		return t
	}
	switch t.tmType {
	case tmIf:
		switch v1 := evalBigStep(t.children[0]); v1.tmType {
		case tmTrue:
			return evalBigStep(t.children[1])
		case tmFalse:
			return evalBigStep(t.children[2])
		}
	case tmSucc:
		if v1 := evalBigStep(t.children[0]); isNumericVal(v1) {
			return term{tmType: tmSucc, children: []term{v1}}
		}
	case tmPred:
		switch v1 := evalBigStep(t.children[0]); v1.tmType {
		case tmZero, tmSucc:
			return v1
		}
	case tmIsZero:
		switch v1 := evalBigStep(t.children[0]); v1.tmType {
		case tmZero:
			return term{tmType: tmTrue}
		case tmSucc:
			return term{tmType: tmFalse}
		}
	}
	return t
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
	file, err := os.Open(args[0])
	if err != nil {
		fmt.Fprintf(os.Stderr, "%v\n", err)
		usage()
	}
	defer file.Close()
	tokens := make(chan token)
	go scan(tokens, bufio.NewScanner(file))
	ast := parse(tokens)
	expect(tokens, tEOF)
	if *smallStep {
		fmt.Print(evalSmallStep(ast))
	} else {
		fmt.Print(evalBigStep(ast))
	}
}
