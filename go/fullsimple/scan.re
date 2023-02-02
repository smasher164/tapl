//go:generate re2go scan.re -o scan.go -i
package main

import "strconv"

func peek(b []byte, i int, limit int) byte {
	if limit <= i {
		return 0
	}
	return b[i]
}

type Token = int
const (
	EOFTok Token = iota
)

type stlcLex struct {
	i int
	saved int
	limit int
	b []byte
	cmds []Command
}

func (sc *stlcLex) Lex(lval *stlcSymType) int {
	beg, end := 0, 0
	/*!stags:re2c format = 'var @@ int'; separator = "\n\t"; */
	/*!re2c
	re2c:define:YYCTYPE = byte;
	re2c:define:YYPEEK = "peek(sc.b, sc.i, sc.limit)";
	re2c:define:YYSKIP = "sc.i++";
	re2c:define:YYBACKUP = "sc.saved = sc.i";
	re2c:define:YYRESTORE = "sc.i = sc.saved";
	re2c:define:YYSHIFT = "sc.i += @@{shift}";
	re2c:define:YYSTAGP = "@@{tag} = sc.i";
	re2c:define:YYSTAGN = "@@{tag} = -1";
	re2c:define:YYSHIFTSTAG = "@@{tag} += @@{shift}";
	re2c:define:YYLESSTHAN = "sc.limit <= sc.i";
	re2c:yyfill:enable = 0;
	re2c:eof = 0;
	re2c:flags:tags = 1;

	lcid = [a-z]+;
	ucid = [A-Z]+;
	intpat = [0-9]+;
	space = [ \t\r\n]+;

	"(" { return LparenTok }
	")" { return RparenTok }
	"λ" { return LambdaTok }
	"." { return DotTok }
	":" { return ColonTok }
	"<" { return LessThanTok }
	">" { return GreaterThanTok }
	"=" { return EqualsTok }
	";" { return SemicolonTok }
	"{" { return LbraceTok }
	"}" { return RbraceTok }
	"," { return CommaTok }
	"=>" { return FatArrowTok }
	"->" { return SkinnyArrowTok }
	"|" { return OrTok }
	"if" { return IfTok }
	"then" { return ThenTok }
	"else" { return ElseTok }
	"true" { return TrueTok }
	"false" { return FalseTok }
	"Bool" { return BoolTok }
	"Nat" { return NatTok }
	"unit" { return UnitValTok }
	"Unit" { return UnitTok }
	"iszero" { return IsZeroTok }
	"pred" { return PredTok }
	"succ" { return SuccTok }
	"fix" { return FixTok }
	"let" { return LetTok }
	"letrec" { return LetRecTok }
	"in" { return InTok }
	"case" { return CaseTok }
	"of" { return OfTok }
	"as" { return AsTok }
	"_" { return UnderscoreTok }

	space { return sc.Lex(lval) }
	@beg lcid @end { lval.text = sc.b[beg:end]; return LCIDTok }
	@beg ucid @end { lval.text = sc.b[beg:end]; return UCIDTok }
	@beg intpat @end { lval.intval, _ = strconv.Atoi(string(sc.b[beg:end])); return IntTok }
	$ { return EOFTok }
	@beg * { unexpected(string(sc.b[beg:beg+1])) }
	*/
}

func (stlcLex) Error(s string) {
	unexpected(s)
}