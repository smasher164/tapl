%{
//go:generate goyacc -l -p stlc -o parse.go parse.y
package main

import "strconv"
%}

%union {
    text []byte
    intval int
    x Term
    t Ty
    c Command
    cl []Command
    f Field
    r Record
    cs []C
    ce C
    tf TyField
    tr []TyField
}

%token TrueTok FalseTok UnitTok NatTok IsZeroTok PredTok SuccTok
%token IfTok ThenTok ElseTok LparenTok RparenTok LambdaTok ColonTok DotTok BoolTok
%token SkinnyArrowTok SemicolonTok EqualsTok UnitValTok LetTok InTok FixTok LbraceTok RbraceTok CommaTok
%token LessThanTok GreaterThanTok AsTok CaseTok OfTok OrTok FatArrowTok LetRecTok UnderscoreTok
%token <text> LCIDTok
%token <text> UCIDTok
%token <intval> IntTok
%type <x> term
%type <x> termApp
%type <x> termSingle
%type <x> termPath
%type <x> termAscribe
%type <x> termSeq
%type <t> ty
%type <t> tySingle
%type <c> command
%type <cl> top
%type <f> field
%type <r> fields
%type <cs> cases
%type <ce> case
%type <tf> typeField
%type <tr> typeFields

%%
start: top { stlclex.(*stlcLex).cmds = $1 }

top: command SemicolonTok top {
        $$ = append($$, $1)
        $$ = append($$, $3...)
    }
    | {} // empty
    ;

command: term { $$ = Eval{$1} }
    | LCIDTok ColonTok ty { $$ = Bind{string($1), VarBinding{ $3 } } }
    | LCIDTok EqualsTok term { $$ = Bind{string($1), TmAbbBind{ Term: $3 } } }
    | UCIDTok { $$ = Bind{string($1), TyVarBind{} } }
    | UCIDTok EqualsTok ty { $$ = Bind{string($1), TyAbbBind{ $3 } } }
    ;

term: termApp { $$ = $1 }
    | IfTok term ThenTok term ElseTok term { $$ = If{ $2, $4, $6 } }
    | LambdaTok LCIDTok ColonTok ty DotTok term { $$ = Abs{ string($2), $4, $6 } }
    | LambdaTok UnderscoreTok ColonTok ty DotTok term { $$ = Abs{ "_", $4, $6 } }
    | LetTok LCIDTok EqualsTok term InTok term { $$ = Let{ string($2), $4, $6 } }
    | LetTok UnderscoreTok EqualsTok term InTok term { $$ = Let{ "_", $4, $6 } }
    | CaseTok term OfTok cases { $$ = Case{ $2, $4 } }
    | LetRecTok LCIDTok ColonTok ty EqualsTok term InTok term { $$ = Let{string($2), Fix{Abs{string($2), $4, $6}}, $8} }
    ;

cases: case { $$ = append($$, $1) }
    | case OrTok cases { $$ = append($$, $1); $$ = append($$, $3...) }
    ;

case: LessThanTok LCIDTok EqualsTok LCIDTok GreaterThanTok FatArrowTok termApp {
        $$ = C{string($2), string($4), $7}
    }
    ;

termApp: termPath { $$ = $1 }
    | termApp termPath { $$ = App{ $1, $2 } }
    | SuccTok termPath { $$ = Succ{ $2 } }
    | FixTok termPath { $$ = Fix{ $2 } }
    | IsZeroTok termPath { $$ = IsZero{ $2 } }
    | PredTok termPath { $$ = Pred{ $2 } }
    ;

termAscribe: termSingle { $$ = $1 }
    | termSingle AsTok ty { $$ = Ascribe{ $1, $3 } }
    ;

termPath: termAscribe { $$ = $1 }
    | termPath DotTok LCIDTok { $$ = Proj{ $1, string($3) } }
    | termPath DotTok IntTok { $$ = Proj{ $1, strconv.Itoa($3) } }
    ;

termSeq: term { $$ = $1 }
    | term SemicolonTok termSeq {
        $$ = App{Abs{"_", TyUnit{}, $3}, $1}
    }
    ;

termSingle: TrueTok { $$ = True{} }
    | FalseTok { $$ = False{} }
    | UnitValTok { $$ = Unit{} }
    | LCIDTok { $$ = Ident( $1 ) }
    | LparenTok termSeq RparenTok { $$ = $2 }
    | LbraceTok fields RbraceTok { $$ = $2 }
    | LessThanTok LCIDTok EqualsTok term GreaterThanTok AsTok ty { $$ = Tag{ string($2), $4, $7 } }
    | IntTok {
        var f func(int) Term
        f = func(n int) Term {
            if n == 0 {
                return Zero{}
            }
            return Succ{ f(n-1) }
        }
        $$ = f($1)
    }
    ;

fields: field { $$ = Record{ $1 } }
    | field CommaTok fields { $$ = append(Record{ $1 }, $3...) }
    | {} // empty
    ;

field: LCIDTok EqualsTok term { $$ = Field{ string($1), $3 } }
    | term { $$ = Field{ "", $1 } }
    ;

ty: tySingle { $$ = $1 }
    | ty SkinnyArrowTok tySingle { $$ = TyArr{ $1, $3 } }
    ;

tySingle: BoolTok { $$ = TyBool{} }
    | NatTok { $$ = TyNat{} }
    | UnitTok { $$ = TyUnit{} }
    | LparenTok ty RparenTok { $$ = $2 }
    | UCIDTok { $$ = TyId( $1 ) }
    | LbraceTok typeFields RbraceTok { $$ = TyRecord($2) }
    | LessThanTok typeFields GreaterThanTok { $$ = TyVariant($2) }
    ;

typeFields: typeField { $$ = []TyField{ $1 } }
    | typeField CommaTok typeFields { $$ = append([]TyField{ $1 }, $3...) }
    | {} // empty
    ;

typeField: LCIDTok ColonTok ty { $$ = TyField{ string($1), $3 } }
    | ty { $$ = TyField{ "", $1 } }
    ;
%%