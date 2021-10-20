grammar ARITH {
    rule TOP { <.ws> <term> <.ws> <EOF> }
    rule term {
        <true>
        | <false>
        | <zero>
        | <succ>
        | <pred>
        | <iszero>
        | <if>
        | { exit note qq 'unexpected token "$(token { \w+ }(self) || "EOF")"' }
    }
    rule succ { "succ" <term> }
    rule pred { "pred" <term> }
    rule iszero { "iszero" <term> }
    rule if { "if" <term> <expect("then")> <term> <expect("else")> <term> }
    rule EOF { <expect("")> }
    token true { "true" }
    token false { "false" }
    token zero { "0" }
    method expect($want) {
        my $got := token { \w+ }(self);
        if $got ne $want {
            exit note qq 'expected token "{$want || "EOF"}", got "$got"';
        }
        $got || token {""}(self)
    }
}

sub usage() {
    exit $*ERR.print: q:to/END/; 
    usage: arith ( -small-step | -big-step ) file

    arith is an implementation of the untyped calculus
    of booleans and numbers (TAPL chapter 3 & 4).
    END
}

class NoRuleApplies is Exception {}

sub eval1($t) {
    given $t.keys {
        when "if" {
            given ch($t, 0).keys {
                when "true" { ch($t, 1) }
                when "false" { ch($t, 2) }
                default { cons("if", [eval1(ch($t, 0)), ch($t, 1), ch($t, 2)]) }
            }
        }
        when "succ" { cons("succ", eval1(ch($t))) }
        when "pred" {
            my $t1 = ch($t);
            given $t1.keys {
                when "zero" { cons("zero") }
                when "succ" { 
                    my $nv1 = ch($t1);
                    is-numeric-val($nv1) ?? $nv1 !! cons("pred", eval1(cons("succ", $nv1)))
                }
                default { cons("pred", eval1($t1)) }
            }
        }
        when "iszero" {
            my $t1 = ch($t);
            given $t1.keys {
                when "zero" { cons("true") }
                when "succ" {
                    my $nv1 = ch($t1);
                    is-numeric-val($nv1) ?? cons("false") !! cons("iszero", eval1(cons("succ", $nv1)))
                }
                default { cons("iszero", eval1($t1)) }
            }
        }
        default { NoRuleApplies.new.throw }
    }
}

sub small-step($t) {
    try { return small-step(eval1($t)) }
    if $!.WHAT === NoRuleApplies { return $t }
}

sub is-val($ast) {
    given $ast.keys {
        when "true" | "false" { True }
        default { is-numeric-val($ast) }
    }
}

sub is-numeric-val($ast) {
    given $ast.keys {
        when "zero" { True }
        when "succ" { is-numeric-val(ch($ast)) }
        default { False }
    }
}

sub big-step($t) {
    is-val($t) ?? $t !! do given $t.keys {
        when "if" {
            given big-step(ch($t, 0)).keys {
                when "true" { big-step(ch($t, 1)) }
                when "false" { big-step(ch($t, 2)) }
            }
        }
        when "succ" {
            my $v1 = big-step(ch($t));
            is-numeric-val($v1) ?? cons("succ", $v1) !! $t
        }
        when "pred" {
            my $v1 = big-step(ch($t));
            given $v1.keys {
                when "zero" { $v1 }
                when "succ" { ch($v1) }
            }
        }
        when "iszero" {
            given big-step(ch($t)).keys {
                when "zero" { cons("true") }
                when "succ" { cons("false") }
            }
        }
        default { $t }
    }
}

sub cons($par, $ch=$par) { { $par => "term" => $ch } }

sub ch($ast, $i=0) {
    my $t = $ast{$ast.keys[0]}<term>;
    $t.WHAT === Array ?? $t[$i] !! $t
}

multi sub printTree($ast, $indent) {
    given $ast.keys {
        when "succ" | "pred" | "iszero" {
            say "$indent└─{name(ch($ast))}";
            printTree(ch($ast), $indent ~ "  ");
        }
        when "if" {
            say "$indent├─{name(ch($ast,0))}";
            printTree(ch($ast,0), $indent ~ "│ ");
            say "$indent├─{name(ch($ast,1))}";
            printTree(ch($ast,1), $indent ~ "│ ");
            say "$indent└─{name(ch($ast,2))}";
            printTree(ch($ast,2), $indent ~ "  ");
        }
    }
}

sub name($ast){
    given $ast.keys {
        when "zero" { "0" }
        default { $ast.keys[0] }
    }
}

multi sub printTree($ast) {
    say name($ast);
    printTree($ast, "");
}

my $eval = do given @*ARGS {
    when ("-small-step",*) { &small-step }
    when ("-big-step",*) { &big-step }
    default { usage }
};

my $ast = ARITH.parsefile(@*ARGS[1])<term>;
printTree($eval($ast));