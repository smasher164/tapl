use std::env;
use std::error::Error;
use std::fmt::{self, Display};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::process;
use std::str::FromStr;

fn usage() -> ! {
    eprintln!("usage: arith ( -small-step | -big-step ) file\n");
    eprintln!("arith is an implementation of the untyped calculus");
    eprintln!("of booleans and numbers (TAPL chapter 3 & 4).");
    process::exit(2);
}

fn err_exit(e: Box<dyn Error>) -> ! {
    eprintln!("{}", e);
    process::exit(1);
}

#[derive(Debug)]
enum ArithError {
    UnexpectedToken(String),
    ExpectedGotToken(String, String),
    NoRuleApplies,
}

impl std::error::Error for ArithError {}

impl Display for ArithError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArithError::UnexpectedToken(s) => write!(f, r#"unexpected token "{}""#, s),
            ArithError::ExpectedGotToken(want, got) => {
                write!(f, r#"expected token "{}", got "{}""#, want, got)
            }
            ArithError::NoRuleApplies => write!(f, "no rule applies"),
        }
    }
}

fn eval1(t: Term) -> Result<Box<Term>, ArithError> {
    match t {
        Term::If(t1, t2, t3) => match *t1 {
            Term::True => Ok(t2),
            Term::False => Ok(t3),
            _ => Ok(Box::from(Term::If(eval1(*t1)?, t2, t3))),
        },
        Term::Succ(t1) => Ok(Box::from(Term::Succ(eval1(*t1)?))),
        Term::Pred(t1) => match *t1 {
            Term::Zero => Ok(Box::from(Term::Zero)),
            Term::Succ(nv1) => {
                if nv1.is_numeric_val() {
                    Ok(nv1)
                } else {
                    Ok(Box::from(Term::Pred(eval1(Term::Succ(nv1))?)))
                }
            }
            _ => Ok(Box::from(Term::Pred(eval1(*t1)?))),
        },
        Term::IsZero(t1) => match *t1 {
            Term::Zero => Ok(Box::from(Term::True)),
            Term::Succ(nv1) => {
                if nv1.is_numeric_val() {
                    Ok(Box::from(Term::False))
                } else {
                    Ok(Box::from(Term::IsZero(eval1(Term::Succ(nv1))?)))
                }
            }
            _ => Ok(Box::from(Term::IsZero(eval1(*t1)?))),
        },
        _ => Err(ArithError::NoRuleApplies),
    }
}

fn small_step(t: Box<Term>) -> Box<Term> {
    match eval1(*t.clone()) {
        Ok(t_prime) => small_step(t_prime),
        Err(_) => t,
    }
}

fn big_step(t: Box<Term>) -> Box<Term> {
    if t.is_val() {
        t
    } else {
        match *t {
            Term::True | Term::False | Term::Zero => t,
            Term::If(ref t1, ref t2, ref t3) => match *big_step(t1.clone()) {
                Term::True => big_step(t2.clone()),
                Term::False => big_step(t3.clone()),
                _ => t,
            },
            Term::Succ(ref t1) => {
                let v1 = big_step(t1.clone());
                if v1.is_numeric_val() {
                    Box::from(Term::Succ(v1))
                } else {
                    t
                }
            }
            Term::Pred(ref t1) => {
                let v1 = big_step(t1.clone());
                match *v1 {
                    Term::Zero => v1,
                    Term::Succ(nv1) => nv1,
                    _ => t,
                }
            }
            Term::IsZero(ref t1) => match *big_step(t1.clone()) {
                Term::Zero => Box::from(Term::True),
                Term::Succ(_) => Box::from(Term::False),
                _ => t,
            },
        }
    }
}

#[derive(Debug, PartialEq)]
enum Token {
    EoF,
    True,
    False,
    If,
    Then,
    Else,
    Zero,
    Succ,
    Pred,
    IsZero,
}

impl FromStr for Token {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "true" => Ok(Token::True),
            "false" => Ok(Token::False),
            "if" => Ok(Token::If),
            "then" => Ok(Token::Then),
            "else" => Ok(Token::Else),
            "0" => Ok(Token::Zero),
            "succ" => Ok(Token::Succ),
            "pred" => Ok(Token::Pred),
            "iszero" => Ok(Token::IsZero),
            s => Err(Box::from(ArithError::UnexpectedToken(s.to_string()))),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::EoF => write!(f, "EOF"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Zero => write!(f, "0"),
            Token::Succ => write!(f, "succ"),
            Token::Pred => write!(f, "pred"),
            Token::IsZero => write!(f, "iszero"),
        }
    }
}

#[derive(Clone)]
enum Term {
    True,
    False,
    Zero,
    Succ(Box<Term>),
    Pred(Box<Term>),
    IsZero(Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
}

impl Term {
    fn name(&self) -> &str {
        match self {
            Term::True => "true",
            Term::False => "false",
            Term::Zero => "0",
            Term::Succ(_) => "succ",
            Term::Pred(_) => "pred",
            Term::IsZero(_) => "iszero",
            Term::If(_, _, _) => "if",
        }
    }
    fn fmt_children(&self, f: &mut fmt::Formatter<'_>, indent: String) -> fmt::Result {
        match self {
            Term::Succ(t) | Term::Pred(t) | Term::IsZero(t) => {
                writeln!(f, "{}└─{}", indent, t.name())?;
                t.fmt_children(f, indent + "  ")
            }
            Term::If(t1, t2, t3) => {
                writeln!(f, "{}├─{}", indent, t1.name())?;
                t1.fmt_children(f, indent.clone() + "│ ")?;
                writeln!(f, "{}├─{}", indent, t2.name())?;
                t2.fmt_children(f, indent.clone() + "│ ")?;
                writeln!(f, "{}└─{}", indent, t3.name())?;
                t3.fmt_children(f, indent + "  ")
            }
            _ => Ok(()),
        }
    }
    fn is_numeric_val(&self) -> bool {
        match self {
            Term::Zero => true,
            Term::Succ(t1) => t1.is_numeric_val(),
            _ => false,
        }
    }
    fn is_val(&self) -> bool {
        match self {
            Term::True | Term::False => true,
            _ => self.is_numeric_val(),
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.name())?;
        self.fmt_children(f, "".to_string())
    }
}

fn expect(
    tokens: &mut impl Iterator<Item = Result<Token, Box<dyn Error>>>,
    want: Token,
) -> Result<(), Box<dyn Error>> {
    match tokens.next() {
        Some(res) => {
            let got = res?;
            if want != got {
                Err(Box::from(ArithError::ExpectedGotToken(
                    want.to_string(),
                    got.to_string(),
                )))
            } else {
                Ok(())
            }
        }
        None => {
            if want != Token::EoF {
                Err(Box::from(ArithError::ExpectedGotToken(
                    want.to_string(),
                    Token::EoF.to_string(),
                )))
            } else {
                Ok(())
            }
        }
    }
}

fn parse(
    tokens: &mut impl Iterator<Item = Result<Token, Box<dyn Error>>>,
) -> Result<Term, Box<dyn Error>> {
    match tokens.next() {
        Some(res) => match res? {
            Token::True => Ok(Term::True),
            Token::False => Ok(Term::False),
            Token::Zero => Ok(Term::Zero),
            Token::Succ => Ok(Term::Succ(Box::from(parse(tokens)?))),
            Token::Pred => Ok(Term::Pred(Box::from(parse(tokens)?))),
            Token::IsZero => Ok(Term::IsZero(Box::from(parse(tokens)?))),
            Token::If => {
                let t1 = Box::from(parse(tokens)?);
                expect(tokens, Token::Then)?;
                let t2 = Box::from(parse(tokens)?);
                expect(tokens, Token::Else)?;
                let t3 = Box::from(parse(tokens)?);
                Ok(Term::If(t1, t2, t3))
            }
            tok => Err(Box::from(ArithError::UnexpectedToken(tok.to_string()))),
        },
        None => Err(Box::from(ArithError::UnexpectedToken(
            Token::EoF.to_string(),
        ))),
    }
}

type Evaluator = fn(Box<Term>) -> Box<Term>;
fn main() {
    let args: Vec<_> = env::args().skip(1).collect();
    let str_args: Vec<_> = args.iter().map(|s| s.as_str()).collect();
    let (eval, filename): (Evaluator, &&str) = match &str_args[..] {
        ["-small-step", filename] => (small_step, filename),
        ["-big-step", filename] => (big_step, filename),
        _ => usage(),
    };
    let file = File::open(filename).unwrap_or_else(|err| {
        eprintln!("{}", err);
        usage();
    });
    let mut tokens = BufReader::new(file).lines().flat_map(|res| match res {
        Ok(l) => l
            .split_whitespace()
            .map(|s| Token::from_str(s))
            .collect::<Vec<Result<_, _>>>()
            .into_iter(),
        Err(e) => vec![Err(Box::from(e))].into_iter(),
    });
    let ast = parse(&mut tokens).unwrap_or_else(|e| err_exit(e));
    expect(&mut tokens, Token::EoF).unwrap_or_else(|e| err_exit(e));
    print!("{}", eval(Box::from(ast)));
}
