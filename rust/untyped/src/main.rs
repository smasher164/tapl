use std::{env, fs, process};

#[derive(Debug)]
enum Term<'a> {
    Var(usize),
    Abs(&'a str,&'a Term<'a>),
    App(&'a Term<'a>,&'a Term<'a>)
}

type Evaluator = fn(Term) -> Term;

fn usage() -> ! {
    eprintln!("usage: untyped ( -small-step | -big-step ) file\n");
    eprintln!("untyped is an implementation of the untyped lambda calculus (TAPL chapters 5-7).");
    process::exit(2);
}

fn err_exit(s: &str) -> ! {
    eprintln!("{}", s);
    process::exit(1);
}

fn small_step(t: Term) -> Term {
    todo!()
}

fn big_step(t: Term) -> Term {
    todo!()
}

fn main() {
    let args: Vec<_> = env::args().skip(1).collect();
    let str_args: Vec<_> = args.iter().map(|s| s.as_str()).collect();
    let (eval, filename): (Evaluator, &&str) = match &str_args[..] {
        ["-small-step", filename] => (small_step, filename),
        ["-big-step", filename] => (big_step, filename),
        _ => usage(),
    };
    let s = fs::read_to_string(filename).unwrap_or_else(|err| {
        eprintln!("{}", err);
        usage();
    });
    let tokens = scan(&s);
    // for tok in tokens {
    //     println!("TOK: {}", tok);
    // }
    let v = vec![];
    let (ast,_) = parse(&v, &tokens);
    println!("{:?}", ast);
}

fn unexpected(s: &str) -> ! {
    err_exit(format!("unexpected token \"{s}\"").as_str());
}

fn expect<'a>(tok: &'a str, tokens: &'a [&'a str]) -> &'a [&'a str] {
    if tokens.len() == 0 {
        err_exit(format!("expected token \"{tok}\", got \"EOF\"").as_str());
    }
    let (hd,tl) = (tokens[0], &tokens[1..]);
    if hd != tok {
        err_exit(format!("expected token \"{tok}\", got \"{hd}\"").as_str());
    }
    tl
}

fn parse_lambda<'a>(ctx: &'a [&'a str], tokens: &'a [&'a str]) -> (Term<'a>, &'a [&'a str]) {
    if tokens.len() == 0 {
        err_exit("expected identifier, got \"EOF\"");
    }
    let (tok,tokens) = (tokens[0], &tokens[1..]);
    let tokens = expect(".", tokens);
    // let arr : &'a [&'a str]= &[tok];
    // let n = [arr, ctx].concat();
    let (body:, tokens) = parse(ctx, tokens);
    (Term::Abs(tok, &body), tokens)
}

fn parse_paren_expr<'a>(ctx: &'a [&'a str], tokens:&'a [&'a str]) -> (Term<'a>, &'a [&'a str]) {
    if tokens.len() == 0 {
        unexpected("EOF");
    }
    let (t,tokens) = parse(ctx, tokens);
    (t,expect(")", tokens))
}

fn parse_single<'a>(ctx: &'a [&'a str], tokens: &'a [&'a str]) -> (Term<'a>, &'a [&'a str]) {
    if tokens.len() == 0 {
        unexpected("EOF");
    }
    let (tok,tokens) = (tokens[0], &tokens[1..]);
    match tok {
        ")" | "." => unexpected(tok),
        "(" => parse_paren_expr(ctx, tokens),
        "λ" => parse_lambda(ctx, tokens),
        _ => match ctx.iter().position(|&v| v == tok) {
            Some(i) => (Term::Var(i),tokens),
            None => err_exit(format!("undefined variable \"{tok}\"").as_str()),
        }
    }
}

fn parse<'a>(ctx: &'a [&'a str], tokens: &'a [&'a str]) -> (Term<'a>, &'a [&'a str]) {
    let (a,tokens) = parse_single(ctx, tokens);
    if tokens.len() == 0 || tokens[0] == ")" {
        return (a, tokens);
    }
    let (b,tokens) = parse_single(ctx, &tokens);
    (Term::App(&a,&b),tokens)
}

fn scan(s: &str) -> Vec<&str> {
    s.split_ascii_whitespace()
        .flat_map(filter("("))
        .flat_map(filter(")"))
        .flat_map(filter("."))
        .flat_map(filter("λ"))
        .filter(|s| !s.is_empty())
        .collect()
}

fn filter<'a>(del: &'a str) -> impl 'a + Fn(&'a str) -> Vec<&'a str> {
    move |s: &str| {
        let mut v = Vec::new();
        let mut cur = s;
        loop {
            match cur.split_once(del) {
                Some((before, after)) => {
                    if before != "" {
                        v.push(before);
                    }
                    cur = after;
                    v.push(del);
                }
                None => {
                    v.push(cur);
                    break;
                }
            }
        }
        v
    }
}
