use anyhow::anyhow;
use std::{env, fs, process, rc::Rc};

#[derive(Debug)]
enum Term<'a> {
    Var(i32),
    Abs(&'a str, Rc<Term<'a>>),
    App(Rc<Term<'a>>, Rc<Term<'a>>),
}

impl Term<'_> {
    #[allow(dead_code)]
    fn de_bruijn_string(&self) -> String {
        match self {
            Term::Abs(_, t1) => format!("(λ.{})", t1.de_bruijn_string()),
            Term::App(t1, t2) => format!("({} {})", t1.de_bruijn_string(), t2.de_bruijn_string()),
            Term::Var(x) => x.to_string(),
        }
    }
    fn context_string<'a>(&self, ctx: &Vec<String>) -> String {
        match self {
            Term::Abs(s, t1) => {
                let (ctx, s) = pick_fresh_name(ctx, s.to_string());
                format!("(λ{}.{})", s, t1.context_string(&ctx))
            }
            Term::App(t1, t2) => format!("({} {})", t1.context_string(ctx), t2.context_string(ctx)),
            Term::Var(x) => ctx[*x as usize].to_string(),
        }
    }
    fn is_val(&self) -> bool {
        if let Term::Abs(..) = self {
            true
        } else {
            false
        }
    }
}

fn pick_fresh_name(ctx: &Vec<String>, s: String) -> (Vec<String>, String) {
    if ctx.contains(&s) {
        pick_fresh_name(ctx, s + "'")
    } else {
        ([vec![s.clone()], ctx.to_vec()].concat(), s.clone())
    }
}

type Evaluator = fn(Rc<Term>) -> Rc<Term>;

fn usage() -> ! {
    eprintln!("usage: untyped ( -small-step | -big-step ) file\n");
    eprintln!("untyped is an implementation of the untyped lambda calculus (TAPL chapters 5-7).");
    process::exit(2);
}

fn err_exit(s: &str) -> ! {
    eprintln!("{}", s);
    process::exit(1);
}

fn small_step(t: Rc<Term>) -> Rc<Term> {
    match eval1(t.clone()) {
        Ok(t_prime) => small_step(t_prime),
        Err(_) => t,
    }
}

fn eval1(t: Rc<Term>) -> Result<Rc<Term>, anyhow::Error> {
    match &*t {
        Term::App(t1, t2) => match &**t1 {
            Term::Abs(x, t12) => {
                if t2.is_val() {
                    Ok(subst_stop(t2.clone(), t12.clone()))
                } else {
                    match eval1(t2.clone()) {
                        Ok(t2_prime) => Ok(Rc::new(Term::App(
                            Rc::new(Term::Abs(x, t12.clone())),
                            t2_prime,
                        ))),
                        err => err,
                    }
                }
            }
            _ => match eval1(t1.clone()) {
                Ok(t1_prime) => Ok(Rc::new(Term::App(t1_prime, t2.clone()))),
                err => err,
            },
        },
        _ => Err(anyhow!("no rule applies")),
    }
}

fn shift(d: i32, c: i32, t: Rc<Term>) -> Rc<Term> {
    match &*t {
        Term::Var(k) => {
            if *k < c {
                t
            } else {
                Rc::new(Term::Var(k + d))
            }
        }
        Term::Abs(s, t1) => Rc::new(Term::Abs(s, shift(d, c + 1, t1.clone()))),
        Term::App(t1, t2) => Rc::new(Term::App(shift(d, c, t1.clone()), shift(d, c, t2.clone()))),
    }
}

fn subst<'a>(j: i32, s: Rc<Term<'a>>, t: Rc<Term<'a>>) -> Rc<Term<'a>> {
    match &*t {
        Term::Var(k) => {
            if *k == j {
                s
            } else {
                t
            }
        }
        Term::Abs(x, t1) => Rc::new(Term::Abs(
            x,
            subst(j + 1, shift(1, 0, s.clone()), t1.clone()),
        )),
        Term::App(t1, t2) => Rc::new(Term::App(
            subst(j, s.clone(), t1.clone()),
            subst(j, s, t2.clone()),
        )),
    }
}

fn subst_stop<'a>(s: Rc<Term<'a>>, t: Rc<Term<'a>>) -> Rc<Term<'a>> {
    shift(-1, 0, subst(0, shift(1, 0, s), t))
}

fn big_step(t: Rc<Term>) -> Rc<Term> {
    match &*t {
        Term::App(t1, t2) => {
            let v1 = big_step(t1.clone());
            match &*v1 {
                Term::Abs(_, t12) => {
                    let v2 = big_step(t2.clone());
                    if v2.is_val() {
                        big_step(subst_stop(v2, t12.clone()))
                    } else {
                        t
                    }
                }
                _ => t,
            }
        }
        _ => t,
    }
}

fn unexpected(s: &str) -> ! {
    err_exit(format!("unexpected token \"{s}\"").as_str());
}

fn expect<'t, 's>(tok: &'t str, tokens: &'s [&'t str]) -> &'s [&'t str] {
    if tokens.len() == 0 {
        err_exit(format!("expected token \"{tok}\", got \"EOF\"").as_str());
    }
    let (hd, tl) = (tokens[0], &tokens[1..]);
    if hd != tok {
        err_exit(format!("expected token \"{tok}\", got \"{hd}\"").as_str());
    }
    tl
}

fn parse_lambda<'t, 's>(ctx: &Vec<&'t str>, tokens: &'s [&'t str]) -> (Term<'t>, &'s [&'t str]) {
    if tokens.len() == 0 {
        err_exit("expected identifier, got \"EOF\"");
    }
    let (tok, tokens) = (tokens[0], &tokens[1..]);
    let tokens = expect(".", tokens);
    let arr: &[&'t str] = &[tok];
    let n = [arr, &ctx].concat();
    let (body, tokens) = parse(&n, tokens);
    (Term::Abs(tok, Rc::new(body)), tokens)
}

fn parse_paren_expr<'t, 's>(
    ctx: &Vec<&'t str>,
    tokens: &'s [&'t str],
) -> (Term<'t>, &'s [&'t str]) {
    if tokens.len() == 0 {
        unexpected("EOF");
    }
    let (t, tokens) = parse(ctx, tokens);
    (t, expect(")", tokens))
}

fn parse_single<'t, 's>(ctx: &Vec<&'t str>, tokens: &'s [&'t str]) -> (Term<'t>, &'s [&'t str]) {
    if tokens.len() == 0 {
        unexpected("EOF");
    }
    let (tok, tokens) = (tokens[0], &tokens[1..]);
    match tok {
        ")" | "." => unexpected(tok),
        "(" => parse_paren_expr(ctx, tokens),
        "λ" => parse_lambda(ctx, tokens),
        _ => match ctx.iter().position(|&v| v == tok) {
            Some(i) => (Term::Var(i as i32), tokens),
            None => err_exit(format!("undefined variable \"{tok}\"").as_str()),
        },
    }
}

fn parse<'t, 's>(ctx: &Vec<&'t str>, tokens: &'s [&'t str]) -> (Term<'t>, &'s [&'t str]) {
    let (a, tokens) = parse_single(ctx, tokens);
    if tokens.len() == 0 || tokens[0] == ")" {
        return (a, tokens);
    }
    let (b, tokens) = parse_single(ctx, &tokens);
    (Term::App(Rc::new(a), Rc::new(b)), tokens)
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

fn filter<'a>(del: &'a str) -> impl Fn(&'a str) -> Vec<&'a str> {
    move |mut cur| {
        let mut v = Vec::new();
        while let Some((before, after)) = cur.split_once(del) {
            v.push(before);
            v.push(del);
            cur = after;
        }
        v.push(cur);
        v
    }
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
    let (ast, tokens) = parse(&vec![], &tokens);
    if tokens.len() != 0 {
        err_exit(format!("expected token \"EOF\", got \"{}\"", tokens[0]).as_str());
    }
    let ast = eval(Rc::new(ast));
    println!("{}", ast.context_string(&vec![]));
}
