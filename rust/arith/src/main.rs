use std::env;
use std::fs::File;
use std::process;

fn usage() -> ! {
    eprintln!("usage: arith ( -small-step | -big-step ) file\n");
    eprintln!("arith is an implementation of the untyped calculus");
    eprintln!("of booleans and numbers (TAPL chapter 3 & 4).");
    process::exit(2);
}

fn small_step(file: File) {}

fn big_step(file: File) {}

fn main() {
    let args: Vec<_> = env::args().skip(1).collect();
    let str_args: Vec<_> = args.iter().map(|s| s.as_str()).collect();
    let (eval, filename): (fn(File), &&str) = match &str_args[..] {
        ["-small-step", filename] => (small_step, filename),
        ["-big-step", filename] => (big_step, filename),
        _ => usage(),
    };
    let mut file = File::open(filename).unwrap_or_else(|err| {
        eprintln!("{}", err);
        usage();
    });
    eval(file);
}
