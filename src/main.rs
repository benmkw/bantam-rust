mod bantam;
use crate::bantam::lexer::Lexer;

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let input = &args[1];

    let lexer = Lexer::new(input);
    let mut parser = bantam::Parser::new(lexer);

    let result = parser.parse_expression();
    println!("{input} => {result}", result = result.print());
}

// use
// cargo test -- --nocapture --test-threads=1
// to show output of successfull tests

// TODO maybe use expect-test
#[cfg(test)]
fn run(input: &str, expected: &str) {
    let lexer = Lexer::new(input);
    let mut parser = bantam::Parser::new(lexer);

    let result = parser.parse_expression();
    let actual = result.print();

    if actual == expected {
        println!("{input:<27} ==> {expected}");
    } else {
        panic!("FAILED: input: {input} expected {expected}, got {actual}");
    }
}

#[test]
fn basic() {
    println!();
    run("a()", "a()");
    run("a(b)", "a(b)");
    run("a(b, c)", "a(b, c)");
    run("a(b)(c)", "a(b)(c)");
    run("a(b) + c(d)", "(a(b) + c(d))");
    run("a(b ? c : d, e + f)", "a((b ? c : d), (e + f))");
}

#[test]
fn unary_precedences() {
    println!();
    run("~!-+a", "(~(!(-(+a))))");
    run("a!!!", "(((a!)!)!)");
}

#[test]
fn unary_and_binary_precedence() {
    println!();
    run("-a * b", "((-a) * b)");
    run("!a + b", "((!a) + b)");
    run("~a ^ b", "((~a) ^ b)");
    run("-a!", "(-(a!))");
    run("!a!", "(!(a!))");
}

#[test]
fn binary_precedence() {
    println!();
    run(
        "a = b + c * d ^ e - f / g",
        "(a = ((b + (c * (d ^ e))) - (f / g)))",
    );
}

#[test]
fn binary_associativity() {
    println!();
    run("a = b = c", "(a = (b = c))");
    run("a + b - c", "((a + b) - c)");
    run("a * b / c", "((a * b) / c)");
    run("a ^ b ^ c", "(a ^ (b ^ c))");
}

#[test]
fn conditional_operator() {
    println!();
    run("a ? b : c ? d : e", "(a ? b : (c ? d : e))");
    run("a ? b ? c : d : e", "(a ? (b ? c : d) : e)");
    run("a + b ? c * d : e / f", "((a + b) ? (c * d) : (e / f))");
}

#[test]
fn grouping() {
    println!();
    run("a + (b + c) + d", "((a + (b + c)) + d)");
    run("a ^ (b + c)", "(a ^ (b + c))");
    run("(!a)!", "((!a)!)");
}
