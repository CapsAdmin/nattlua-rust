use nattlua::code::*;
use nattlua::lexer::*;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    let contents = fs::read_to_string(path).unwrap();

    let code = Code::new(&contents, path);
    let mut lexer = Lexer::new(code);

    let (tokens, errors) = lexer.get_tokens();

    println!("errors: ");
    for error in &errors {
        println!("\t{}", error);
    }

    println!("tokens: ");
    for token in &tokens {
        println!("\t{}", token);
        for whitespace_token in &token.whitespace {
            println!("\t\t{}", whitespace_token);
        }
    }
}
