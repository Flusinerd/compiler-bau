use std::fs;

mod parser;
mod scanner;

fn main() {
    let contents =
        fs::read_to_string("program.bo").expect("Should have been able to read the file");

    let mut s = scanner::scanner::Scanner::new(contents);
    let tokens = s.scan_tokens();
    println!("{:?}", tokens);

    let mut parser = parser::Parser::new(tokens);
    let program = parser.parse_program();
    println!("{:?}", program);
}
