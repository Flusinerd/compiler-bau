use std::fs;

mod bytecode;
mod compiler;
mod error_formatting;
mod expr;
mod input;
mod line_reader;
mod parser;
mod repl;
mod scanner;

fn get_input(args: Vec<String>) -> Option<input::Input> {
    if args.len() > 1 {
        let filename = &args[1];
        match fs::read_to_string(filename) {
            Ok(input) => {
                return Some(input::Input {
                    source: input::Source::File(filename.to_string()),
                    content: input,
                });
            }
            Err(err) => {
                println!("Error reading {}: {}", filename, err);
                std::process::exit(-1);
            }
        }
    }
    None
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if let Some(input) = get_input(args) {
        let tokens = scanner::scan_tokens(input.content.clone()).expect("Error scanning tokens");
        println!("{:?}", tokens);
        let stmts = parser::parse(tokens).expect("Error parsing tokens");
        println!("{:?}", stmts);
        let bytecode =
            compiler::Compiler::compile(input.content.clone()).expect("Error compiling tokens");
        println!("===Bytecode===");
        println!("{:?}", bytecode);
    } else {
        repl::run();
    }
}
