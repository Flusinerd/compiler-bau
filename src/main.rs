use std::fs;

use clap::{App, Arg};

mod builtins;
mod bytecode;
mod bytecode_interpreter;
mod compiler;
mod debugger;
mod error_formatting;
mod expr;
mod gc;
mod input;
mod line_reader;
mod parser;
mod scanner;
mod value;

const INPUT_STR: &str = "INPUT";
const SHOW_TOKENS_STR: &str = "tokens";
const SHOW_AST_STR: &str = "ast";
const DISASSEMBLE_STR: &str = "disassemble";
const DEBUG_STR: &str = "debug";
const LITERAL_INPUT: &str = "c";

fn get_input(matches: &clap::ArgMatches) -> Option<input::Input> {
    if let Some(literal_input) = matches.value_of(LITERAL_INPUT) {
        return Some(input::Input {
            source: input::Source::Literal,
            content: literal_input.to_string(),
        });
    }
    if let Some(input_file) = matches.value_of(INPUT_STR) {
        match fs::read_to_string(input_file) {
            Ok(input) => {
                return Some(input::Input {
                    source: input::Source::File(input_file.to_string()),
                    content: input,
                });
            }
            Err(err) => {
                println!("Error reading {}: {}", input_file, err);
                std::process::exit(-1);
            }
        }
    }

    None
}

fn main() {
    let matches = App::new("bo")
        .version("0.0.1")
        .arg(
            Arg::with_name(INPUT_STR)
                .help("sets input file to use")
                .required(false)
                .index(1),
        )
        .arg(
            Arg::with_name(SHOW_TOKENS_STR)
                .long("--tokens")
                .takes_value(false)
                .help("show the token stream"),
        )
        .arg(
            Arg::with_name(SHOW_AST_STR)
                .long("--ast")
                .takes_value(false)
                .help("show the AST"),
        )
        .arg(
            Arg::with_name(DISASSEMBLE_STR)
                .long("--op-code")
                .takes_value(false)
                .help("show the bytecode"),
        )
        .arg(
            Arg::with_name(DEBUG_STR)
                .long("--debug")
                .takes_value(false)
                .help("run in the debugger"),
        )
        .arg(
            Arg::with_name(LITERAL_INPUT)
                .long("-input")
                .takes_value(true)
                .help("provide a literal string of Gina code"),
        )
        .get_matches();
    if let Some(input) = get_input(&matches) {
        if matches.is_present(SHOW_TOKENS_STR) || matches.is_present(SHOW_AST_STR) {
            match scanner::scan_tokens(input.content.clone()) {
                Ok(tokens) => {
                    if matches.is_present(SHOW_TOKENS_STR) {
                        println!("{:#?}", tokens);
                        std::process::exit(0);
                    }

                    let stmts_maybe = parser::parse(tokens);

                    match stmts_maybe {
                        Ok(stmts) => {
                            if matches.is_present(SHOW_AST_STR) {
                                println!("{:#?}", stmts);
                                std::process::exit(0);
                            }
                        }
                        Err(err) => {
                            error_formatting::format_parse_error(&err, &input);
                            std::process::exit(-1)
                        }
                    }
                }
                Err(err) => {
                    error_formatting::format_lexical_error(&err, &input);
                    std::process::exit(-1);
                }
            }
        }

        let func_or_err = compiler::Compiler::compile(input.content.clone());

        match func_or_err {
            Ok(func) => {
                if matches.is_present(DISASSEMBLE_STR) {
                    println!(
                        "{}",
                        bytecode_interpreter::disassemble_chunk(&func.chunk, "")
                    );
                    std::process::exit(0);
                }
                if matches.is_present(DEBUG_STR) {
                    debugger::Debugger::new(func, input.content).debug();
                    std::process::exit(0);
                }
                let mut interpreter = bytecode_interpreter::Interpreter::default();
                let res = interpreter.interpret(func);
                match res {
                    Ok(()) => {
                        std::process::exit(0);
                    }
                    Err(bytecode_interpreter::InterpreterError::Runtime(err)) => {
                        println!(
                            "Runtime error: {}\n\n{}",
                            err,
                            interpreter.format_backtrace()
                        );

                        std::process::exit(1);
                    }
                }
            }
            Err(err) => {
                error_formatting::format_compiler_error(&err, &input);
                std::process::exit(1);
            }
        }
    }
}
