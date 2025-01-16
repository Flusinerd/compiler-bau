use crate::{error_formatting, input, line_reader, parser, scanner};

fn eval_tokens(mut tokens: Vec<scanner::Token>, recursion_depth: i64, line: &str) {
    let handle_err = |err| {
        error_formatting::format_parse_error(
            err,
            &input::Input {
                source: input::Source::Literal,
                content: line.to_string(),
            },
        );
    };
    match parser::parse(tokens.clone()) {
        Ok(stmts) => {
            println!("{:?}", stmts);
        }
        Err(
            err @ parser::Error::TokenMismatch {
                expected: scanner::TokenType::Semicolon,
                found:
                    scanner::Token {
                        ty: scanner::TokenType::Eof,
                        ..
                    },
                ..
            },
        ) => {
            let expected_eof = tokens.pop().unwrap();

            tokens.push(scanner::Token {
                ty: scanner::TokenType::Semicolon,
                lexeme: Vec::new(),
                literal: None,
                line: 0,
                col: -1,
            });
            tokens.push(expected_eof);

            if recursion_depth > 0 {
                handle_err(&err)
            } else {
                eval_tokens(tokens, recursion_depth + 1, line)
            }
        }
        Err(err) => handle_err(&err),
    }
}

pub fn run() {
    let mut line_reader = line_reader::LineReader::new(".repl-history.txt", ">>> ");
    println!(
        "===================================================\n\
         Welcome to the REPL of \"BO\" \n\
         ===================================================\n",
    );

    loop {
        let readline = line_reader.readline();

        match readline {
            line_reader::LineReadStatus::Line(line) => match scanner::scan_tokens(line.clone()) {
                Ok(tokens) => eval_tokens(tokens, 0, &line),
                Err(err) => {
                    error_formatting::format_lexical_error(
                        &err,
                        &input::Input {
                            source: input::Source::Literal,
                            content: line.to_string(),
                        },
                    );
                }
            },
            line_reader::LineReadStatus::Done => break,
        }
    }
}
