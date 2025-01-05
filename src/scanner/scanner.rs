use regex::Regex;
use crate::scanner::tokens::Token;

pub struct Scanner {
    source: String,
    position: usize,
    line: usize,
    column: usize,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            position: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        // Define regex patterns and handlers
        let patterns: Vec<(Regex, Box<dyn Fn(&str, usize, usize) -> Option<Token>>)> = vec![
            (
                Regex::new(r"^func").unwrap(),
                Box::new(|_, line, position| Some(Token::FuncKeyword { line, position })),
            ),
            (
                Regex::new(r"^return").unwrap(),
                Box::new(|_, line, position| Some(Token::ReturnKeyword { line, position })),
            ),
            (
                Regex::new(r"^(int64|double)").unwrap(),
                Box::new(|s, line, position| Some(Token::TypeKeyword { value: s.to_string(), line, position })),
            ),
            (
                Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap(),
                Box::new(|s, line, position| Some(Token::Identifier { value: s.to_string(), line, position })),
            ),
            (
                Regex::new(r"^[0-9]+(\.[0-9]+)?").unwrap(),
                Box::new(|s, line, position| Some(Token::Number { value: s.to_string(), line, position })),
            ),
            (
                Regex::new(r"^[+\-*/%=,();{}]").unwrap(),
                Box::new(|s, line, position| Some(Token::Symbol { value: s.chars().next().unwrap(), line, position })),
            ),
            (
                Regex::new(r"^\s+").unwrap(),
                Box::new(|_, _, _| None), // Skip whitespace
            ),
        ];

        while self.position < self.source.len() {
            let remaining = &self.source[self.position..];
            let mut matched = false;

            for (regex, handler) in &patterns {
                if let Some(mat) = regex.find(remaining) {
                    matched = true;
                    let match_str = mat.as_str();
                    let current_position = self.position;

                    self.position += mat.end();

                    // Update line and column based on match
                    for ch in match_str.chars() {
                        if ch == '\n' {
                            self.line += 1;
                            self.column = 1;
                        } else {
                            self.column += 1;
                        }
                    }

                    // Call the handler and add token to the list
                    if let Some(token) = handler(match_str, self.line, current_position) {
                        tokens.push(token);
                    }
                    break;
                }
            }

            if !matched {
                panic!(
                    "Unexpected character at line {}, column {}, position {}: '{}'",
                    self.line,
                    self.column,
                    self.position,
                    remaining.chars().next().unwrap()
                );
            }
        }

        tokens.push(Token::EOF { line: self.line, position: self.position }); // Add EOF token
        tokens
    }
}
