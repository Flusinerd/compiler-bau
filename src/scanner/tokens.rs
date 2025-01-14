use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    FuncKeyword {
        line: usize,
        position: usize,
    },
    ReturnKeyword {
        line: usize,
        position: usize,
    },
    TypeKeyword {
        value: String,
        line: usize,
        position: usize,
    },
    Identifier {
        value: String,
        line: usize,
        position: usize,
    },
    Number {
        value: String,
        line: usize,
        position: usize,
    },
    Symbol {
        value: char,
        line: usize,
        position: usize,
    },
    EOF {
        line: usize,
        position: usize,
    },
}

impl PartialEq<Token> for &Token {
    fn eq(&self, other: &Token) -> bool {
        match (self, other) {
            (Token::FuncKeyword { .. }, Token::FuncKeyword { .. }) => true,
            (Token::ReturnKeyword { .. }, Token::ReturnKeyword { .. }) => true,
            (Token::TypeKeyword { value: a, .. }, Token::TypeKeyword { value: b, .. }) => true,
            (Token::Identifier { value: a, .. }, Token::Identifier { value: b, .. }) => true,
            (Token::Number { value: a, .. }, Token::Number { value: b, .. }) => true,
            (Token::Symbol { value: a, .. }, Token::Symbol { value: b, .. }) => true,
            (Token::EOF { .. }, Token::EOF { .. }) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::FuncKeyword { line, position } => {
                write!(f, "FuncKeyword at line {}, position {}", line, position)
            }
            Token::ReturnKeyword { line, position } => {
                write!(f, "ReturnKeyword at line {}, position {}", line, position)
            }
            Token::TypeKeyword {
                value,
                line,
                position,
            } => {
                write!(
                    f,
                    "TypeKeyword(value: \"{}\") at line {}, position {}",
                    value, line, position
                )
            }
            Token::Identifier {
                value,
                line,
                position,
            } => {
                write!(
                    f,
                    "Identifier(value: \"{}\") at line {}, position {}",
                    value, line, position
                )
            }
            Token::Number {
                value,
                line,
                position,
            } => {
                write!(
                    f,
                    "Number(value: \"{}\") at line {}, position {}",
                    value, line, position
                )
            }
            Token::Symbol {
                value,
                line,
                position,
            } => {
                write!(
                    f,
                    "Symbol(value: '{}') at line {}, position {}",
                    value, line, position
                )
            }
            Token::EOF { line, position } => {
                write!(f, "EOF at line {}, position {}", line, position)
            }
        }
    }
}
