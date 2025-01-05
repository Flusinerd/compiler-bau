#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    FuncKeyword { line: usize, position: usize },
    ReturnKeyword { line: usize, position: usize },
    TypeKeyword { value: String, line: usize, position: usize },
    Identifier { value: String, line: usize, position: usize },
    Number { value: String, line: usize, position: usize },
    Symbol { value: char, line: usize, position: usize },
    EOF { line: usize, position: usize },
}

impl PartialEq<Token> for &Token {
    fn eq(&self, other: &Token) -> bool {
        match (self, other) {
            (Token::FuncKeyword { .. }, Token::FuncKeyword { .. }) => true,
            (Token::ReturnKeyword { .. }, Token::ReturnKeyword { .. }) => true,
            (Token::TypeKeyword { value: a, .. }, Token::TypeKeyword { value: b, .. }) => a == b,
            (Token::Identifier { value: a, .. }, Token::Identifier { value: b, .. }) => a == b,
            (Token::Number { value: a, .. }, Token::Number { value: b, .. }) => a == b,
            (Token::Symbol { value: a, .. }, Token::Symbol { value: b, .. }) => a == b,
            (Token::EOF { .. }, Token::EOF { .. }) => true,
            _ => false,
        }
    }
}