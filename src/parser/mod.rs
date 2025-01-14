mod ast;

use crate::parser::ast::ASTNode;
use crate::scanner::tokens::Token;

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    // Entry point for parsing
    pub fn parse_program(&mut self) -> ASTNode {
        let mut nodes = Vec::new();
        while !self.is_at_end() {
            nodes.push(self.parse_function());
        }
        ASTNode::Program(nodes)
    }

    fn error(&self, token: &Token, message: &str) -> ! {
        let (line, position) = match token {
            Token::FuncKeyword { line, position }
            | Token::ReturnKeyword { line, position }
            | Token::TypeKeyword { line, position, .. }
            | Token::Identifier { line, position, .. }
            | Token::Number { line, position, .. }
            | Token::Symbol { line, position, .. }
            | Token::EOF { line, position } => (*line, *position),
        };

        panic!(
            "Parser error at line {}, position {}: {}",
            line, position, message
        );
    }

    // Parse a function
    fn parse_function(&mut self) -> ASTNode {
        self.consume_func_keyword("Expected 'func' keyword");

        let name = self.consume_identifier("Expected function name");
        self.consume_symbol('(', "Expected '(' after function name");

        println!("Parsing params");
        let mut params = Vec::new();
        if !self.check_symbol(')') {
            loop {
                println!("loop");
                let param_type = self.consume_type();
                let param_name = self.consume_identifier("Expected parameter name");
                params.push((param_type, param_name));

                if !self.match_symbol(',') {
                    break;
                }
            }
        }
        println!("After if");

        self.consume_symbol(')', "Expected ')' after parameters");
        let return_type = self.consume_type();

        self.consume_symbol('{', "Expected '{' before function body");
        let mut body = Vec::new();
        while !self.check_symbol('}') {
            body.push(self.parse_statement());
        }

        self.consume_symbol('}', "Expected '}' after function body");

        ASTNode::Function {
            name,
            params,
            return_type,
            body,
        }
    }

    // Parse a statement
    fn parse_statement(&mut self) -> ASTNode {
        if self.match_token(&Token::ReturnKeyword {
            line: 0,
            position: 0,
        }) {
            let expr = self.parse_expression();
            self.consume_symbol(';', "Expected ';' after return statement");
            return ASTNode::Return(Box::new(expr));
        }

        let var_type = self.consume_type();
        let var_name = self.consume_identifier("Expected variable name");
        self.consume_symbol('=', "Expected '=' after variable name");
        let value = self.parse_expression();
        self.consume_symbol(';', "Expected ';' after variable declaration");

        ASTNode::VariableDeclaration {
            var_type,
            name: var_name,
            value: Box::new(value),
        }
    }

    // Parse an expression (supports binary operations)
    fn parse_expression(&mut self) -> ASTNode {
        self.parse_term()
    }

    // Parse a term (handles addition and subtraction)
    fn parse_term(&mut self) -> ASTNode {
        let mut node = self.parse_factor();

        while self.match_any(&[
            Token::Symbol {
                value: '+',
                line: 0,
                position: 0,
            },
            Token::Symbol {
                value: '-',
                line: 0,
                position: 0,
            },
        ]) {
            let operator = match self.previous() {
                Token::Symbol { value, .. } => value,
                _ => unreachable!(),
            };
            let right = self.parse_factor();
            node = ASTNode::BinaryOperation {
                operator,
                left: Box::new(node),
                right: Box::new(right),
            };
        }

        node
    }

    // Parse a factor (handles multiplication, division, and modulo)
    fn parse_factor(&mut self) -> ASTNode {
        let mut node = self.parse_primary();

        while self.match_any(&[
            Token::Symbol {
                value: '*',
                line: 0,
                position: 0,
            },
            Token::Symbol {
                value: '/',
                line: 0,
                position: 0,
            },
            Token::Symbol {
                value: '%',
                line: 0,
                position: 0,
            },
        ]) {
            let operator = match self.previous() {
                Token::Symbol { value, .. } => value,
                _ => unreachable!(),
            };
            let right = self.parse_primary();
            node = ASTNode::BinaryOperation {
                operator,
                left: Box::new(node),
                right: Box::new(right),
            };
        }

        node
    }

    // Parse a primary expression
    fn parse_primary(&mut self) -> ASTNode {
        if self.match_token(&Token::Number {
            value: "".to_string(),
            line: 0,
            position: 0,
        }) {
            return ASTNode::Number(self.previous_value());
        }

        if self.match_token(&Token::Identifier {
            value: "".to_string(),
            line: 0,
            position: 0,
        }) {
            return ASTNode::Identifier(self.previous_value());
        }

        if self.match_token(&Token::Symbol {
            value: '(',
            line: 0,
            position: 0,
        }) {
            let expr = self.parse_expression();
            self.consume_symbol(')', "Expected ')' after expression");
            return expr;
        }

        panic!("Unexpected token: {:?}", self.peek());
    }

    // Helper functions for token consumption
    fn consume_func_keyword(&mut self, message: &str) {
        if let Token::FuncKeyword { .. } = self.peek() {
            self.advance();
        } else {
            self.error(self.peek(), message);
        }
    }

    fn consume(&mut self, expected: &Token, message: &str) -> Token {
        if self.check(expected) {
            return self.advance();
        }
        self.error(self.peek(), message);
    }

    fn consume_type(&mut self) -> String {
        if let Token::TypeKeyword {
            value,
            line: _,
            position: _,
        } = self.advance()
        {
            return value;
        }
        self.error(&self.previous(), "Expected type keyword");
    }

    fn consume_identifier(&mut self, message: &str) -> String {
        if let Token::Identifier {
            value,
            line: _,
            position: _,
        } = self.advance()
        {
            return value;
        }
        self.error(&self.previous(), message);
    }

    fn consume_symbol(&mut self, expected: char, message: &str) {
        if self.check_symbol(expected) {
            self.advance();
            return;
        }
        self.error(self.peek(), message);
    }

    fn check_symbol(&self, symbol: char) -> bool {
        match self.peek_symbol() {
            Some(peeked_symbol) => peeked_symbol == symbol,
            None => false,
        }
    }

    fn match_token(&mut self, token: &Token) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_symbol(&mut self, symbol: char) -> bool {
        if self.check_symbol(symbol) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_any(&mut self, tokens: &[Token]) -> bool {
        for token in tokens {
            if self.check(token) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token: &Token) -> bool {
        let res = !self.is_at_end() && self.peek() == *token;
        println!("Check: {:?} == {:?}", self.peek(), token);
        res
    }

    fn advance(&mut self) -> Token {
        let token = self.tokens[self.position].clone();
        self.position += 1;
        token
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.position]
    }

    fn peek_symbol(&self) -> Option<char> {
        match &self.tokens[self.position] {
            Token::Symbol { value, .. } => Some(*value),
            _ => None,
        }
    }

    fn previous(&self) -> Token {
        self.tokens[self.position - 1].clone()
    }

    fn previous_value(&self) -> String {
        match self.previous() {
            Token::Identifier {
                value,
                line: _,
                position: _,
            }
            | Token::Number {
                value,
                line: _,
                position: _,
            } => value,
            _ => self.error(&self.previous(), "Expected a value"),
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek()
            == Token::EOF {
                line: 0,
                position: 0,
            }
    }
}
