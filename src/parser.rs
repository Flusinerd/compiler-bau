use std::fmt;

use crate::{
    expr,
    scanner::{self, TokenType},
};

#[derive(Default)]
struct Parser {
    tokens: Vec<scanner::Token>,
    current: usize,
    in_fundec: bool,
}

pub enum Error {
    UnexpectedToken(scanner::Token),
    TokenMismatch {
        expected: scanner::TokenType,
        found: scanner::Token,
        maybe_on_err_string: Option<String>,
    },
    MaxParamsExceeded {
        line: usize,
        col: i64,
    },
    ReturnNotInFunc {
        line: usize,
        col: i64,
    },
    TooManyArguments {
        line: usize,
        col: i64,
    },
    ExpectedExpression {
        token_type: scanner::TokenType,
        line: usize,
        col: i64,
    },
    InvalidTokenInUnaryOp {
        token_type: scanner::TokenType,
        line: usize,
        col: i64,
    },
    InvalidTokenInBinaryOp {
        token_type: scanner::TokenType,
        line: usize,
        col: i64,
    },
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Error::UnexpectedToken(tok) => write!(
                f,
                "Unexpected token {:?} at line={},col={}",
                tok.ty, tok.line, tok.col
            ),
            Error::TokenMismatch {
                maybe_on_err_string,
                expected,
                found,
            } => {
                write!(
                    f,
                    "Expected token {:?} but found {:?} at line={},col={}",
                    expected, found.ty, found.line, found.col
                )?;
                if let Some(on_err_string) = maybe_on_err_string {
                    write!(f, ": {}", on_err_string)?;
                }
                fmt::Result::Ok(())
            }
            Error::MaxParamsExceeded { line, col } => write!(
                f,
                "Cannot have more than 255 parameters in a declaration. Line={},col={}",
                line, col
            ),
            Error::ReturnNotInFunc { line, col } => write!(
                f,
                "return statement not enclosed in a FunDecl at line={},col={}",
                line, col
            ),
            Error::TooManyArguments { line, col } => write!(
                f,
                "Cannot have more than 255 arguments to a function call. Line={},col={}",
                line, col
            ),
            Error::ExpectedExpression {
                token_type,
                line,
                col,
            } => write!(
                f,
                "Expected expression, but found token {:?} at line={},col={}",
                token_type, line, col
            ),
            Error::InvalidTokenInUnaryOp {
                token_type,
                line,
                col,
            } => write!(
                f,
                "invalid token in unary op {:?} at line={},col={}",
                token_type, line, col
            ),
            Error::InvalidTokenInBinaryOp {
                token_type,
                line,
                col,
            } => write!(
                f,
                "invalid token in binary op {:?} at line={},col={}",
                token_type, line, col
            ),
        }
    }
}

pub fn parse(tokens: Vec<scanner::Token>) -> Result<Vec<expr::Stmt>, Error> {
    let mut p = Parser {
        tokens,
        ..Default::default()
    };
    let stmts_or_err = p.parse();

    match stmts_or_err {
        Ok(stmts_or_err) => {
            if !p.is_at_end() {
                let tok = &p.tokens[p.current];
                Err(Error::UnexpectedToken(tok.clone()))
            } else {
                Ok(stmts_or_err)
            }
        }
        Err(err) => Err(err),
    }
}

impl Parser {
    pub fn parse(&mut self) -> Result<Vec<expr::Stmt>, Error> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            let stmt = self.declaration()?;
            statements.push(stmt);
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Result<expr::Stmt, Error> {
        if self.matches(TokenType::Type) {
            return self.var_decl();
        }

        if self.matches(scanner::TokenType::Func) {
            return Ok(expr::Stmt::FunDecl(self.fun_decl()?));
        }

        self.statement()
    }

    fn fun_decl(&mut self) -> Result<expr::FuncDecl, Error> {
        let name_tok = self
            .consume(
                scanner::TokenType::Identifier,
                "Expected function name".to_string().as_ref(),
            )?
            .clone();

        let fun_symbol = expr::Symbol {
            name: String::from_utf8(name_tok.lexeme).unwrap(),
            line: name_tok.line,
            col: name_tok.col,
        };

        let (parameters, body) = self.params_and_body()?;

        Ok(expr::FuncDecl {
            name: fun_symbol,
            params: parameters,
            body,
        })
    }

    fn params_and_body(&mut self) -> Result<(Vec<expr::Param>, Vec<expr::Stmt>), Error> {
        self.consume(
            scanner::TokenType::LeftParen,
            "Expected ( after function name".to_string().as_ref(),
        )?;

        let mut parameters = Vec::new();

        if !self.check(scanner::TokenType::RightParen) {
            loop {
                if parameters.len() >= 255 {
                    let peek_tok = self.peek();
                    return Err(Error::MaxParamsExceeded {
                        line: peek_tok.line,
                        col: peek_tok.col,
                    });
                }

                let type_token = self
                    .consume(
                        scanner::TokenType::Type,
                        "Expected type before parameter name",
                    )?
                    .clone();

                let param_type = match &type_token.literal {
                    Some(scanner::Literal::Int64Type) => expr::Type::Int64,
                    Some(scanner::Literal::DoubleType) => expr::Type::Double,
                    _ => panic!("internal error in parser: expected type token"),
                };

                let name_token = self
                    .consume(scanner::TokenType::Identifier, "Expected parameter name")?
                    .clone();

                let symbol = expr::Symbol {
                    name: String::from_utf8(name_token.lexeme).unwrap(),
                    line: name_token.line,
                    col: name_token.col,
                };

                parameters.push(expr::Param {
                    ty: param_type,
                    name: symbol,
                });

                if !self.matches(scanner::TokenType::Comma) {
                    break;
                }
            }
        }
        let parameters = parameters;

        self.consume(
            scanner::TokenType::RightParen,
            "Expected ) after parameter list",
        )?;

        self.consume(
            scanner::TokenType::Type,
            "Expected return type after )".to_string().as_ref(),
        )?;

        self.consume(
            scanner::TokenType::LeftBrace,
            "Expected { before function body",
        )?;
        let saved_is_in_fundec = self.in_fundec;
        self.in_fundec = true;
        let body = self.block()?;
        self.in_fundec = saved_is_in_fundec;

        Ok((parameters, body))
    }

    fn var_decl(&mut self) -> Result<expr::Stmt, Error> {
        let name_token = self
            .consume(scanner::TokenType::Identifier, "Expected variable name")?
            .clone();

        let maybe_initializer = if self.matches(scanner::TokenType::Equal) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ; after variable declaration",
        )?;

        Ok(expr::Stmt::VarDecl(
            expr::Symbol {
                name: String::from_utf8(name_token.lexeme).unwrap(),
                line: name_token.line,
                col: name_token.col,
            },
            maybe_initializer,
        ))
    }

    fn statement(&mut self) -> Result<expr::Stmt, Error> {
        if self.matches(scanner::TokenType::Print) {
            return self.print_statement();
        }

        if self.matches(scanner::TokenType::LeftBrace) {
            return Ok(expr::Stmt::Block(self.block()?));
        }

        if self.matches(scanner::TokenType::Return) {
            return self.return_statement();
        }

        self.expression_statement()
    }

    fn return_statement(&mut self) -> Result<expr::Stmt, Error> {
        let prev_tok = self.previous().clone();

        if !self.in_fundec {
            return Err(Error::ReturnNotInFunc {
                line: prev_tok.line,
                col: prev_tok.col,
            });
        }

        let maybe_retval = if !self.matches(scanner::TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        if maybe_retval.is_some() {
            self.consume(
                scanner::TokenType::Semicolon,
                "Expected ; after return value",
            )?;
        }

        Ok(expr::Stmt::Return(
            expr::SourceLocation {
                line: prev_tok.line,
                col: prev_tok.col,
            },
            maybe_retval,
        ))
    }

    fn block(&mut self) -> Result<Vec<expr::Stmt>, Error> {
        let mut stmts = Vec::new();

        while !self.check(scanner::TokenType::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?)
        }

        self.consume(scanner::TokenType::RightBrace, "Expected } after block.")?;

        Ok(stmts)
    }

    fn print_statement(&mut self) -> Result<expr::Stmt, Error> {
        let expr = self.expression()?;
        self.consume(scanner::TokenType::Semicolon, "Expected ; after value")?;
        Ok(expr::Stmt::Print(expr))
    }

    fn expression_statement(&mut self) -> Result<expr::Stmt, Error> {
        let expr = self.expression()?;
        self.consume(scanner::TokenType::Semicolon, "Expected ; after value")?;
        Ok(expr::Stmt::Expr(expr))
    }

    fn expression(&mut self) -> Result<expr::Expr, Error> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<expr::Expr, Error> {
        let expr = self.addition()?;

        if self.matches(scanner::TokenType::Equal) {
            let new_value = self.assignment()?;

            if let expr::Expr::Variable(sym) = &expr {
                return Ok(expr::Expr::Assign(sym.clone(), Box::new(new_value)));
            }
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<expr::Expr, Error> {
        let mut expr = self.multiplication()?;

        while self.match_one_of(vec![scanner::TokenType::Minus, scanner::TokenType::Plus]) {
            let operator_token = self.previous().clone();
            let right = Box::new(self.multiplication()?);
            let binop_maybe = Parser::op_token_to_binop(&operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    expr = expr::Expr::Binary(left, binop, right);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<expr::Expr, Error> {
        let mut expr = self.unary()?;

        while self.match_one_of(vec![scanner::TokenType::Slash, scanner::TokenType::Star]) {
            let operator_token = self.previous().clone();
            let right = Box::new(self.unary()?);
            let binop_maybe = Parser::op_token_to_binop(&operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    expr = expr::Expr::Binary(left, binop, right);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<expr::Expr, Error> {
        if self.match_one_of(vec![scanner::TokenType::Minus]) {
            let operator_token = self.previous().clone();
            let right = Box::new(self.unary()?);
            let unary_op_maybe = Parser::op_token_to_unary_op(&operator_token);

            return match unary_op_maybe {
                Ok(unary_op) => Ok(expr::Expr::Unary(unary_op, right)),
                Err(err) => Err(err),
            };
        }
        self.call()
    }

    fn call(&mut self) -> Result<expr::Expr, Error> {
        let mut expr = self.primary()?;

        loop {
            if self.matches(scanner::TokenType::LeftParen) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: expr::Expr) -> Result<expr::Expr, Error> {
        let mut arguments = Vec::new();

        if !self.check(scanner::TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    let peek_tok = self.peek();
                    return Err(Error::TooManyArguments {
                        line: peek_tok.line,
                        col: peek_tok.col,
                    });
                }
                arguments.push(self.expression()?);
                if !self.matches(scanner::TokenType::Comma) {
                    break;
                }
            }
        }

        let token = self.consume(
            scanner::TokenType::RightParen,
            "Expected ) after arguments.",
        )?;

        Ok(expr::Expr::Call(
            Box::new(callee),
            expr::SourceLocation {
                line: token.line,
                col: token.col,
            },
            arguments,
        ))
    }

    fn primary(&mut self) -> Result<expr::Expr, Error> {
        if self.matches(scanner::TokenType::Number) {
            match &self.previous().literal {
                Some(scanner::Literal::Number(n)) => {
                    return Ok(expr::Expr::Literal(expr::Literal::Number(*n)))
                }
                Some(l) => panic!(
                    "internal error in parser: when parsing number, found literal {:?}",
                    l
                ),
                None => panic!("internal error in parser: when parsing number, found no literal"),
            }
        }
        if self.matches(scanner::TokenType::Identifier) {
            match &self.previous().literal {
                Some(scanner::Literal::Identifier(s)) => {
                    return Ok(expr::Expr::Variable(expr::Symbol {
                        name: s.clone(),
                        line: self.previous().line,
                        col: self.previous().col,
                    }))
                }
                Some(l) => panic!(
                    "internal error in parser: when parsing identifier, found literal {:?}",
                    l
                ),
                None => {
                    panic!("internal error in parser: when parsing identifier, found no literal")
                }
            }
        }
        if self.matches(scanner::TokenType::LeftParen) {
            let expr = Box::new(self.expression()?);
            self.consume(
                scanner::TokenType::RightParen,
                "Expected ')' after expression.",
            )?;
            return Ok(expr::Expr::Grouping(expr));
        }
        Err(Error::ExpectedExpression {
            token_type: self.peek().ty,
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    fn consume(
        &mut self,
        tok: scanner::TokenType,
        on_err_str: &str,
    ) -> Result<&scanner::Token, Error> {
        if self.check(tok) {
            return Ok(self.advance());
        }
        Err(Error::TokenMismatch {
            expected: tok,
            found: self.peek().clone(),
            maybe_on_err_string: Some(on_err_str.into()),
        })
    }

    fn op_token_to_unary_op(tok: &scanner::Token) -> Result<expr::UnaryOp, Error> {
        match tok.ty {
            scanner::TokenType::Minus => Ok(expr::UnaryOp {
                ty: expr::UnaryOpTy::Minus,
                line: tok.line,
                col: tok.col,
            }),
            _ => Err(Error::InvalidTokenInUnaryOp {
                token_type: tok.ty,
                line: tok.line,
                col: tok.col,
            }),
        }
    }

    fn op_token_to_binop(tok: &scanner::Token) -> Result<expr::BinaryOp, Error> {
        match tok.ty {
            scanner::TokenType::Plus => Ok(expr::BinaryOp {
                ty: expr::BinaryOpTy::Plus,
                line: tok.line,
                col: tok.col,
            }),
            scanner::TokenType::Minus => Ok(expr::BinaryOp {
                ty: expr::BinaryOpTy::Minus,
                line: tok.line,
                col: tok.col,
            }),
            scanner::TokenType::Star => Ok(expr::BinaryOp {
                ty: expr::BinaryOpTy::Star,
                line: tok.line,
                col: tok.col,
            }),
            scanner::TokenType::Slash => Ok(expr::BinaryOp {
                ty: expr::BinaryOpTy::Slash,
                line: tok.line,
                col: tok.col,
            }),
            _ => Err(Error::InvalidTokenInBinaryOp {
                token_type: tok.ty,
                line: tok.line,
                col: tok.col,
            }),
        }
    }

    fn match_one_of(&mut self, types: Vec<scanner::TokenType>) -> bool {
        for ty in types.iter() {
            if self.matches(*ty) {
                return true;
            }
        }
        false
    }

    fn matches(&mut self, ty: scanner::TokenType) -> bool {
        if self.check(ty) {
            self.advance();
            return true;
        }
        false
    }

    fn check(&self, ty: scanner::TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().ty == ty
    }

    fn advance(&mut self) -> &scanner::Token {
        if !self.is_at_end() {
            self.current += 1
        }

        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().ty == scanner::TokenType::Eof
    }

    fn peek(&self) -> &scanner::Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &scanner::Token {
        &self.tokens[self.current - 1]
    }
}
