use ordered_float::NotNan;

use super::*;
use std::str::FromStr;

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    /// (
    LParen,
    /// )
    RParen,
    /// integer literal
    //TODO: add float support
    Number(NotNan<f64>),
    /// Any other group of characters
    Symbol(String),
}

impl Token {
    pub fn sym(s: &str) -> Self {
        Self::Symbol(s.to_owned())
    }

    pub fn num(f: f64) -> Result<Self> {
        Ok(Self::Number(NotNan::new(f)?))
    }
}

impl FromStr for Token {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "(" => Token::LParen,
            ")" => Token::RParen,
            // rip experimental feature for now
            // i if let Ok(i_parsed) = i.parse::<i64>() => Token::Integer(i_parsed)
            x => {
                if let Ok(f) = x.parse::<NotNan<f64>>() {
                    Token::Number(f)
                } else {
                    Token::Symbol(x.to_owned())
                }
            }
        })
    }
}

pub fn tokenize(source: &str) -> Vec<Token> {
    source
        .replace('(', " ( ")
        .replace(')', " ) ")
        .split_whitespace()
        .map(|lex| lex.parse().expect("Lexing never fails"))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// macro to setup test boilerplate for lexer::tokenize
    macro_rules! lexer_test {
        ($fn_name:ident, $code:literal, $expected:expr) => {
            #[test]
            fn $fn_name() -> Result<()> {
                let tokens = lexer::tokenize($code);
                assert_eq!(tokens, $expected);
                Ok(())
            }
        };
    }

    lexer_test!(empty, "", vec![]);

    lexer_test!(symbol, "test", vec![Token::sym("test")]);

    lexer_test!(integer, "42", vec![Token::num(42.)?]);

    lexer_test!(float, "5.10", vec![Token::num(5.10)?]);

    lexer_test!(neq_float, "-3.20", vec![Token::num(-3.20)?]);

    lexer_test!(
        empty_nested_lists,
        "(())",
        vec![Token::LParen, Token::LParen, Token::RParen, Token::RParen,]
    );

    lexer_test!(
        multiplication,
        "(* (+ 1 2) (- 5 3))",
        vec![
            Token::LParen,
            Token::sym("*"),
            Token::LParen,
            Token::sym("+"),
            Token::num(1.)?,
            Token::num(2.)?,
            Token::RParen,
            Token::LParen,
            Token::sym("-"),
            Token::num(5.)?,
            Token::num(3.)?,
            Token::RParen,
            Token::RParen,
        ]
    );

    lexer_test!(
        factorial,
        "(define (factorial n)
    (if (= n 0 )
        1
        (* n (factorial (- n 1)))))",
        vec![
            Token::LParen,
            Token::sym("define"),
            Token::LParen,
            Token::sym("factorial"),
            Token::sym("n"),
            Token::RParen,
            Token::LParen,
            Token::sym("if"),
            Token::LParen,
            Token::sym("="),
            Token::sym("n"),
            Token::num(0.)?,
            Token::RParen,
            Token::num(1.)?,
            Token::LParen,
            Token::sym("*"),
            Token::sym("n"),
            Token::LParen,
            Token::sym("factorial"),
            Token::LParen,
            Token::sym("-"),
            Token::sym("n"),
            Token::num(1.)?,
            Token::RParen,
            Token::RParen,
            Token::RParen,
            Token::RParen,
            Token::RParen,
        ]
    );
}
