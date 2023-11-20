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
    Integer(i64),
    /// Any other group of characters
    Symbol(String),
}

impl Token {
    pub fn sym(s: &str) -> Self {
        Self::Symbol(s.to_owned())
    }
}

impl FromStr for Token {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "(" => Token::LParen,
            ")" => Token::RParen,
            // rip experimental feature for now
            // i if let Ok(i_parsed) = i.parse::<i64>() => Token::Integer(i_parsed)
            x => {
                if let Ok(i) = x.parse::<i64>() {
                    Token::Integer(i)
                } else {
                    Token::Symbol(x.to_owned())
                }
            }
        })
    }
}

pub fn tokenize(source: &str) -> Result<Vec<Token>> {
    source
        .replace('(', " ( ")
        .replace(')', " ) ")
        .split_whitespace()
        .map(|lex| lex.parse())
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
                let tokens = lexer::tokenize($code)?;
                assert_eq!(tokens, $expected);
                Ok(())
            }
        };
    }

    lexer_test!(empty, "", vec![]);

    lexer_test!(symbol, "test", vec![Token::sym("test")]);

    lexer_test!(integer, "42", vec![Token::Integer(42)]);

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
            Token::Integer(1),
            Token::Integer(2),
            Token::RParen,
            Token::LParen,
            Token::sym("-"),
            Token::Integer(5),
            Token::Integer(3),
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
            Token::Integer(0),
            Token::RParen,
            Token::Integer(1),
            Token::LParen,
            Token::sym("*"),
            Token::sym("n"),
            Token::LParen,
            Token::sym("factorial"),
            Token::LParen,
            Token::sym("-"),
            Token::sym("n"),
            Token::Integer(1),
            Token::RParen,
            Token::RParen,
            Token::RParen,
            Token::RParen,
            Token::RParen,
        ]
    );
}
