use anyhow::{bail, ensure, Error, Result};
use std::path::PathBuf;

pub mod lexer {
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
}

pub mod parser {
    use super::*;
    use lexer::Token;

    type PResult<'tok, T, E = Error> = Result<(T, &'tok [Token]), E>;

    #[derive(Debug)]
    pub enum Expression {
        Symbol(String),
        Integer(i64),
        Bool(bool),
        List(Vec<Expression>),
    }

    fn split_first_or<'a>(seq: &'a [Token], reason: &'static str) -> PResult<'a, &'a Token> {
        let Some(x) = seq.split_first() else {
            bail!(reason)
        };
        Ok(x)
    }

    // a bit type-unsafe but it's fine for now
    fn parse_atom(token: &Token) -> Expression {
        match &token {
            Token::Integer(i) => Expression::Integer(*i),
            Token::Symbol(s) => match s.as_str() {
                "true" => Expression::Bool(true),
                "false" => Expression::Bool(false),
                _ => Expression::Symbol(s.clone()),
            },
            _ => panic!("Received a token not convertible to an atom!"),
        }
    }

    enum ParseFirstExp {
        End,
        Atom(Expression),
        List(Vec<Expression>),
    }

    fn parse_first(tokens: &[Token]) -> PResult<ParseFirstExp, ()> {
        let Some((first, mut rest)) = tokens.split_first() else {
            return Err(());
        };
        let result = match &first {
            Token::RParen => ParseFirstExp::End,
            Token::Integer(_) | Token::Symbol(_) => ParseFirstExp::Atom(parse_atom(first)),
            Token::LParen => {
                let l = parse_list(rest).map_err(|_| ())?;
                rest = l.1;
                ParseFirstExp::List(l.0)
            }
        };
        Ok((result, rest))
    }

    // TODO: refactor those to use parse_first

    fn parse_list(mut tokens: &[Token]) -> PResult<Vec<Expression>> {
        let mut list: Vec<Expression> = vec![];
        loop {
            let (first, mut rest) = split_first_or(tokens, "Unclosed list!")?;
            let exp = match &first {
                Token::RParen => return Ok((list, rest)),
                Token::Integer(_) | Token::Symbol(_) => parse_atom(first),
                Token::LParen => {
                    let l = parse_list(rest)?;
                    // dbg!(rest);
                    // dbg!(l.1);
                    rest = l.1;
                    Expression::List(l.0)
                }
            };
            list.push(exp);
            tokens = rest;
        }
    }

    pub fn parse_expr(tokens: &[Token]) -> PResult<Expression> {
        let (first, mut rest) = split_first_or(tokens, "Unexpected EOF!")?;
        Ok((
            match first {
                Token::RParen => bail!("Unexpected ')'"),
                Token::Integer(_) | Token::Symbol(_) => parse_atom(first),
                Token::LParen => {
                    let l = parse_list(rest)?;
                    rest = l.1;
                    dbg!(&rest);
                    Expression::List(l.0)
                }
            },
            rest,
        ))
    }

    pub fn parse_file(source: &str) -> Result<Expression> {
        parse_expr(&lexer::tokenize(source)?)
    }
}

pub mod eval {}

pub mod fs {
    use super::*;
    use anyhow::Context;

    #[derive(Debug)]
    pub struct RealPathBuf(pub PathBuf);

    impl RealPathBuf {
        /// read the file
        pub fn read(&self) -> Result<String> {
            std::fs::read_to_string(&self.0).with_context(|| {
                format!(
                    "Failed to read file: {}",
                    self.0
                        .to_str()
                        .expect("Unexpected UTF-8 error while opening a file!")
                )
            })
        }
    }

    impl TryFrom<PathBuf> for RealPathBuf {
        type Error = Error;

        fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
            ensure!(&value.is_file(), "File provided doesn't exist!");
            Ok(Self(value))
        }
    }
}

pub fn run_file(script: fs::RealPathBuf) -> Result<()> {
    let source_code = script.read()?;
    dbg!(&source_code);
    let tokens = lexer::tokenize(&source_code)?;
    dbg!(&tokens);
    let exp = parser::parse_expr(&tokens)?;
    dbg!(&exp);
    Ok(())
}

pub fn run_repl() -> Result<()> {
    todo!("later, for now running files")
}

pub fn run(script: Option<fs::RealPathBuf>) -> Result<()> {
    match script {
        Some(path) => run_file(path),
        None => run_repl(),
    }
}

#[cfg(test)]
mod tests {

    mod lexer {
        use super::super::*;
        use lexer::Token;

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
            fibbonacci,
            "
(define (factorial n)
    (if (= n 0 )
      1
      (* n (factorial (- n 1)))))
        ",
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
}
