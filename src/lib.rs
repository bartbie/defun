use anyhow::{bail, ensure, Error, Result};
use std::path::PathBuf;

pub mod lexer {
    use super::*;
    use std::str::FromStr;

    #[derive(Debug)]
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

    #[derive(Debug)]
    pub enum Expression {
        Symbol(String),
        Integer(i64),
        Bool(bool),
        List(Vec<Expression>),
    }

    fn split_first_or<'a>(
        seq: &'a [Token],
        reason: &'static str,
    ) -> Result<(&'a Token, &'a [Token])> {
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

    // NOTE: these could be refactored to be share code?

    fn parse_list(mut tokens: &[Token]) -> Result<Vec<Expression>> {
        let mut list: Vec<Expression> = vec![];
        loop {
            let (first, rest) = split_first_or(tokens, "Unclosed list!")?;
            let exp = match &first {
                Token::RParen => return Ok(list),
                Token::Integer(_) | Token::Symbol(_) => parse_atom(first),
                Token::LParen => Expression::List(parse_list(rest)?),
            };
            list.push(exp);
            tokens = rest;
        }
    }

    pub fn parse_tokens(tokens: &[Token]) -> Result<Expression> {
        let (first, rest) = split_first_or(tokens, "Unexpected EOF!")?;
        Ok(match first {
            Token::RParen => bail!("Unexpected ')'"),
            Token::Integer(_) | Token::Symbol(_) => parse_atom(first),
            Token::LParen => Expression::List(parse_list(rest)?),
        })
    }

    pub fn parse(source: &str) -> Result<Expression> {
        parse_tokens(&lexer::tokenize(source)?)
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
    let exp = parser::parse_tokens(&tokens)?;
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
