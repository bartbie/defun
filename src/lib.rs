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
    // "Any sufficiently complicated C or Fortran program contains an ad hoc, informally-specified, bug-ridden, slow implementation of half of Common Lisp."
    // - Peter Greenspun
    // this lisp parser somehow evolved into poor man's parser combinators
    use super::*;
    use lexer::Token;

    type PResult<'tok, T, E = Error> = Result<(T, &'tok [Token]), E>;

    #[derive(Debug, Eq, PartialEq)]
    pub enum Expression {
        Symbol(String),
        Integer(i64),
        Bool(bool),
        List(Vec<Expression>),
    }

    impl Expression {
        pub fn sym(s: &str) -> Self {
            Self::Symbol(s.to_owned())
        }

        pub fn new_list() -> Self {
            Self::List(vec![])
        }
    }

    /// Creates a new Expression::List like `vec!`.
    ///
    /// Essentially a thin wrapper around `vec!`.
    macro_rules! list {
        [] => (
            Expression::new_list()
        );
        [$elem:expr; $n:expr] => (
            Expression::List(vec![$elem; $n])
        );
        [$($x:expr),+ $(,)?] => (
            Expression::List(vec![$($x),+])
        );
    }

    pub(crate) use list;

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

    fn parse_first(tokens: &[Token]) -> PResult<Option<Expression>, ()> {
        let (first, mut rest) = tokens.split_first().ok_or(())?;
        let result = match &first {
            Token::RParen => None,
            Token::Integer(_) | Token::Symbol(_) => Some(parse_atom(first)),
            Token::LParen => {
                let l = parse_list(rest).map_err(|_| ())?;
                rest = l.1;
                Some(Expression::List(l.0))
            }
        };
        Ok((result, rest))
    }

    fn parse_list(mut tokens: &[Token]) -> PResult<Vec<Expression>> {
        let mut list: Vec<Expression> = vec![];
        loop {
            let Ok((maybe_exp, rest)) = parse_first(tokens) else {
                bail!("Unclosed list!")
            };
            let Some(exp) = maybe_exp else {
                return Ok((list, rest));
            };
            list.push(exp);
            tokens = rest;
        }
    }

    pub fn parse_expr(tokens: &[Token]) -> PResult<Expression> {
        let Ok((res, rest)) = parse_first(tokens) else {
            bail!("Unexpected EOF!")
        };
        let Some(exp) = res else {
            bail!("Unexpected ')'")
        };
        Ok((exp, rest))
    }

    pub fn parse_script(source: &str) -> Result<Vec<Expression>> {
        let tokens = lexer::tokenize(source)?;
        let mut expressions = vec![];
        let mut unparsed: &[Token] = &tokens;

        while !unparsed.is_empty() {
            let (exp, rest) = parse_expr(unparsed)?;
            expressions.push(exp);
            unparsed = rest;
        }

        Ok(expressions)
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
    let parsed_file = parser::parse_script(&source_code)?;
    dbg!(&parsed_file);
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
            factorial,
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

    mod parser {
        use super::super::*;
        use parser::{list, Expression};

        /// macro to setup test boilerplate for parser::parse_script
        macro_rules! parser_test {
            ($fn_name:ident, $code:literal, $expected:expr) => {
                #[test]
                fn $fn_name() -> Result<()> {
                    let exps = parser::parse_script($code)?;
                    assert_eq!(exps, $expected);
                    Ok(())
                }
            };
        }

        /// tests with singular expressions
        mod expr {
            use super::*;

            parser_test!(empty, "", vec![]);

            parser_test!(symbol, "test", vec![Expression::sym("test")]);

            parser_test!(integer, "42", vec![Expression::Integer(42)]);

            parser_test!(empty_nested_lists, "(())", vec![list![list![]]]);

            parser_test!(
                multiplication,
                "(* (+ 1 2) (- 5 3))",
                vec![list![
                    Expression::sym("*"),
                    list![
                        Expression::sym("+"),
                        Expression::Integer(1),
                        Expression::Integer(2),
                    ],
                    list![
                        Expression::sym("-"),
                        Expression::Integer(5),
                        Expression::Integer(3),
                    ]
                ]]
            );

            parser_test!(
                factorial,
                "(define (factorial n)
            (if (= n 0 ) 1 (* n (factorial (- n 1)))))",
                vec![list![
                    Expression::sym("define"),
                    list![Expression::sym("factorial"), Expression::sym("n"),],
                    list![
                        Expression::sym("if"),
                        list![
                            Expression::sym("="),
                            Expression::sym("n"),
                            Expression::Integer(0),
                        ],
                        Expression::Integer(1),
                        list![
                            Expression::sym("*"),
                            Expression::sym("n"),
                            list![
                                Expression::sym("factorial"),
                                list![
                                    Expression::sym("-"),
                                    Expression::sym("n"),
                                    Expression::Integer(1),
                                ],
                            ],
                        ],
                    ],
                ]]
            );
        }

        mod script {}
    }
}
