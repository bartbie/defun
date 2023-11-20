use anyhow::{anyhow, bail, ensure, Error, Result};
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
    use expr::Expression;
    use lexer::Token;

    type PResult<'tok, T, E = Error> = Result<(T, &'tok [Token]), E>;

    // a bit type-unsafe but it's fine for now
    fn parse_atom(token: &Token) -> Expression {
        match &token {
            Token::Integer(i) => Expression::Integer(*i),
            Token::Symbol(s) => match s.as_str() {
                "#t" | "#true" => Expression::Bool(true),
                "#f" | "#false" => Expression::Bool(false),
                _ => Expression::Symbol(s.clone()),
            },
            _ => panic!("Received a token not convertible to an atom!"),
        }
    }

    fn parse_head(tokens: &[Token]) -> PResult<Option<Expression>, ()> {
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
            let Ok((maybe_exp, rest)) = parse_head(tokens) else {
                bail!("Unclosed list!")
            };
            let Some(exp) = maybe_exp else {
                return Ok((list, rest));
            };
            list.push(exp);
            tokens = rest;
        }
    }

    // this API could probably be much improved

    pub fn parse_expr(tokens: &[Token]) -> PResult<Expression> {
        let Ok((res, rest)) = parse_head(tokens) else {
            bail!("Unexpected EOF!")
        };
        let Some(exp) = res else {
            bail!("Unexpected ')'")
        };
        Ok((exp, rest))
    }

    pub fn parse_single_expr(source: &str) -> Result<Expression> {
        let tokens = lexer::tokenize(source)?;
        let (exp, rest) = parse_expr(&tokens)?;
        if !rest.is_empty() {
            bail!("Unexpected tokens after the expression!")
        }
        Ok(exp)
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

pub mod expr {
    use super::*;

    #[derive(Debug, Clone, Eq, PartialEq)]
    pub enum Expression {
        Symbol(String),
        Integer(i64), // TODO add floats later
        Bool(bool),
        List(Vec<Expression>),
        Lambda(fn(&[Expression]) -> Result<Expression>),
        Void,
    }

    impl Expression {
        pub fn sym(s: &str) -> Self {
            Self::Symbol(s.to_owned())
        }

        pub fn new_list() -> Self {
            Self::List(vec![])
        }
    }

    type Lambda = fn(&[Expression]) -> Result<Expression>;

    impl From<()> for Expression {
        fn from(_: ()) -> Self {
            Expression::Void
        }
    }

    impl From<i64> for Expression {
        fn from(value: i64) -> Self {
            Expression::Integer(value)
        }
    }

    impl From<bool> for Expression {
        fn from(value: bool) -> Self {
            Expression::Bool(value)
        }
    }

    impl From<Vec<Expression>> for Expression {
        fn from(value: Vec<Expression>) -> Self {
            Expression::List(value)
        }
    }

    impl From<Lambda> for Expression {
        fn from(value: Lambda) -> Self {
            Expression::Lambda(value)
        }
    }

    impl TryFrom<Expression> for i64 {
        type Error = Error;

        fn try_from(value: Expression) -> Result<Self, Self::Error> {
            if let Expression::Integer(i) = value {
                Ok(i)
            } else {
                bail!("Not a number!")
            }
        }
    }

    impl TryFrom<Expression> for () {
        type Error = ();

        fn try_from(value: Expression) -> Result<Self, Self::Error> {
            if let Expression::Void = value {
                Ok(())
            } else {
                Err(())
            }
        }
    }

    impl TryFrom<Expression> for Vec<Expression> {
        type Error = Error;

        fn try_from(value: Expression) -> Result<Self, Self::Error> {
            if let Expression::List(l) = value {
                Ok(l)
            } else {
                bail!("Not a list!")
            }
        }
    }

    impl TryFrom<Expression> for Lambda {
        type Error = Error;

        fn try_from(value: Expression) -> Result<Self, Self::Error> {
            if let Expression::Lambda(f) = value {
                Ok(f)
            } else {
                bail!("Not a procedure!")
            }
        }
    }

    impl TryFrom<Expression> for bool {
        type Error = Error;

        fn try_from(value: Expression) -> Result<Self, Self::Error> {
            if let Expression::Bool(b) = value {
                Ok(b)
            } else {
                bail!("Not a procedure!")
            }
        }
    }

    /// Creates a [`Expression::List`] like `vec!`.
    ///
    /// A thin wrapper around `vec!`, expands to `Expression::List(vec![/*...*/])`.
    /// - Create a [`Expression::List`] containing a given list of elements:
    ///
    /// ```
    /// # use defun::parser::{list, Expression};
    /// let Expression::List(l) = list![Expression::Integer(1), Expression::sym("2")] else {
    ///    panic!("Couldn't create Expression::List!");
    /// };
    /// assert_eq!(l[0], Expression::Integer(1));
    /// assert_eq!(l[1], Expression::sym("2"));
    /// ```
    #[macro_export]
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

    pub use list;
}

pub mod env {
    use super::*;
    use expr::Expression;
    use std::collections::HashMap;

    #[derive(Debug, Default)]
    pub struct Env {
        vars: HashMap<String, Expression>,
    }

    impl Env {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn new_global() -> Self {
            use builtins::*;
            let mut env = Self::new();
            env.set("+", Expression::Lambda(math::add));
            env
        }

        pub fn get(&self, name: &str) -> Option<Expression> {
            self.vars.get(name).cloned()
        }

        pub fn set(&mut self, name: &str, val: Expression) -> Option<Expression> {
            self.vars.insert(name.to_owned(), val)
        }
    }
}

pub mod builtins {
    use super::*;
    use expr::Expression;

    pub mod math {
        use super::*;

        pub fn add(args: &[Expression]) -> Result<Expression> {
            let sum: Result<i64> = args
                .iter()
                .map(|a| a.clone().try_into())
                .try_fold(0, |acc, e: Result<i64>| Ok(acc + e?));

            Ok(Expression::Integer(sum?))
        }
    }
}

pub mod eval {
    use super::*;
    use env::Env;
    use expr::Expression;

    fn get_sym(sym: &str, env: &mut Env) -> Result<Expression> {
        env.get(sym).ok_or(anyhow!("Unbounded symbol: {}!", sym))
    }

    fn eval_define(rest: &[Expression], env: &mut Env) -> Result<Expression> {
        let [sym, exp] = rest else {
            bail!("Ill-formed expression!")
        };
        let Expression::Symbol(name) = sym else {
            bail!("Expected identifier!")
        };
        let val = eval(exp, env)?;
        env.set(name, val);
        Ok(Expression::Void)
    }

    fn eval_bool(elem: &Expression, env: &mut Env) -> Result<bool> {
        let after_eval = eval(elem, env)?;
        // match after_eval {
        //     Expression::Bool(b) => *b,
        //     Expression::Integer(i) => *i != 0,
        //     Expression::Void => false,
        //     Expression::Symbol(_) => todo!(),
        //
        //     Expression::List(l) => eval_list(l, env)?,
        // };
        todo!()
    }

    fn eval_if(rest: &[Expression], env: &mut Env) -> Result<Expression> {
        let [cond, left, right] = rest else {
            bail!("Ill-formed expression!")
        };
        let test = eval_bool(cond, env)?;
        eval(if test { left } else { right }, env)
    }

    // fn eval_op(op: &Expression, env: &mut Env) {}
    //
    // TODO
    // handle special procedures
    // if let Expression::Symbol(sym) = head {
    //     match sym.as_str() {
    //         "define" => return eval_define(rest, env).map(|x| x.into()),
    //         "if" => return eval_if(rest, env),
    //     };
    // }

    fn eval_list(list: &[Expression], env: &mut Env) -> Result<Expression> {
        let [head, args @ ..] = list else {
            bail!("Ill-formed expression!")
        };

        // loop via recursion
        let op = match head {
            Expression::Symbol(sym) => get_sym(sym, env)?,
            Expression::List(l) => eval_list(l, env)?,
            _ => bail!("Operator is not a procedure!"),
        };

        let Expression::Lambda(f) = op else {
            bail!("Operator is not a procedure!")
        };

        f(args)
    }

    pub fn eval(exp: &Expression, env: &mut Env) -> Result<Expression> {
        Ok(match exp {
            Expression::Integer(i) => Expression::Integer(*i),
            Expression::Bool(b) => Expression::Bool(*b),
            Expression::Symbol(sym) => get_sym(sym, env)?,
            Expression::List(l) => eval_list(l, env)?,
            Expression::Lambda(fun) => Expression::Lambda(*fun),
            Expression::Void => Expression::Void,
        })
    }
}

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

    mod parser {
        use super::super::*;
        use expr::{list, Expression};

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
        mod single {
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

        mod script {
            use super::*;

            parser_test!(
                factorial,
                "(define (factorial n)
            (if (= n 0 ) 1 (* n (factorial (- n 1)))))
            (factorial 10)",
                vec![
                    list![
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
                    ],
                    list![Expression::sym("factorial"), Expression::Integer(10),],
                ]
            );
        }
    }
}
