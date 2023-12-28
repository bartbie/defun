// "Any sufficiently complicated C or Fortran program contains an ad hoc, informally-specified, bug-ridden, slow implementation of half of Common Lisp."
// - Peter Greenspun
// this lisp parser somehow evolved into poor man's parser combinators
use crate::{expr::Expression, lexer, lexer::Token};
use thiserror::Error;

/// Enum representing parser errors.
#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("Unclosed list!")]
    UnclosedList,
    #[error("Unexpected EOF!")]
    UnexpectedEOF,
    #[error("unexpected tokens after the expression!")]
    UnexpectedTokens,
    #[error("unexpected ')'")]
    UnexpectedRParen,
}

type PResult<'tok, T, E = ParseError> = Result<(T, &'tok [Token]), E>;

/// module dedicated to parsing a singular Expression
mod single_expr {
    use super::*;
    #[derive(Error, Debug, Clone)]
    /// implementation error representing `parse_single` errors
    pub enum Error {
        #[error("Empty slice!")]
        EmptySlice,
        #[error(transparent)]
        ParseErr(#[from] ParseError),
    }

    impl Error {
        pub fn on_empty(self, slice_alt: ParseError) -> ParseError {
            match self {
                Error::EmptySlice => slice_alt,
                Error::ParseErr(e) => e.clone(),
            }
        }
    }

    /// Reqursively parse the tokens until an [`Expression`] is parsed,
    /// the [`Token::RParen`] is hit or an error occurs
    pub fn parse(tokens: &[Token]) -> PResult<Option<Expression>, Error> {
        let (first, mut rest) = tokens.split_first().ok_or(Error::EmptySlice)?;
        let result = match &first {
            Token::RParen => None,
            Token::Number(_) | Token::Symbol(_) => Some(parse_atom(first)),
            Token::LParen => {
                let l = parse_s_list(rest)?;
                rest = l.1;
                Some(Expression::SList(l.0.into()))
            }
            Token::Apos => {
                let res = parse(rest)?;
                rest = res.1;
                let quoted = res.0.ok_or(ParseError::UnexpectedRParen)?;
                Some(Expression::quote(quoted))
            }
        };
        Ok((result, rest))
    }

    // helper methods for parse_req

    #[inline]
    /// Parses an atom into [`Expression`].
    /// panics when passedn a variant other than
    /// [`Token::Number`] or [`Token::Symbol`]
    fn parse_atom(token: &Token) -> Expression {
        // a bit type-unsafe but it's fine for now
        match &token {
            Token::Number(i) => Expression::Number(*i),
            Token::Symbol(s) => match s.as_str() {
                "#t" | "#true" => Expression::Bool(true),
                "#f" | "#false" => Expression::Bool(false),
                // inspired by Gambit
                "#!v" | "#!void" => Expression::Void,
                _ => Expression::Symbol(s.clone()),
            },
            _ => panic!("Received a token not convertible to an atom!"),
        }
    }

    #[inline]
    /// Parses an slist into [`Expression`].
    fn parse_s_list(mut tokens: &[Token]) -> PResult<Vec<Expression>> {
        let mut list: Vec<Expression> = vec![];
        loop {
            let (maybe_exp, rest) =
                parse(tokens).map_err(|e| e.on_empty(ParseError::UnclosedList))?;
            let Some(exp) = maybe_exp else {
                return Ok((list, rest));
            };
            list.push(exp);
            tokens = rest;
        }
    }
}

// this API could probably be much improved

pub fn parse_tokens(tokens: &[Token]) -> PResult<Expression> {
    let (res, rest) =
        single_expr::parse(tokens).map_err(|e| e.on_empty(ParseError::UnexpectedEOF))?;
    let Some(exp) = res else {
        return Err(ParseError::UnexpectedRParen);
    };
    Ok((exp, rest))
}

pub fn parse_expr(source: &str) -> Result<Expression, ParseError> {
    let tokens = lexer::tokenize(source);
    let (exp, rest) = parse_tokens(&tokens)?;
    if !rest.is_empty() {
        return Err(ParseError::UnexpectedTokens);
    }
    Ok(exp)
}

pub fn parse_script(source: &str) -> Result<Vec<Expression>, ParseError> {
    let tokens = lexer::tokenize(source);
    let mut expressions = vec![];
    let mut unparsed: &[Token] = &tokens;

    while !unparsed.is_empty() {
        let (exp, rest) = parse_tokens(unparsed)?;
        expressions.push(exp);
        unparsed = rest;
    }

    Ok(expressions)
}

#[cfg(test)]
mod tests {

    use super::super::*;
    use anyhow::Result;
    use expr::{s_list, Expression};

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

        parser_test!(number, "42", vec![Expression::num(42.)?]);

        parser_test!(float, "3.12", vec![Expression::num(3.12)?]);

        parser_test!(neg_float, "-0.12", vec![Expression::num(-0.12)?]);

        parser_test!(empty_nested_lists, "(())", vec![s_list![s_list![]]]);

        parser_test!(
            multiplication,
            "(* (+ 1 2) (- 5 3))",
            vec![s_list![
                Expression::sym("*"),
                s_list![
                    Expression::sym("+"),
                    Expression::num(1.)?,
                    Expression::num(2.)?,
                ],
                s_list![
                    Expression::sym("-"),
                    Expression::num(5.)?,
                    Expression::num(3.)?,
                ]
            ]]
        );

        parser_test!(
            factorial,
            "(define (factorial n)
            (if (= n 0 ) 1 (* n (factorial (- n 1)))))",
            vec![s_list![
                Expression::sym("define"),
                s_list![Expression::sym("factorial"), Expression::sym("n"),],
                s_list![
                    Expression::sym("if"),
                    s_list![
                        Expression::sym("="),
                        Expression::sym("n"),
                        Expression::num(0.)?,
                    ],
                    Expression::num(1.)?,
                    s_list![
                        Expression::sym("*"),
                        Expression::sym("n"),
                        s_list![
                            Expression::sym("factorial"),
                            s_list![
                                Expression::sym("-"),
                                Expression::sym("n"),
                                Expression::num(1.)?,
                            ],
                        ],
                    ],
                ],
            ]]
        );

        parser_test!(quote, "'12", vec![Expression::quote(Expression::num(12.)?)]);

        parser_test!(
            quoted_list,
            "'(x y)",
            vec![Expression::quote(s_list![
                Expression::sym("x"),
                Expression::sym("y"),
            ]),]
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
                s_list![
                    Expression::sym("define"),
                    s_list![Expression::sym("factorial"), Expression::sym("n"),],
                    s_list![
                        Expression::sym("if"),
                        s_list![
                            Expression::sym("="),
                            Expression::sym("n"),
                            Expression::num(0.)?,
                        ],
                        Expression::num(1.)?,
                        s_list![
                            Expression::sym("*"),
                            Expression::sym("n"),
                            s_list![
                                Expression::sym("factorial"),
                                s_list![
                                    Expression::sym("-"),
                                    Expression::sym("n"),
                                    Expression::num(1.)?,
                                ],
                            ],
                        ],
                    ],
                ],
                s_list![Expression::sym("factorial"), Expression::num(10.)?,],
            ]
        );
    }
}
