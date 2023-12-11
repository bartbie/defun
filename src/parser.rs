// "Any sufficiently complicated C or Fortran program contains an ad hoc, informally-specified, bug-ridden, slow implementation of half of Common Lisp."
// - Peter Greenspun
// this lisp parser somehow evolved into poor man's parser combinators
use crate::{expr::Expression, lexer, lexer::Token};
use thiserror::Error;

/// Enum representing parser errors.
#[derive(Error, Debug)]
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

// a bit type-unsafe but it's fine for now
fn parse_atom(token: &Token) -> Expression {
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

fn parse_head(tokens: &[Token]) -> PResult<Option<Expression>, ()> {
    let (first, mut rest) = tokens.split_first().ok_or(())?;
    let result = match &first {
        Token::RParen => None,
        Token::Number(_) | Token::Symbol(_) => Some(parse_atom(first)),
        Token::LParen => {
            let l = parse_list(rest).map_err(|_| ())?;
            rest = l.1;
            Some(Expression::SList(l.0.into()))
        }
    };
    Ok((result, rest))
}

fn parse_list(mut tokens: &[Token]) -> PResult<Vec<Expression>> {
    let mut list: Vec<Expression> = vec![];
    loop {
        let Ok((maybe_exp, rest)) = parse_head(tokens) else {
            return Err(ParseError::UnclosedList);
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
        return Err(ParseError::UnexpectedEOF);
    };
    let Some(exp) = res else {
        return Err(ParseError::UnexpectedRParen);
    };
    Ok((exp, rest))
}

pub fn parse_single_expr(source: &str) -> Result<Expression, ParseError> {
    let tokens = lexer::tokenize(source);
    let (exp, rest) = parse_expr(&tokens)?;
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
        let (exp, rest) = parse_expr(unparsed)?;
        expressions.push(exp);
        unparsed = rest;
    }

    Ok(expressions)
}

#[cfg(test)]
mod tests {

    use super::super::*;
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
