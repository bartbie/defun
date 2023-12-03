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

#[cfg(test)]
mod tests {

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
