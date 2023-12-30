use crate::{env::Env, expr::Expression, prelude::*};
use std::{cell::RefCell, rc::Rc};
use thiserror::Error;

/// Error representing runtime stdlib exceptions.
#[derive(Error, Debug)]
#[error("{msg}")]
pub struct StdErr {
    msg: String,
}

macro_rules! float {
    ($fl:literal) => {
        $fl.try_into().expect("don't put NaN here")
    };
}

pub mod core {
    use super::*;

    use crate::eval::{EvalError, Signal};

    pub fn eval(args: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        use crate::{eval, expr};
        let [arg] = args else {
            return Err(EvalError::WrongArgCount {
                required: 1,
                passed: args.len(),
            });
        };
        if let Expression::Quoted(expr::Quoted(q)) = arg {
            eval::eval(q, env)
        } else {
            Ok(arg.clone())
        }
    }

    pub fn exit(args: &[Expression], _env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        let status = match args {
            [] => None,
            [st] => Some(st),
            _ => {
                return Err(EvalError::WrongArgCount {
                    required: 0,
                    passed: args.len(),
                })
            }
        };
        let default = &Expression::Number(float!(0.));
        Err(try_signal_status(status.unwrap_or(default)).map_or_else(
            || {
                StdErr {
                    msg: "Expected unsigned 8 bit integer!".to_owned(),
                }
                .into()
            },
            |x| Signal::ExitSignal(x).into(),
        ))
    }

    fn try_signal_status(expr: &Expression) -> Option<u8> {
        let Expression::Number(i) = expr else {
            return None;
        };
        let i: f64 = (*i).into();
        if (i as u8) as f64 == i {
            Some(i as u8)
        } else {
            None
        }
    }
}

/// NOTE: math ops do NOT short-circuit
pub mod math {
    use ordered_float::NotNan;

    use crate::{eval::EvalError, expr::ExprError};

    use super::*;

    use std::ops::*;

    type Num = NotNan<f64>;

    #[inline]
    /// Creates an iterator that tries_into and returns [`Result<Num>`]
    fn numbers(args: &[Expression]) -> impl Iterator<Item = Result<Num, EvalError>> + '_ {
        args.iter()
            .map(move |a| a.try_into().map_err(|e: ExprError| e.into()))
    }

    trait ReduceOk {
        /// [`fold_ok`] but the first element of the iterator is the accumulator, akin to [`reduce`],
        /// returns [`None`] if no elements
        fn reduce_ok<I, E>(&mut self, f: fn(I, I) -> I) -> Result<Option<I>, E>
        where
            Self: Iterator<Item = Result<I, E>>,
        {
            self.fold_ok(None, |acc, e| match acc {
                Some(i) => Some(f(i, e)),
                None => Some(e),
            })
        }
    }

    impl<T> ReduceOk for T where T: Iterator<Item = Result<Num, EvalError>> {}

    pub fn add(args: &[Expression], _env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        numbers(args)
            .fold_ok(float!(0_f64), Add::add)
            .map(Expression::Number)
    }
    pub fn mul(args: &[Expression], _env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        numbers(args)
            .fold_ok(float!(1.), Mul::mul)
            .map(Expression::Number)
    }

    pub fn sub(args: &[Expression], _env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        // NOTE: I tested on gambit and `(-)` throws an error instead of returning the initial
        // value like other ops.
        // For simplicity i will also just return the initial
        numbers(args)
            .reduce_ok(Sub::sub)
            .map(|e| e.unwrap_or(float!(0.)))
            .map(Expression::Number)
    }

    pub fn div(args: &[Expression], _env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        numbers(args)
            .reduce_ok(Div::div)
            .map(|e| e.unwrap_or(float!(1.)))
            .map(Expression::Number)
    }

    pub fn eq(args: &[Expression], _env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        numbers(args)
            .fold_ok((true, None), |acc, e| match acc {
                // could be converted into an enum but it's fine
                (true, None) => (true, Some(e)),
                (true, Some(i)) => (e == i, Some(e)),
                (false, _) => (false, None),
            })
            .map(|(b, _)| Expression::Bool(b))
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::{eval, EvalError, Signal};
    use crate::parser;
    use anyhow::Result;

    use super::*;

    fn test_eval_expr(code: &str) -> Result<Expression, EvalError> {
        let tokens = parser::parse_expr(code)?;
        eval(&tokens, Env::new_global_rc())
    }

    mod math {
        use super::*;

        #[test]
        fn addition() -> Result<()> {
            let code = "(+ 2 3)";
            let result = test_eval_expr(code)?;
            assert_eq!(result.unwrap_number(), 5.);
            Ok(())
        }

        #[test]
        fn substraction() -> Result<()> {
            let code = "(- 4 3)";
            let result = test_eval_expr(code)?;
            assert_eq!(result.unwrap_number(), 1.);
            Ok(())
        }

        #[test]
        fn substraction_neg() -> Result<()> {
            let code = "(- 6 10)";
            let result = test_eval_expr(code)?;
            assert_eq!(result.unwrap_number(), -4.);
            Ok(())
        }

        #[test]
        fn multiplication() -> Result<()> {
            let code = "(* 3 5)";
            let result = test_eval_expr(code)?;
            assert_eq!(result.unwrap_number(), 15.);
            Ok(())
        }

        #[test]
        fn multiplication_nested() -> Result<()> {
            let code = "(* (+ 1 2) (+ 5 3))";
            let result = test_eval_expr(code)?;
            assert_eq!(result.unwrap_number(), 24.);
            Ok(())
        }

        #[test]
        fn division() -> Result<()> {
            let code = "(/ 3 5)";
            let result = test_eval_expr(code)?;
            assert_eq!(result.unwrap_number(), 3. / 5.);
            Ok(())
        }

        #[test]
        fn division_neg() -> Result<()> {
            let code = "(/ -6 7)";
            let result = test_eval_expr(code)?;
            assert_eq!(result.unwrap_number(), -6. / 7.);
            Ok(())
        }

        #[test]
        fn division_default() -> Result<()> {
            let code = "(/)";
            let result = test_eval_expr(code)?;
            assert_eq!(result.unwrap_number(), 1.);
            Ok(())
        }

        #[test]
        fn eq() -> Result<()> {
            let code = "(= 1 1)";
            let result = test_eval_expr(code)?;
            assert!(result.unwrap_bool());
            Ok(())
        }
    }

    mod core {
        use super::*;

        #[test]
        fn eval() -> Result<()> {
            let code = "(eval (quote (+ 1 3)))";
            let result = test_eval_expr(code)?;
            assert_eq!(result.unwrap_number(), 4.);
            Ok(())
        }

        #[test]
        fn exit() -> Result<()> {
            let code = "(exit)";
            let result = test_eval_expr(code);
            assert!(matches!(
                result.unwrap_err(),
                EvalError::Signal(Signal::ExitSignal(0))
            ));
            Ok(())
        }

        #[test]
        fn exit_custom_code() -> Result<()> {
            let code = "(exit 1)";
            let result = test_eval_expr(code);
            assert!(matches!(
                result.unwrap_err(),
                EvalError::Signal(Signal::ExitSignal(1))
            ));
            Ok(())
        }

        #[test]
        fn exit_too_many_args() -> Result<()> {
            let code = "(exit 1 5)";
            let result = test_eval_expr(code);
            assert!(matches!(
                result.unwrap_err(),
                EvalError::WrongArgCount {
                    required: 0,
                    passed: 2
                }
            ));
            Ok(())
        }
    }
}
