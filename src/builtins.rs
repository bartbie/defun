use super::*;
use env::Env;
use expr::Expression;
use std::{cell::RefCell, rc::Rc};

/// NOTE: math ops do NOT short-circuit
pub mod math {
    use ordered_float::NotNan;

    use crate::{eval::EvalError, expr::ExprError};

    use super::*;

    use std::ops::*;

    type Num = NotNan<f64>;

    #[inline]
    /// Creates an iterator that evals expressions and returns [`Result<Num>`]
    fn numbers(args: &[Expression]) -> impl Iterator<Item = Result<Num, EvalError>> + '_ {
        args.iter()
            .map(move |a| a.try_into().map_err(|e: ExprError| e.into()))
    }

    trait MathFold {
        /// Iterator that folds [`Result<Num>`] via `op` if it's `Ok`.
        /// Does *NOT* short-circuit like `try_fold` or `fold_ok` as Lisps do not short-circuit their
        /// math operators.
        fn math_fold<T>(self, initial: T, op: fn(T, Num) -> T) -> Result<T, EvalError>;
    }

    impl<I: Iterator<Item = Result<Num, EvalError>>> MathFold for I {
        #[inline]
        fn math_fold<T>(self, initial: T, op: fn(T, Num) -> T) -> Result<T, EvalError> {
            #[allow(clippy::manual_try_fold)] // we cannot short-circuit in here :(
            self.fold(Some(initial), |acc, e| Some(op(acc?, e.ok()?)))
                .ok_or(ExprError::NotAList.into())
        }
    }

    macro_rules! float {
        ($fl:literal) => {
            $fl.try_into().expect("don't put NaN here")
        };
    }

    pub fn add(args: &[Expression], _env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        numbers(args)
            .math_fold(float!(0_f64), Add::add)
            .map(Expression::Number)
    }
    pub fn mul(args: &[Expression], _env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        numbers(args)
            .math_fold(float!(1.), Mul::mul)
            .map(Expression::Number)
    }
    pub fn sub(args: &[Expression], _env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        // NOTE: I tested on gambit and `(-)` throws an error instead of returning the initial
        // value like other ops.
        // For simplicity i will also just return the initial
        numbers(args)
            .math_fold(float!(0.), Sub::sub)
            .map(Expression::Number)
    }

    pub fn div(args: &[Expression], _env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        numbers(args)
            .math_fold(float!(1.), Div::div)
            .map(Expression::Number)
    }

    pub fn eq(args: &[Expression], _env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        numbers(args)
            .math_fold((true, None), |acc, e| match acc {
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
    use crate::eval::{eval, EvalError};

    use super::*;

    fn test_eval_expr(code: &str) -> Result<Expression, EvalError> {
        let tokens = parser::parse_single_expr(code)?;
        eval(&tokens, Env::new_global_rc())
    }

    #[test]
    fn addition() -> Result<()> {
        let code = "(+ 2 3)";
        let result = test_eval_expr(code)?;
        assert_eq!(result.unwrap_number(), 5.);
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
    fn eq() -> Result<()> {
        let code = "(= 1 1)";
        let result = test_eval_expr(code)?;
        assert!(result.unwrap_bool());
        Ok(())
    }
}
