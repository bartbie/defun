use super::*;
use env::Env;
use eval::eval;
use expr::Expression;
use std::{cell::RefCell, rc::Rc};

/// NOTE: math ops do NOT short-circuit
pub mod math {
    use super::*;

    use std::ops::*;

    #[inline]
    /// Creates an iterator that evals expressions and returns [`Result<i64>`]
    fn eval_num(
        args: &[Expression],
        env: Rc<RefCell<Env>>,
    ) -> impl Iterator<Item = Result<i64>> + '_ {
        args.iter().map(move |a| eval(a, env.clone())?.try_into())
    }

    trait MathFold {
        /// Iterator that folds [`Result<i64>`] via `op` if it's `Ok`.
        /// Does *NOT* short-circuit like `try_fold` or `fold_ok` as Lisps do not short-circuit their
        /// math operators.
        fn math_fold<T>(self, initial: T, op: fn(T, i64) -> T) -> Result<T>;
    }

    impl<I: Iterator<Item = Result<i64>>> MathFold for I {
        #[inline]
        fn math_fold<T>(self, initial: T, op: fn(T, i64) -> T) -> Result<T> {
            #[allow(clippy::manual_try_fold)] // we cannot short-circuit in here :(
            self.fold(Some(initial), |acc, e| Some(op(acc?, e.ok()?)))
                .ok_or(anyhow!("Not a number!")) // TODO: obviously unify the errors into proper types
        }
    }

    pub fn add(args: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression> {
        eval_num(args, env)
            .math_fold(0, Add::add)
            .map(Expression::Integer)
    }
    pub fn mul(args: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression> {
        eval_num(args, env)
            .math_fold(1, Mul::mul)
            .map(Expression::Integer)
    }
    pub fn sub(args: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression> {
        // NOTE: I tested on gambit and `(-)` throws an error instead of returning the initial
        // value like other ops.
        // For simplicity i will also just return the initial
        eval_num(args, env)
            .math_fold(0, Sub::sub)
            .map(Expression::Integer)
    }

    pub fn div(_args: &[Expression], _env: Rc<RefCell<Env>>) -> Result<Expression> {
        todo!("Division will be implemented when refactored to floats!")
    }

    pub fn eq(args: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression> {
        eval_num(args, env)
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
    use super::*;

    fn test_eval_expr(code: &str) -> Result<Expression> {
        let tokens = parser::parse_single_expr(code)?;
        eval(&tokens, Env::new_global_rc())
    }

    #[test]
    fn addition() -> Result<()> {
        let code = "(+ 2 3)";
        let result = test_eval_expr(code)?;
        assert_eq!(result.unwrap_integer(), 5);
        Ok(())
    }

    #[test]
    fn multiplication() -> Result<()> {
        let code = "(* 3 5)";
        let result = test_eval_expr(code)?;
        assert_eq!(result.unwrap_integer(), 15);
        Ok(())
    }

    #[test]
    fn multiplication_nested() -> Result<()> {
        let code = "(* (+ 1 2) (+ 5 3))";
        let result = test_eval_expr(code)?;
        assert_eq!(result.unwrap_integer(), 24);
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
