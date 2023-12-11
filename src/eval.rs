use crate::{expr::ExprError, parser::ParseError};

use self::special::eval_begin;

use super::*;
use env::Env;
use expr::{Call, Expression, Lambda};
use std::{cell::RefCell, rc::Rc};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("Unbound symbol: {0}!")]
    UnboundSymbol(String),
    #[error("Ill-formed expression!")]
    IllFormedExpr,
    #[error("Expected identifier!")]
    ExpectedIdent,
    #[error("Operator is not a procedure!")]
    OpNotProc,
    #[error("This procedure requires {required} arguments, {passed} passed!")]
    WrongArgCount { required: usize, passed: usize },
    #[error(transparent)]
    ParseErr(#[from] ParseError),
    #[error(transparent)]
    ExprErr(#[from] ExprError),
}

fn get_sym(sym: &str, env: &mut Env) -> Result<Expression, EvalError> {
    env.get(sym).ok_or(EvalError::UnboundSymbol(sym.to_owned()))
}

/// special forms that require different evaluation than normal procedures
mod special {
    use std::str::FromStr;

    use super::*;

    pub enum SpecialForm {
        If,
        Define,
        Set,
        And,
        Or,
        Lambda,
        Begin,
    }

    impl FromStr for SpecialForm {
        type Err = (); // no need for more here for now

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(match s {
                "if" => Self::If,
                "define" => Self::Define,
                "set!" => Self::Set,
                "and" => Self::And,
                "or" => Self::Or,
                "lambda" => Self::Lambda,
                "begin" => Self::Begin,
                _ => return Err(()),
            })
        }
    }

    pub fn eval_special(
        form: SpecialForm,
        rest: &[Expression],
        env: Rc<RefCell<Env>>,
    ) -> Result<Expression, EvalError> {
        match form {
            SpecialForm::If => eval_if(rest, env),
            SpecialForm::Define => eval_define(rest, env),
            SpecialForm::Set => eval_set(rest, env),
            SpecialForm::And => eval_and(rest, env),
            SpecialForm::Or => eval_or(rest, env),
            SpecialForm::Lambda => eval_lambda(rest, env),
            SpecialForm::Begin => eval_begin(rest, env),
        }
    }

    pub fn eval_define(
        rest: &[Expression],
        env: Rc<RefCell<Env>>,
    ) -> Result<Expression, EvalError> {
        let [sym, exp] = rest else {
            return Err(EvalError::IllFormedExpr);
        };
        let Expression::Symbol(name) = sym else {
            return Err(EvalError::ExpectedIdent);
        };
        let val = eval(exp, env.clone())?;
        env.borrow_mut().set(name, val);
        Ok(Expression::Void)
    }

    pub fn eval_if(rest: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        let [cond, left, right] = rest else {
            return Err(EvalError::IllFormedExpr);
        };
        let (_, test) = eval_cond(cond, env.clone())?;

        eval(if test { left } else { right }, env)
    }

    /// In Scheme ifs just check if condition is equal to `false`,
    /// no actual type coercion is happening.
    fn eval_cond(exp: &Expression, env: Rc<RefCell<Env>>) -> Result<(Expression, bool), EvalError> {
        let exp = eval(exp, env)?;

        let b = match exp {
            Expression::Bool(b) => b,
            _ => true,
        };

        Ok((exp, b))
    }

    pub fn eval_and(rest: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        let [left, right] = rest else {
            return Err(EvalError::IllFormedExpr);
        };

        let left = eval_cond(left, env.clone())?;
        if left.1 {
            let right = eval_cond(right, env)?;
            if right.1 {
                Ok(right.0)
            } else {
                Ok(left.0)
            }
        } else {
            Ok(left.0)
        }
    }

    pub fn eval_or(rest: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        let [left, right] = rest else {
            return Err(EvalError::IllFormedExpr);
        };

        let (eval_left, test) = eval_cond(left, env.clone())?;
        if test {
            Ok(eval_left)
        } else {
            let (eval_right, _) = eval_cond(right, env)?;
            Ok(eval_right)
        }
    }

    pub fn eval_set(rest: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        let [name, new_val] = rest else {
            return Err(EvalError::IllFormedExpr);
        };
        let Expression::Symbol(name) = name else {
            return Err(EvalError::ExpectedIdent);
        };

        let new_val = eval(new_val, env.clone())?;
        env.borrow_mut().set(name, new_val);
        Ok(Expression::Void)
    }

    fn check_args(args: &Expression) -> Result<Vec<String>, EvalError> {
        match args {
            Expression::Symbol(s) => Ok(vec![s.clone()]),
            Expression::List(l) => l
                .iter()
                .map(|e| match e {
                    Expression::Symbol(s) => Ok(s),
                    _ => Err(EvalError::ExpectedIdent),
                })
                .fold_ok(vec![], |acc, e| acc.tap_mut(|v| v.push(e.clone()))),
            _ => Err(EvalError::ExpectedIdent),
        }
    }

    pub fn eval_lambda(
        rest: &[Expression],
        env: Rc<RefCell<Env>>,
    ) -> Result<Expression, EvalError> {
        let [args, _ignored @ .., body] = rest else {
            return Err(EvalError::IllFormedExpr);
        };
        let args = check_args(args)?;

        Ok(Expression::Lambda(Lambda {
            args,
            body: Rc::new(body.clone()),
            env: env.clone(),
        }))
    }

    pub fn eval_begin(rest: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        rest.iter()
            .map(|e| eval(e, env.clone()))
            .fold_ok(Expression::Void, |_, e| e)
    }
}

fn eval_list(list: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
    let [head, args @ ..] = list else {
        return Err(EvalError::IllFormedExpr);
    };

    // handle special procedures
    if let Expression::Symbol(sym) = head {
        if let Ok(form) = sym.parse() {
            return special::eval_special(form, args, env);
        }
    }

    // loop via recursion
    let op = match head {
        Expression::Symbol(sym) => get_sym(sym, &mut env.borrow_mut())?,
        Expression::List(l) => eval_list(l, env.clone())?,
        _ => return Err(EvalError::OpNotProc),
    };

    // eval args
    let args: Vec<_> = args
        .iter()
        .map(|a| eval::eval(a, env.clone()))
        .collect::<Result<_, _>>()?;

    match op {
        Expression::Proc(f) => f.call(&args, env),
        Expression::Lambda(l) => l.call(&args, env),
        _ => Err(EvalError::OpNotProc),
    }
}

pub fn eval(exp: &Expression, env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
    Ok(match exp {
        Expression::Symbol(sym) => get_sym(sym, &mut env.borrow_mut())?,
        Expression::List(l) => eval_list(l, env)?,
        Expression::Number(i) => Expression::Number(*i),
        Expression::Bool(b) => Expression::Bool(*b),
        Expression::Void => Expression::Void,
        // those will happen when evaluating other stuff
        Expression::Proc(fun) => Expression::Proc(*fun),
        Expression::Lambda(l) => Expression::Lambda(l.clone()),
    })
}

pub fn eval_script(exps: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
    eval_begin(exps, env)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_eval_expr(code: &str) -> Result<Expression, EvalError> {
        let tokens = parser::parse_single_expr(code)?;
        dbg!(eval(&tokens, Env::new_global_rc()))
    }

    fn test_eval_script(code: &str) -> Result<Expression, EvalError> {
        let tokens = parser::parse_script(code)?;
        dbg!(eval_script(&tokens, Env::new_global_rc()))
    }

    #[test]
    fn number() -> Result<()> {
        let number = 25_f64;
        let result = test_eval_expr(&25.to_string())?;
        assert_eq!(result.unwrap_number(), number);
        Ok(())
    }

    #[test]
    fn symbol() -> Result<()> {
        let symbol = "+";
        let result = test_eval_expr(symbol)?;
        assert_eq!(
            // SAFETY: while this is not normally guaranteed to be equal due to LLVM shenanigans,
            // we put that pointer address in the expression at runtime so they should always be the same.
            result.unwrap_proc().0 as usize,
            builtins::math::add as usize
        );
        Ok(())
    }

    #[test]
    fn bool() -> Result<()> {
        let val = true;
        let result = test_eval_expr(if val { "#t" } else { "#f" })?;
        assert_eq!(result.unwrap_bool(), val);
        Ok(())
    }

    #[test]
    fn if_true() -> Result<()> {
        let code = "(if #t (+ 5 3) (* 2 3))";
        let result = test_eval_expr(code)?;
        assert_eq!(result.unwrap_number(), 8.);
        Ok(())
    }

    #[test]
    fn if_false() -> Result<()> {
        let code = "(if #f (+ 5 3) (* 2 3))";
        let result = test_eval_expr(code)?;
        assert_eq!(result.unwrap_number(), 6.);
        Ok(())
    }

    #[test]
    fn or_left() -> Result<()> {
        let code = "(or #t 2)";
        let result = test_eval_expr(code)?;
        assert!(result.unwrap_bool());
        Ok(())
    }

    #[test]
    fn or_right() -> Result<()> {
        let code = "(or #f 3)";
        let result = test_eval_expr(code)?;
        assert_eq!(result.unwrap_number(), 3.);
        Ok(())
    }

    #[test]
    fn and_left() -> Result<()> {
        let code = "(and #f 4)";
        let result = test_eval_expr(code)?;
        assert!(!result.unwrap_bool());
        Ok(())
    }

    #[test]
    fn and_right() -> Result<()> {
        let code = "(and #t 5)";
        let result = test_eval_expr(code)?;
        assert_eq!(result.unwrap_number(), 5.);
        Ok(())
    }

    #[test]
    fn define() -> Result<()> {
        let code = "(define x 2)";
        let result = test_eval_expr(code)?;
        assert!(matches!(result, Expression::Void));
        Ok(())
    }

    #[test]
    fn define_script() -> Result<()> {
        let code = "
(define x 2)
x";
        let result = test_eval_script(code)?;
        assert_eq!(result.unwrap_number(), 2.);
        Ok(())
    }

    #[test]
    fn set() -> Result<()> {
        let code = "
(define x 1)
(set! x 2)
x
";
        let result = test_eval_script(code)?;
        assert_eq!(result.unwrap_number(), 2.);
        Ok(())
    }

    #[test]
    fn begin() -> Result<()> {
        let code = "(begin (define x (+ 1 4)) x)";
        let result = test_eval_expr(code)?;
        assert_eq!(result.unwrap_number(), 5.);
        Ok(())
    }

    #[test]
    fn begin_empty() -> Result<()> {
        let code = "(begin)";
        let result = test_eval_expr(code)?;
        assert!(result.is_void());
        Ok(())
    }

    #[test]
    fn lambda() -> Result<()> {
        let code = "((lambda x (+ x 1)) 2)";
        let result = test_eval_expr(code)?;
        assert_eq!(result.unwrap_number(), 3.);
        Ok(())
    }

    mod scope {
        use super::*;

        #[test]
        fn lambda() -> Result<()> {
            let code = "
            (define outer
                (lambda (initial)
                    (lambda (addend)
                        (begin
                            (set! initial (+ initial addend))
                            initial))))
            (define counter (outer 5))
            (counter 7)
            ";
            let result = test_eval_script(code)?;
            assert_eq!(result.unwrap_number(), 12.);
            Ok(())
        }

        #[test]
        fn lambda_global() -> Result<()> {
            let code = "
            (define count 2)
            (define counter
                    (lambda (addend)
                        (begin
                            (set! count (+ count addend))
                            count)))
            (counter 7)
            ";
            let result = test_eval_script(code)?;
            assert_eq!(result.unwrap_number(), 9.);
            Ok(())
        }
    }

    #[test]
    fn args_evaled() -> Result<()> {
        let code = "
        (begin
            (define x 1)
            (define y 3)
            (+ x y)
        )";
        let result = test_eval_expr(code)?;
        assert_eq!(result.unwrap_number(), 4.);
        Ok(())
    }
}
