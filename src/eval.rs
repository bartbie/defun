use super::*;
use env::Env;
use expr::Expression;

fn get_sym(sym: &str, env: &mut Env) -> Result<Expression> {
    env.get(sym).ok_or(anyhow!("Unbounded symbol: {}!", sym))
}

/// special forms that require different evaluation than normal procedures
pub mod special {
    use std::str::FromStr;

    use super::*;

    pub enum SpecialForm {
        If,
        Define,
        Set,
        And,
        Or,
    }

    impl FromStr for SpecialForm {
        type Err = (); // no need for more here for now

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(match s {
                "if" => Self::If,
                "define" => Self::Define,
                "set" => Self::Set,
                "and" => Self::And,
                "or" => Self::Or,
                _ => return Err(()),
            })
        }
    }

    pub fn eval_special(
        form: SpecialForm,
        rest: &[Expression],
        env: &mut Env,
    ) -> Result<Expression> {
        match form {
            SpecialForm::If => eval_if(rest, env),
            SpecialForm::Define => eval_define(rest, env),
            SpecialForm::Set => eval_set(rest, env),
            SpecialForm::And => eval_and(rest, env),
            SpecialForm::Or => eval_or(rest, env),
        }
    }

    pub fn eval_define(rest: &[Expression], env: &mut Env) -> Result<Expression> {
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

    pub fn eval_if(rest: &[Expression], env: &mut Env) -> Result<Expression> {
        let [cond, left, right] = rest else {
            bail!("Ill-formed expression!")
        };
        let (_, test) = eval_cond(cond, env)?;

        eval(if test { left } else { right }, env)
    }

    /// In Scheme ifs just check if condition is equal to `false`,
    /// no actual type coercion is happening.
    fn eval_cond(exp: &Expression, env: &mut Env) -> Result<(Expression, bool)> {
        let exp = eval(exp, env)?;

        let b = match exp {
            Expression::Bool(b) => b,
            _ => true,
        };

        Ok((exp, b))
    }

    pub fn eval_and(rest: &[Expression], env: &mut Env) -> Result<Expression> {
        let [left, right] = rest else {
            bail!("Ill-formed expression!")
        };

        let left = eval_cond(left, env)?;
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

    pub fn eval_or(rest: &[Expression], env: &mut Env) -> Result<Expression> {
        let [left, right] = rest else {
            bail!("Ill-formed expression!")
        };

        let (eval_left, test) = eval_cond(left, env)?;
        if test {
            Ok(eval_left)
        } else {
            let (eval_right, _) = eval_cond(right, env)?;
            Ok(eval_right)
        }
    }

    pub fn eval_set(_rest: &[Expression], _env: &mut Env) -> Result<Expression> {
        todo!()
    }
}

fn eval_list(list: &[Expression], env: &mut Env) -> Result<Expression> {
    let [head, args @ ..] = list else {
        bail!("Ill-formed expression!")
    };

    // handle special procedures
    if let Expression::Symbol(sym) = head {
        if let Ok(form) = sym.parse() {
            return special::eval_special(form, args, env);
        }
    }

    // loop via recursion
    let op = match head {
        Expression::Symbol(sym) => get_sym(sym, env)?,
        Expression::List(l) => eval_list(l, env)?,
        _ => bail!("Operator is not a procedure!"),
    };

    let Expression::Lambda(f) = op else {
        bail!("Operator is not a procedure!")
    };

    f(args, env)
}

pub fn eval(exp: &Expression, env: &mut Env) -> Result<Expression> {
    dbg!(&exp);
    Ok(match exp {
        Expression::Integer(i) => Expression::Integer(*i),
        Expression::Bool(b) => Expression::Bool(*b),
        Expression::Symbol(sym) => get_sym(sym, env)?,
        Expression::List(l) => eval_list(l, env)?,
        Expression::Lambda(fun) => Expression::Lambda(*fun),
        Expression::Void => Expression::Void,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_eval_expr(code: &str) -> Result<Expression> {
        let tokens = parser::parse_single_expr(code)?;
        eval(&tokens, &mut Env::new_global())
    }

    #[test]
    fn number() -> Result<()> {
        let number = 25;
        let result = test_eval_expr(&25.to_string())?;
        assert_eq!(result.unwrap_integer(), number);
        Ok(())
    }

    #[test]
    fn symbol() -> Result<()> {
        let symbol = "+";
        let result = test_eval_expr(symbol)?;
        assert_eq!(
            // INFO: while this is not normally guaranteed to be equal due to LLVM shenanigans,
            // we put that pointer address in the expression at runtime so they should always be the same.
            result.unwrap_lambda() as usize,
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
        assert_eq!(result.unwrap_integer(), 8);
        Ok(())
    }

    #[test]
    fn if_false() -> Result<()> {
        let code = "(if #f (+ 5 3) (* 2 3))";
        let result = test_eval_expr(code)?;
        assert_eq!(result.unwrap_integer(), 6);
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
        assert_eq!(result.unwrap_integer(), 3);
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
        assert_eq!(result.unwrap_integer(), 5);
        Ok(())
    }
}
