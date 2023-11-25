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
        let test = eval_cond(cond, env)?;
        eval(if test { left } else { right }, env)
    }

    /// In Scheme ifs just check if condition is equal to `false`,
    /// no actual type coercion is happening.
    fn eval_cond(elem: &Expression, env: &mut Env) -> Result<bool> {
        // PERF: cloning list can be costly,
        // convert to separate Option maybe
        let mut elem = elem.clone();
        while elem.is_list() {
            elem = eval(&elem, env)?;
        }

        match &elem {
            Expression::Symbol(sym) => eval_cond(&get_sym(sym, env)?, env),
            Expression::Bool(b) => Ok(*b),
            _ => Ok(true),
        }
    }

    pub fn eval_set(_rest: &[Expression], _env: &mut Env) -> Result<Expression> {
        todo!()
    }

    pub fn eval_and(_rest: &[Expression], _env: &mut Env) -> Result<Expression> {
        todo!()
    }

    pub fn eval_or(_rest: &[Expression], _env: &mut Env) -> Result<Expression> {
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

    #[test]
    fn addition() -> Result<()> {
        let code = "(+ 2 3)";
        let expected = 5;
        let result = {
            let tokens = parser::parse_single_expr(code)?;
            eval(&tokens, &mut Env::new_global())?
        };
        assert_eq!(result.unwrap_integer(), expected);
        Ok(())
    }

    #[test]
    fn multiplication() -> Result<()> {
        let code = "(* 3 5)";
        let expected = 15;
        let result = {
            let tokens = parser::parse_single_expr(code)?;
            eval(&tokens, &mut Env::new_global())?
        };
        assert_eq!(result.unwrap_integer(), expected);
        Ok(())
    }

    #[test]
    fn multiplication_nested() -> Result<()> {
        let code = "(* (+ 1 2) (+ 5 3))";
        let expected = 24;
        let result = {
            let tokens = parser::parse_single_expr(code)?;
            eval(&tokens, &mut Env::new_global())?
        };
        assert_eq!(result.unwrap_integer(), expected);
        Ok(())
    }
}
