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
