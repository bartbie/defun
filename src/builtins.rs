use super::*;
use env::Env;
use eval::eval;
use expr::Expression;

pub mod math {
    use super::*;

    use std::ops::*;

    /// Creates an iterator that returns [`Result<i64>`]
    fn eval_num<'a>(
        args: &'a [Expression],
        env: &'a mut Env,
    ) -> impl Iterator<Item = Result<i64>> + 'a {
        args.iter().map(|a| eval(a, env)?.try_into())
    }

    macro_rules! math_fn {
        ($fn_name:ident, $start:literal, $closure:expr) => {
            pub fn $fn_name(args: &[Expression], env: &mut Env) -> Result<Expression> {
                eval_num(args, env)
                    .fold_ok($start, $closure)
                    .map(Expression::Integer)
            }
        };
    }

    math_fn! {add, 0, Add::add}
    math_fn! {mul, 1, Mul::mul}
    math_fn! {sub, 0, Sub::sub}

    pub fn div(_args: &[Expression], _env: &mut Env) -> Result<Expression> {
        todo!("Division will be implemented when refactored to numbers!")
    }
}
