use super::*;
use env::Env;
use ordered_float::NotNan;
use std::{cell::RefCell, rc::Rc};
use variantly::Variantly;

pub type Proc = fn(&[Expression], Rc<RefCell<Env>>) -> Result<Expression>;

// TODO
// pub struct SList(pub Vec<Expression>);
// pub struct QList(pub Vec<Expression>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Lambda {
    pub args: Vec<String>,
    pub body: Rc<Expression>,
    pub env: Rc<RefCell<Env>>,
}

impl Lambda {
    pub fn run(&self, args: &[Expression], outer_env: Rc<RefCell<Env>>) -> Result<Expression> {
        let args: Vec<_> = args
            .iter()
            .map(move |a| eval::eval(a, outer_env.clone()))
            .collect::<Result<_>>()?;
        {
            let passed = args.len();
            let required = self.args.len();
            ensure!(
                passed == required,
                "This procedure requires {} arguments, {} passed!",
                required,
                passed,
            )
        };
        let mut eval_env = Env::child(self.env.clone());
        for (name, val) in self.args.iter().zip(args) {
            eval_env.set(name, val)?;
        }
        let eval_env = Rc::new(RefCell::new(eval_env));
        eval::eval(&self.body, eval_env)
    }
}

#[derive(Variantly, Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    Symbol(String),
    Number(NotNan<f64>),
    Bool(bool),
    List(Vec<Expression>),
    Proc(Proc),
    Lambda(Lambda),
    Void,
}

impl Expression {
    pub fn sym(s: &str) -> Self {
        Self::Symbol(s.to_owned())
    }

    pub fn new_list() -> Self {
        Self::List(vec![])
    }

    pub fn num(f: f64) -> Result<Self> {
        Ok(Self::Number(NotNan::new(f)?))
    }
}

impl From<()> for Expression {
    fn from(_: ()) -> Self {
        Expression::Void
    }
}

impl From<NotNan<f64>> for Expression {
    fn from(value: NotNan<f64>) -> Self {
        Expression::Number(value)
    }
}

impl From<bool> for Expression {
    fn from(value: bool) -> Self {
        Expression::Bool(value)
    }
}

impl From<Vec<Expression>> for Expression {
    fn from(value: Vec<Expression>) -> Self {
        Expression::List(value)
    }
}

impl From<Proc> for Expression {
    fn from(value: Proc) -> Self {
        Expression::Proc(value)
    }
}

impl TryFrom<Expression> for NotNan<f64> {
    type Error = Error;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        if let Expression::Number(i) = value {
            Ok(i)
        } else {
            bail!("Not a number!")
        }
    }
}

impl TryFrom<Expression> for () {
    type Error = ();

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        if let Expression::Void = value {
            Ok(())
        } else {
            Err(())
        }
    }
}

impl TryFrom<Expression> for Vec<Expression> {
    type Error = Error;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        if let Expression::List(l) = value {
            Ok(l)
        } else {
            bail!("Not a list!")
        }
    }
}

impl TryFrom<Expression> for Proc {
    type Error = Error;
    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        if let Expression::Proc(f) = value {
            Ok(f)
        } else {
            bail!("Not a procedure!")
        }
    }
}

impl TryFrom<Expression> for bool {
    type Error = Error;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        if let Expression::Bool(b) = value {
            Ok(b)
        } else {
            bail!("Not a procedure!")
        }
    }
}

/// Creates a [`Expression::List`] like `vec!`.
///
/// A thin wrapper around `vec!`, expands to `Expression::List(vec![/*...*/])`.
/// - Create a [`Expression::List`] containing a given list of elements:
///
/// ```
/// # use defun::expr::{list, Expression};
/// # use anyhow::Result;
/// # fn main() -> Result<()> {
/// let l = list![Expression::num(1.)?, Expression::sym("2")].unwrap_list();
/// assert_eq!(l[0], Expression::num(1.)?);
/// assert_eq!(l[1], Expression::sym("2"));
/// # Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! list {
    [] => (
        Expression::new_list()
    );
    [$elem:expr; $n:expr] => (
        Expression::List(vec![$elem; $n])
    );
    [$($x:expr),+ $(,)?] => (
        Expression::List(vec![$($x),+])
    );
}

pub use list;
