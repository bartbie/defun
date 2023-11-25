use super::*;
use env::Env;
use variantly::Variantly;

pub type Proc = fn(&[Expression], &mut Env) -> Result<Expression>;

// TODO
// pub struct SList(pub Vec<Expression>);
// pub struct QList(pub Vec<Expression>);

#[derive(Variantly, Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    Symbol(String),
    Integer(i64),
    Bool(bool),
    List(Vec<Expression>),
    Lambda(Proc),
    Void,
}

impl Expression {
    pub fn sym(s: &str) -> Self {
        Self::Symbol(s.to_owned())
    }

    pub fn new_list() -> Self {
        Self::List(vec![])
    }
}

impl From<()> for Expression {
    fn from(_: ()) -> Self {
        Expression::Void
    }
}

impl From<i64> for Expression {
    fn from(value: i64) -> Self {
        Expression::Integer(value)
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
        Expression::Lambda(value)
    }
}

impl TryFrom<Expression> for i64 {
    type Error = Error;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        if let Expression::Integer(i) = value {
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
        if let Expression::Lambda(f) = value {
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
/// let Expression::List(l) = list![Expression::Integer(1), Expression::sym("2")] else {
///    panic!("Couldn't create Expression::List!");
/// };
/// assert_eq!(l[0], Expression::Integer(1));
/// assert_eq!(l[1], Expression::sym("2"));
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
