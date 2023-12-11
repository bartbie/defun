use crate::{env::Env, eval, eval::EvalError, prelude::*};

use ordered_float::NotNan;
use std::{cell::RefCell, rc::Rc};
use thiserror::Error;
use variantly::Variantly;

#[derive(Error, Debug)]
pub enum ExprError {
    #[error("Not a List!")]
    NotAList,
    #[error("Not a Procedure!")]
    NotAProc,
    #[error("Not a Number!")]
    NotANum,
    #[error("Not Void!")]
    NotVoid,
    #[error("Not a Bool!")]
    NotABool,
    #[error("Not a Symbol!")]
    NotASym,
}

pub trait Call {
    fn call(&self, args: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression, EvalError>;
}

type ProcFn = fn(&[Expression], Rc<RefCell<Env>>) -> Result<Expression, EvalError>;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Proc(pub ProcFn);

impl Call for Proc {
    fn call(&self, args: &[Expression], env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        self.0(args, env)
    }
}

impl From<ProcFn> for Proc {
    fn from(value: fn(&[Expression], Rc<RefCell<Env>>) -> Result<Expression, EvalError>) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Lambda {
    pub args: Vec<String>,
    pub body: Rc<Expression>,
    pub env: Rc<RefCell<Env>>,
}

impl Call for Lambda {
    fn call(&self, args: &[Expression], _env: Rc<RefCell<Env>>) -> Result<Expression, EvalError> {
        {
            let passed = args.len();
            let required = self.args.len();
            if passed != required {
                return Err(EvalError::WrongArgCount { required, passed });
            }
        };
        let mut eval_env = Env::child(self.env.clone());
        for (name, val) in self.args.iter().zip(args) {
            eval_env.set(name, val.clone());
        }
        let eval_env = Rc::new(RefCell::new(eval_env));
        eval::eval(&self.body, eval_env)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SList(pub Vec<Expression>);

impl From<Vec<Expression>> for SList {
    fn from(value: Vec<Expression>) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct QList(pub Vec<Expression>);

impl From<Vec<Expression>> for QList {
    fn from(value: Vec<Expression>) -> Self {
        Self(value)
    }
}

#[derive(Variantly, Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    Symbol(String),
    Number(NotNan<f64>),
    Bool(bool),
    SList(SList),
    QList(QList),
    Proc(Proc),
    Lambda(Lambda),
    Void,
}

impl Expression {
    pub fn sym(s: &str) -> Self {
        Self::Symbol(s.to_owned())
    }

    pub fn new_s_list() -> Self {
        Self::SList(vec![].into())
    }

    pub fn num(f: f64) -> Result<Self> {
        Ok(Self::Number(NotNan::new(f)?))
    }
}

// copy types

macro_rules! impl_from_copy {
    ($type:ty, $body:expr, $unwrap_or:ident, $ref_unwrap_or:ident, $err:expr) => {
        impl From<$type> for Expression {
            fn from(value: $type) -> Self {
                $body(value)
            }
        }

        impl From<&$type> for Expression {
            fn from(value: &$type) -> Self {
                $body(*value)
            }
        }

        impl TryFrom<Expression> for $type {
            type Error = ExprError;

            fn try_from(value: Expression) -> Result<Self, Self::Error> {
                value.$unwrap_or($err)
            }
        }

        impl TryFrom<&Expression> for $type {
            type Error = ExprError;

            fn try_from(value: &Expression) -> Result<Self, Self::Error> {
                value.$ref_unwrap_or($err).map(|x| *x)
            }
        }
    };
}

impl From<()> for Expression {
    fn from(_: ()) -> Self {
        Expression::Void
    }
}

impl TryFrom<Expression> for () {
    type Error = ExprError;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        if let Expression::Void = value {
            Ok(())
        } else {
            Err(ExprError::NotVoid)
        }
    }
}

impl_from_copy!(
    NotNan<f64>,
    Expression::Number,
    number_or,
    number_ref_or,
    ExprError::NotANum
);

impl_from_copy!(
    Proc,
    Expression::Proc,
    proc_or,
    proc_ref_or,
    ExprError::NotAProc
);
impl_from_copy!(
    bool,
    Expression::Bool,
    bool_or,
    bool_ref_or,
    ExprError::NotABool
);

// non-copy types
macro_rules! impl_from {
    ($type:ty, $body:expr, $unwrap_or:ident, $err:expr) => {
        impl From<$type> for Expression {
            fn from(value: $type) -> Self {
                $body(value)
            }
        }

        impl TryFrom<Expression> for $type {
            type Error = ExprError;

            fn try_from(value: Expression) -> Result<Self, Self::Error> {
                value.$unwrap_or($err)
            }
        }
    };
}

impl_from!(SList, Expression::SList, s_list_or, ExprError::NotAList);

impl_from!(Lambda, Expression::Lambda, lambda_or, ExprError::NotAProc);
impl_from!(String, Expression::Symbol, symbol_or, ExprError::NotASym);

/// Creates a [`Expression::List`] like `vec!`.
///
/// A thin wrapper around `vec!`, expands to `Expression::List(vec![/*...*/])`.
/// - Create a [`Expression::List`] containing a given list of elements:
///
/// ```
/// # use defun::expr::{s_list, Expression};
/// # use anyhow::Result;
/// # fn main() -> Result<()> {
/// let l = s_list![Expression::num(1.)?, Expression::sym("2")].unwrap_s_list();
/// assert_eq!(l.0[0], Expression::num(1.)?);
/// assert_eq!(l.0[1], Expression::sym("2"));
/// # Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! s_list {
    [] => (
        Expression::new_s_list()
    );
    [$elem:expr; $n:expr] => (
        Expression::SList(vec![$elem; $n].into())
    );
    [$($x:expr),+ $(,)?] => (
        Expression::SList(vec![$($x),+].into())
    );
}

pub use s_list;
