use crate::{env, eval, expr, parser};
use std::io;
use std::{cell::RefCell, rc::Rc};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    ParseErr(#[from] parser::ParseError),
    #[error(transparent)]
    EvalErr(#[from] eval::EvalError),
    #[error(transparent)]
    IOErr(#[from] io::Error),
}

pub type Result<T = expr::Expression> = std::result::Result<T, Error>;

pub fn eval_with_env(code: &str, env: Rc<RefCell<env::Env>>) -> Result {
    let ast = parser::parse_script(code)?;
    Ok(eval::eval_script(&ast, env)?)
}

#[derive(Debug)]
pub struct Interpreter {
    env: Rc<RefCell<env::Env>>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            env: env::Env::new_global_rc(),
        }
    }
}

impl Interpreter {
    pub fn eval(&mut self, code: &str) -> Result {
        eval_with_env(code, self.env.clone())
    }

    pub fn run(&mut self, mut source: impl io::Read) -> Result {
        let code = {
            let mut s = String::new();
            source.read_to_string(&mut s)?;
            s
        };
        self.eval(&code)
    }
}

pub fn eval(code: &str) -> Result {
    Interpreter::default().eval(code)
}

pub fn run(source: impl io::Read) -> Result {
    Interpreter::default().run(source)
}
