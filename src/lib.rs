mod prelude {
    pub use anyhow::{ensure, Error, Result};
    pub use itertools::Itertools;
    pub use tap::prelude::*;
}

pub mod builtins;
pub mod env;
pub mod eval;
pub mod expr;
pub mod lexer;
pub mod parser;

use crate::prelude::*;
use std::path::{Path, PathBuf};
use std::{fs, io};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RunError {
    #[error(transparent)]
    ParseErr(#[from] parser::ParseError),
    #[error(transparent)]
    EvalErr(#[from] eval::EvalError),
    #[error(transparent)]
    IOErr(#[from] io::Error),
}

pub fn eval(code: &str) -> Result<expr::Expression, RunError> {
    let ast = parser::parse_script(code)?;
    let script_env = env::Env::new_global_rc();
    Ok(eval::eval_script(&ast, script_env)?)
}

pub fn run_file(path: &Path) -> Result<expr::Expression, RunError> {
    let code = fs::read_to_string(path)?;
    eval(&code)
}

pub fn run_stdin() -> Result<expr::Expression, RunError> {
    let code = io::read_to_string(io::stdin())?;
    eval(&code)
}

pub fn run_repl() -> Result<expr::Expression, RunError> {
    todo!("later, for now running files")
}

#[derive(Debug)]
pub enum RunMode {
    Script(PathBuf),
    Repl,
    Stdin,
}

#[derive(Debug)]
pub struct RunOpts {
    pub mode: RunMode,
}

pub fn run(opts: RunOpts) -> Result<expr::Expression, RunError> {
    match opts.mode {
        RunMode::Script(path) => run_file(&path),
        RunMode::Repl => run_repl(),
        RunMode::Stdin => run_stdin(),
    }
}
