mod prelude {
    pub use itertools::Itertools;
    pub use tap::prelude::*;
}

pub mod builtins;
pub mod env;
pub mod eval;
pub mod expr;
pub mod lexer;
pub mod parser;
/// Module representing high-level entry-point of the REPL.
pub mod repl;

/// Module representing high-level entry-point of the interpreter.
pub mod interpreter {
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

    pub fn eval(code: &str) -> Result {
        eval_with_env(code, env::Env::new_global_rc())
    }

    pub fn eval_with_env(code: &str, env: Rc<RefCell<env::Env>>) -> Result {
        let ast = parser::parse_script(code)?;
        Ok(eval::eval_script(&ast, env)?)
    }

    pub fn run(mut source: impl io::Read) -> Result {
        let code = {
            let mut s = String::new();
            source.read_to_string(&mut s)?;
            s
        };
        eval(&code)
    }
}

/// Module that holds implementation detail of [`crate::run`].
/// Called glue because it glues [`crate::interpreter`] and [`crate::repl`] together.
/// Plus it sounds cool.
mod glue {
    use crate::{expr, interpreter, repl, Mode, Opts};
    use std::{fs::File, io};
    use thiserror::Error;

    #[derive(Error, Debug)]
    pub enum Error {
        #[error(transparent)]
        Run(#[from] interpreter::Error),
        #[error(transparent)]
        Repl(#[from] rustyline::error::ReadlineError),
    }
    impl From<repl::Error> for Error {
        fn from(value: repl::Error) -> Self {
            match value {
                repl::Error::Readline(r) => r.into(),
                repl::Error::ExitSignal(x) => interpreter::Error::from(x).into(),
            }
        }
    }

    pub type Result<T> = std::result::Result<T, Error>;

    fn open_file(path: &std::path::Path) -> std::result::Result<File, interpreter::Error> {
        File::open(path).map_err(interpreter::Error::from)
    }

    pub fn run(opts: Opts) -> Result<Option<expr::Expression>> {
        Ok(match opts.mode {
            Mode::Script(path) => interpreter::run(open_file(&path)?).map(Some)?,
            Mode::Stdin => interpreter::run(io::stdin()).map(Some)?,
            Mode::Repl => {
                repl::greet();
                repl::run().map(|_| None)?
            }
        })
    }
}

use std::{path::PathBuf, process::exit};

#[derive(Debug)]
pub enum Mode {
    Script(PathBuf),
    Repl,
    Stdin,
}

impl From<Option<PathBuf>> for Mode {
    fn from(value: Option<PathBuf>) -> Self {
        if let Some(path) = value {
            Mode::Script(path)
        // TODO: fix Stdin mode
        // } else if atty::is(atty::Stream::Stdin) {
        //     RunMode::Stdin
        } else {
            Mode::Repl
        }
    }
}

#[derive(Debug)]
pub struct Opts {
    pub mode: Mode,
}

pub fn run(opts: Opts) {
    match glue::run(opts) {
        Ok(o) => {
            if let Some(exp) = o {
                println!("{}", exp)
            }
        }
        Err(err) => {
            eprintln!("{}", err);
            if let glue::Error::Run(interpreter::Error::EvalErr(eval::EvalError::ExitSignal(
                code,
            ))) = err
            {
                exit(code as i32)
            }
        }
    }
}
