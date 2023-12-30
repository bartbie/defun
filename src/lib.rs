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
}

/// Module that holds implementation detail of [`crate::run`].
/// Called glue because it glues [`crate::interpreter`] and [`crate::repl`] together.
/// Plus it sounds cool.
mod glue {
    use crate::{eval, expr, interpreter, repl, Mode, Opts};
    use std::{fs::File, io};
    use thiserror::Error;

    #[derive(Error, Debug)]
    pub enum Error {
        #[error(transparent)]
        Run(interpreter::Error),
        #[error(transparent)]
        Repl(#[from] rustyline::error::ReadlineError),
        #[error(transparent)]
        Signal(#[from] eval::Signal),
    }

    impl From<interpreter::Error> for Error {
        fn from(value: interpreter::Error) -> Self {
            if let interpreter::Error::EvalErr(eval::EvalError::Signal(sig)) = value {
                Self::Signal(sig)
            } else {
                Self::Run(value)
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
                let sig = repl::run()?;
                return Err(sig.into());
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
            if let glue::Error::Signal(sig) = err {
                match sig {
                    eval::Signal::ExitSignal(code) => exit(code as i32),
                }
            }
        }
    }
}
