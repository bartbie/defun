/// Module representing high-level entry-point of the REPL.
pub mod repl;

pub mod run {

    /// Module that holds implementation detail of [`crate::run::run`].
    /// It glues [`defun::interpreter`] and [`crate::repl`] together.
    mod glue {
        use super::{Mode, Opts};
        use crate::repl;
        use defun::{eval, expr, interpreter};
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

    impl Mode {
        pub fn stdin_or_repl() -> Self {
            //TODO
            Self::Repl
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
                        defun::eval::Signal::ExitSignal(code) => exit(code as i32),
                    }
                }
            }
        }
    }
}

pub mod parse {
    use defun::{expr::Expression, parser};
    use itertools::Itertools;
    use std::{fs, io, path::PathBuf};
    use thiserror::Error;

    #[derive(Error, Debug)]
    enum Error {
        #[error(transparent)]
        ParseErr(#[from] parser::ParseError),
        #[error(transparent)]
        IOErr(#[from] io::Error),
    }

    fn inner(file: PathBuf) -> Result<Vec<Expression>, Error> {
        Ok(parser::parse_script(&fs::read_to_string(file)?)?)
    }

    pub fn run(file: PathBuf) {
        match inner(file) {
            Ok(x) => println!("{}", x.iter().join("\n")),
            Err(e) => eprintln!("Error -- {}", e),
        }
    }
}

pub mod lex {
    use defun::lexer;
    use itertools::Itertools;
    use std::{fs, io, path::PathBuf};

    fn inner(file: PathBuf) -> Result<Vec<lexer::Token>, io::Error> {
        Ok(lexer::tokenize(&fs::read_to_string(file)?))
    }

    fn to_str(tok: &lexer::Token) -> String {
        format!("{:?}", tok).replace("Token::", "")
    }

    pub fn run(file: PathBuf) {
        match inner(file) {
            Ok(x) => println!("{}", x.iter().map(to_str).join("\n")),

            Err(e) => eprintln!("Error -- {}", e),
        }
    }
}

use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser, Debug)]
/// A non-compliant Scheme interpreter written in Rust.
pub struct Args {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Run program from a script file.
    Run {
        /// Program to run.
        file: PathBuf,
    },
    /// Parse program from a script file.
    Parse {
        /// Program to parse.
        file: PathBuf,
    },

    /// Lex (tokenize) program from a script file.
    Lex {
        /// Program to lex.
        file: PathBuf,
    },
}

pub fn run() {
    let args = Args::parse();
    let Some(command) = args.command else {
        return run::run(run::Opts {
            mode: run::Mode::stdin_or_repl(),
        });
    };
    match command {
        Commands::Run { file } => {
            run::run(run::Opts {
                mode: run::Mode::Script(file),
            });
        }
        Commands::Parse { file } => parse::run(file),
        Commands::Lex { file } => lex::run(file),
    }
}
