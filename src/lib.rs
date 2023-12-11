use anyhow::{ensure, Error, Result};
use itertools::Itertools;
use tap::prelude::*;

pub mod builtins;
pub mod env;
pub mod eval;
pub mod expr;
pub mod lexer;
pub mod parser;

pub mod fs {
    use super::*;
    use anyhow::Context;
    use std::path::PathBuf;

    #[derive(Debug)]
    pub struct RealPathBuf(pub PathBuf);

    impl RealPathBuf {
        /// read the file
        pub fn read(&self) -> Result<String> {
            std::fs::read_to_string(&self.0).with_context(|| {
                format!(
                    "Failed to read file: {}",
                    self.0
                        .to_str()
                        .expect("Unexpected UTF-8 error while opening a file!")
                )
            })
        }
    }

    impl TryFrom<PathBuf> for RealPathBuf {
        type Error = Error;

        fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
            ensure!(&value.is_file(), "File provided doesn't exist!");
            Ok(Self(value))
        }
    }
}

pub fn run_file(script: fs::RealPathBuf) -> Result<()> {
    let source_code = script.read()?;
    dbg!(&source_code);
    let tokens = lexer::tokenize(&source_code);
    dbg!(&tokens);
    let parsed_file = parser::parse_script(&source_code)?;
    dbg!(&parsed_file);
    Ok(())
}

pub fn run_repl() -> Result<()> {
    todo!("later, for now running files")
}

#[derive(Debug)]
pub enum RunMode {
    Script(fs::RealPathBuf),
    REPL,
}

#[derive(Debug)]
pub struct RunOpts {
    pub mode: RunMode,
}

pub fn run(opts: RunOpts) -> Result<()> {
    match opts.mode {
        RunMode::Script(path) => run_file(path),
        RunMode::REPL => run_repl(),
    }
}
