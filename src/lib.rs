use anyhow::{ensure, Error, Result};
use std::path::PathBuf;

pub mod lexer {}

pub mod parser {}

pub mod eval {}

pub mod fs {
    use super::*;
    use anyhow::Context;

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
    let x = script.read()?;
    print!("{:?}", x); // TODO: remove this line later
    todo!()
    // Ok(())
}

pub fn run_repl() -> Result<()> {
    todo!("later, for now running files")
}

pub fn run(script: Option<fs::RealPathBuf>) -> Result<()> {
    match script {
        Some(path) => run_file(path),
        None => run_repl(),
    }
}
