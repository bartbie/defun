const HISTORY_FILE: &str = ".defunhistory";

use thiserror::Error;

use crate::{env, eval, interpreter};

pub fn greet() {
    eprintln!("Welcome to Defun REPL.")
}

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Readline(#[from] rustyline::error::ReadlineError),

    /// NOTE:
    /// an ugly hack around the fact that ExitSignal is a just a variant of [`eval::EvalError`]
    #[error(transparent)]
    ExitSignal(#[from] eval::EvalError),
}

pub fn run() -> Result<(), Error> {
    let env = env::Env::new_global_rc();
    let mut editor = {
        let config = rustyline::Config::builder()
            .auto_add_history(true)
            .completion_type(rustyline::CompletionType::List)
            .build();
        rustyline::Editor::<(), _>::with_config(config)?
    };
    // let mut editor = rustyline::DefaultEditor::new()?;
    _ = editor.load_history(HISTORY_FILE);

    let res = loop {
        let readline = editor.readline(">> ");
        match readline {
            Ok(line) => match interpreter::eval_with_env(&line, env.clone()) {
                Ok(exp) => println!("{}", exp),
                Err(err) => {
                    if let interpreter::Error::EvalErr(x @ eval::EvalError::ExitSignal(_)) = err {
                        break x.into();
                    }
                    eprintln!("Error -- {}", err)
                }
            },
            Err(err) => break err.into(),
        }
    };
    _ = editor.save_history(HISTORY_FILE);
    Err(res)
}
