use defun::{eval, interpreter};
use rustyline::{history::History, Editor, Helper};

const HISTORY_FILE: &str = ".defunhistory";

pub fn greet() {
    eprintln!("Welcome to Defun REPL.")
}

pub type Error = rustyline::error::ReadlineError;

fn run_line(ip: &mut interpreter::Interpreter, line: &str) -> Option<eval::Signal> {
    match ip.eval(line) {
        Ok(exp) => println!("{}", exp),
        Err(err) => {
            if let interpreter::Error::EvalErr(eval::EvalError::Signal(sig)) = err {
                return Some(sig);
            }
            eprintln!("Error -- {}", err)
        }
    }
    None
}

fn run_loop<H: Helper, I: History>(
    ip: &mut interpreter::Interpreter,
    editor: &mut Editor<H, I>,
) -> Result<eval::Signal, Error> {
    loop {
        if let Some(sig) = run_line(ip, &editor.readline(">> ")?) {
            return Ok(sig);
        }
    }
}

pub fn run() -> Result<eval::Signal, Error> {
    let mut ip = interpreter::Interpreter::default();
    let mut editor = {
        let config = rustyline::Config::builder()
            .auto_add_history(true)
            .completion_type(rustyline::CompletionType::List)
            .build();
        rustyline::Editor::<(), _>::with_config(config)?
    };
    _ = editor.load_history(HISTORY_FILE);
    let res = run_loop(&mut ip, &mut editor);
    _ = editor.save_history(HISTORY_FILE);
    res
}
