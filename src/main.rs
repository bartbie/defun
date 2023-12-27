use clap::Parser;
use defun::{run, RunMode, RunOpts};
use std::path::PathBuf;

// TODO: create proper subcommands structure here
#[derive(Parser, Debug)]
struct Args {
    /// script file to run
    file: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    let mode = if let Some(path) = args.file {
        RunMode::Script(path)
    } else if atty::is(atty::Stream::Stdin) {
        RunMode::Stdin
    } else {
        RunMode::Repl
    };
    match run(RunOpts { mode }) {
        Ok(last) => println!("{}", last),
        Err(err) => eprintln!("{}", err),
    }
}
