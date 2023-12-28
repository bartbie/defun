use clap::Parser;
use defun::{run, Mode, Opts};
use std::path::PathBuf;

// TODO: create proper subcommands structure here
#[derive(Parser, Debug)]
struct Args {
    /// script file to run
    file: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    dbg!(&args);
    run(Opts {
        mode: Mode::from(args.file),
    });
}
