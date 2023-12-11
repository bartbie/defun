use anyhow::Result;
use clap::Parser;
use defun::{run, RunMode, RunOpts};
use std::path::PathBuf;

// TODO: create proper subcommands structure here
#[derive(Parser, Debug)]
struct Args {
    /// script file to run
    file: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();
    dbg!(&args);
    // TODO: change this to clap's App::error
    run(RunOpts {
        mode: RunMode::Script(args.file.try_into()?),
    })
}
