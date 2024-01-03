mod prelude {
    pub use itertools::Itertools;
    pub use tap::prelude::*;
}

pub mod builtins;
pub mod env;
pub mod eval;
pub mod expr;
/// Module representing high-level entry-point of the interpreter.
pub mod interpreter;
pub mod lexer;
pub mod parser;

