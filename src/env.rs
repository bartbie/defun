use super::*;
use expr::Expression;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct Env {
    vars: HashMap<String, Expression>,
}

impl Env {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_global() -> Self {
        use builtins::*;
        let mut env = Self::new();
        env.set("+", Expression::Lambda(math::add));
        env
    }

    pub fn get(&self, name: &str) -> Option<Expression> {
        self.vars.get(name).cloned()
    }

    pub fn set(&mut self, name: &str, val: Expression) -> Option<Expression> {
        self.vars.insert(name.to_owned(), val)
    }
}
