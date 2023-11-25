use super::*;
use expr::{Expression, Proc};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Default)]
pub struct Env {
    vars: HashMap<String, Expression>,
    outer: Option<Rc<RefCell<Self>>>,
}

impl Env {
    pub fn new() -> Self {
        Self::default()
    }

    fn set_proc(&mut self, name: &str, proc: Proc) -> &mut Self {
        self.set(name, Expression::Lambda(proc));
        self
    }

    pub fn new_global() -> Self {
        use builtins::*;
        Self::new().tap_mut(|env| {
            env.set_proc("+", math::add)
                .set_proc("*", math::mul)
                .set_proc("-", math::sub);
        })
    }

    pub fn child(parent: &Rc<RefCell<Self>>) -> Self {
        Env {
            vars: Default::default(),
            outer: Some(Rc::clone(parent)),
        }
    }

    pub fn get(&self, name: &str) -> Option<Expression> {
        match self.vars.get(name) {
            Some(x) => Some(x.clone()),
            None => self.outer.as_ref().and_then(|o| o.borrow().get(name)),
        }
    }

    pub fn set(&mut self, name: &str, val: Expression) -> Option<Expression> {
        self.vars.insert(name.to_owned(), val)
    }
}
