use crate::{
    expr::{Expression, Proc},
    prelude::*,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Default, Eq, PartialEq)]
pub struct Env {
    vars: HashMap<String, Expression>,
    outer: Option<Rc<RefCell<Self>>>,
}

impl Env {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    fn set_proc(&mut self, name: &str, proc: Proc) -> &mut Self {
        self.set(name, Expression::Proc(proc));
        self
    }

    pub fn new_global() -> Self {
        use crate::builtins::*;
        Self::new().tap_mut(|env| {
            env.set_proc("+", Proc(math::add))
                .set_proc("*", Proc(math::mul))
                .set_proc("-", Proc(math::sub))
                .set_proc("/", Proc(math::div))
                .set_proc("=", Proc(math::eq))
                .set_proc("eval", Proc(core::eval))
                .set_proc("exit", Proc(core::exit));
        })
    }

    pub fn child(parent: Rc<RefCell<Self>>) -> Self {
        Env {
            vars: Default::default(),
            outer: Some(parent),
        }
    }

    pub fn new_global_rc() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self::new_global()))
    }

    pub fn child_rc(parent: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self::child(parent)))
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
