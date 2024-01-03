use criterion::{black_box, criterion_group, criterion_main, Criterion};
use pprof::criterion::{Output, PProfProfiler};
use rand::{seq::SliceRandom, thread_rng, Rng};

use ordered_float::OrderedFloat;
use std::ops;
use tap::Tap;
use variantly::Variantly;

// aliases for quickly changing the implementation
pub type Int = i32;
pub type Float = OrderedFloat<f64>;

#[derive(Variantly, Debug, Copy, Clone, Eq)]
pub enum Number {
    Integer(Int),
    Float(Float),
}

impl Number {
    pub fn from(num: impl Into<Self>) -> Self {
        num.into()
    }

    #[inline]
    fn upcast(self) -> Float {
        match self {
            Self::Integer(i) => OrderedFloat(i as f64),
            Self::Float(f) => f,
        }
    }

    #[inline]
    fn op(self, rhs: Self, op_i: fn(Int, Int) -> Int, op_f: fn(Float, Float) -> Float) -> Self {
        if let Self::Integer(i) = rhs {
            if let Self::Integer(i2) = self {
                return Self::Integer(op_i(i, i2));
            }
        }
        Self::Float(op_f(self.upcast(), rhs.upcast()))
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        if let Self::Integer(i) = other {
            if let Self::Integer(i2) = self {
                return i == i2;
            }
        }
        self.upcast() == other.upcast()
    }
}

impl Ord for Number {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if let Self::Integer(i) = other {
            if let Self::Integer(i2) = self {
                return i.cmp(i2);
            }
        }
        self.upcast().cmp(&other.upcast())
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl ops::Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        self.op(rhs, ops::Add::add, ops::Add::add)
    }
}

impl ops::Sub for Number {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self.op(rhs, ops::Sub::sub, ops::Sub::sub)
    }
}

impl ops::Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        self.op(rhs, ops::Mul::mul, ops::Mul::mul)
    }
}

impl ops::Div for Number {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::Float(self.upcast() / rhs.upcast())
    }
}

impl From<Int> for Number {
    fn from(value: Int) -> Self {
        Number::Integer(value)
    }
}

impl From<&Int> for Number {
    fn from(value: &Int) -> Self {
        Number::Integer(*value)
    }
}

impl From<Float> for Number {
    fn from(value: Float) -> Self {
        Number::Float(value)
    }
}

impl From<&Float> for Number {
    fn from(value: &Float) -> Self {
        Number::Float(*value)
    }
}

// generate rand

fn generate_rand_floats(size: usize) -> Vec<f64> {
    thread_rng()
        .sample_iter::<f64, _>(rand::distributions::Standard)
        .take(size)
        .collect()
}

// bench

fn f64_calc(numbers: &[f64]) -> f64 {
    let mut res = 0_f64;
    for n in numbers {
        res += *n;
    }
    res
}

fn i32_calc(numbers: &[i32]) -> i32 {
    let mut res = 0;
    for n in numbers {
        res += *n;
    }
    res
}

fn number_calc(numbers: &[Number]) -> Number {
    let mut res = Number::Integer(0);
    for n in numbers {
        res = res + *n;
    }
    res
}

pub fn criterion_benchmark(c: &mut Criterion) {
    const N: usize = 10000;
    use Number::*;
    let floats = generate_rand_floats(N);
    let ints: Vec<i32> = floats.iter().map(|f| f.trunc() as i32).collect();
    let number_floats: Vec<Number> = floats.iter().map(|f| Float((*f).into())).collect();
    let number_ints: Vec<Number> = ints.iter().map(Number::from).collect();
    let number_mixed = {
        let mut ints = number_ints.clone().split_off(N / 2);
        let floats = number_floats
            .clone()
            .tap_mut(|v| v.reverse())
            .split_off(N / 2);
        ints.extend(floats);
        // shuffle them just in case
        ints.shuffle(&mut thread_rng());
        ints
    };

    c.bench_function("floats", |b| b.iter(|| f64_calc(black_box(&floats))));
    c.bench_function("ints", |b| b.iter(|| (i32_calc(&ints))));
    c.bench_function("enum_floats", |b| b.iter(|| (number_calc(&number_floats))));
    c.bench_function("enum_ints", |b| b.iter(|| (number_calc(&number_ints))));
    c.bench_function("enum_mixed", |b| b.iter(|| (number_calc(&number_mixed))));
}

criterion_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets = criterion_benchmark
}

criterion_main!(benches);
