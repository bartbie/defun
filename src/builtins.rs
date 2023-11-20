use super::*;
use expr::Expression;

pub mod math {
    use super::*;

    pub fn add(args: &[Expression]) -> Result<Expression> {
        let sum: Result<i64> = args
            .iter()
            .map(|a| a.clone().try_into())
            .try_fold(0, |acc, e: Result<i64>| Ok(acc + e?));

        Ok(Expression::Integer(sum?))
    }
}
