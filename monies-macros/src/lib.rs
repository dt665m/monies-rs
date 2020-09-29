#![allow(unused_imports)]

use std::convert::{From, Into};
use std::fmt;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};

use lazy_static::lazy_static;
use paste::paste;
use rust_decimal::prelude::*;
use rust_decimal_macros::*;

pub trait Currency {
    fn decimal_value(&self) -> &Decimal;

    fn is_zero(&self) -> bool;

    fn is_negative(&self) -> bool;

    fn is_positive(&self) -> bool;

    fn code(&self) -> &'static str;

    fn number(&self) -> &'static str;

    fn exponent(&self) -> u32;
}

#[macro_export]
macro_rules! impl_money {
    ($(#[$derive:meta])* $name:ident, $code:expr, $number:expr, $exponent:expr, $format:literal) => {
        paste! {
            lazy_static! {
                pub static ref [<$name:upper _MIN>]: Decimal = Decimal::new(1, $exponent);
            }
        }

        // this line of the macro causes the rust analyzer's formatter to break
        $(#[$derive])*
        #[derive(Eq, PartialEq, Debug, Copy, Clone)]
        pub struct $name {
            kind: &'static str,
            value: Decimal,
        }

        impl Default for $name {
            fn default() -> Self {
                Self {
                    kind: $code,
                    value: Decimal::default(),
                }
            }
        }

        impl From<Decimal> for $name {
            fn from(other: Decimal) -> $name {
                Self {
                    kind: $code,
                    value: other,
                }
            }
        }

        impl $name {
            pub fn new(value: i64) -> Self {
                Self::from_major(value)
            }

            pub fn from_minor(value: i64) -> Self {
                Decimal::new(value, $exponent).into()
            }

            pub fn from_major(value: i64) -> Self {
                Decimal::new(value, 0).into()
            }

            /// This uses rust_decimal's Bankers Rounding Strategy
            pub fn round(&self) -> Self {
                self.value.round_dp($exponent).into()
            }

            pub fn round_up(&self) -> Self {
                self.value
                    .round_dp_with_strategy($exponent, RoundingStrategy::RoundUp)
                    .into()
            }

            pub fn round_down(&self) -> Self {
                self.value
                    .round_dp_with_strategy($exponent, RoundingStrategy::RoundDown)
                    .into()
            }
            pub fn round_half_up(&self) -> Self {
                self.value
                    .round_dp_with_strategy($exponent, RoundingStrategy::RoundHalfUp)
                    .into()
            }

            pub fn round_half_down(&self) -> Self {
                self.value
                    .round_dp_with_strategy($exponent, RoundingStrategy::RoundHalfDown)
                    .into()
            }

            pub fn checked_add(&self, rhs: Self) -> Option<Self> {
                self.value.checked_add(rhs.value).map(Into::into)
            }

            pub fn checked_sub(&self, rhs: Self) -> Option<Self> {
                self.value.checked_sub(rhs.value).map(Into::into)
            }

            pub fn split(&self, n: u64) -> Option<Vec<Self>> {
                if n == 0 {
                    return None;
                }

                // we are not using decimal.checked_div() because given our inputs, we should
                // not overflow or have 0 as the divisor.
                // we use decimal.rescale here because we want to truncate, essentially rounding
                // down.  Rescaling in the decimal library is cheaper than rounding
                let mut quotient = self.value / Decimal::from(n);
                quotient.rescale($exponent);

                let mut remainder = self.value;
                let mut monies: Vec<Self> = (0..n)
                    .into_iter()
                    .map(|_| {
                        remainder -= quotient;
                        quotient.into()
                    })
                    .collect();

                // Add leftovers to the first parties.
                for m in &mut monies {
                    if remainder.is_zero() {
                        break;
                    }
                    paste! {
                        m.value += *[<$name:upper _MIN>];
                        remainder -= *[<$name:upper _MIN>];
                    }
                }
                Some(monies)
            }

            pub fn alloc(&self, ratios: Vec<u64>) -> Option<Vec<Self>> {
                if ratios.is_empty() {
                    return None;
                }

                let mut sum: u64 = 0;
                for ratio in ratios.iter() {
                    if *ratio == 0 {
                        return None;
                    }
                    sum += ratio;
                }

                let sum = Decimal::from(sum);
                let mut total: Decimal = Decimal::from(0);
                let mut monies = ratios
                    .iter()
                    .map(|r| {
                        let decimal_ratio = Decimal::from(*r);
                        let mut share = self.value.checked_mul(decimal_ratio)?;
                        share = share.checked_div(sum)?;
                        share.rescale($exponent);
                        total = total.checked_add(share)?;
                        Some(share.into())
                    })
                    .collect::<Option<Vec<Self>>>()?;

                // Calculate leftover value and divide to first parties.
                let mut remainder = self.value - total;
                if remainder.is_zero() {
                    return Some(monies);
                }

                for m in &mut monies {
                    if remainder.is_zero() {
                        break;
                    }
                    paste! {
                        m.value += *[<$name:upper _MIN>];
                        remainder -= *[<$name:upper _MIN>];
                    }
                }
                Some(monies)
            }
        }

        impl Currency for $name {
            fn decimal_value(&self) -> &Decimal {
                &self.value
            }

            fn is_zero(&self) -> bool {
                self.value.is_zero()
            }

            fn is_negative(&self) -> bool {
                self.value.is_sign_negative() && !self.value.is_zero()
            }

            fn is_positive(&self) -> bool {
                self.value.is_sign_positive() && !self.value.is_zero()
            }

            fn code(&self) -> &'static str {
                self.kind
            }

            fn number(&self) -> &'static str {
                $number
            }

            fn exponent(&self) -> u32 {
                $exponent
            }
        }

        impl PartialOrd for $name {
            fn partial_cmp(&self, other: &$name) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for $name {
            fn cmp(&self, other: &$name) -> std::cmp::Ordering {
                self.value.cmp(&other.value)
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, $format, self.value)
            }
        }

        impl Add for $name {
            type Output = $name;
            fn add(self, other: $name) -> $name {
                (self.value + other.value).into()
            }
        }

        impl AddAssign for $name {
            fn add_assign(&mut self, other: Self) {
                self.value = self.value + other.value;
            }
        }

        impl Sub for $name {
            type Output = $name;
            fn sub(self, other: $name) -> $name {
                (self.value - other.value).into()
            }
        }

        impl SubAssign for $name {
            fn sub_assign(&mut self, other: Self) {
                self.value = self.value - other.value;
            }
        }

        impl_mul_div!(isize, $name, $exponent);
        impl_mul_div!(i32, $name, $exponent);
        impl_mul_div!(i64, $name, $exponent);
        impl_mul_div!(usize, $name, $exponent);
        impl_mul_div!(u32, $name, $exponent);
        impl_mul_div!(u64, $name, $exponent);
    };
}

#[allow(unused_macros)]
macro_rules! impl_mul_div {
    ($type:ty, $name:ident, $exponent:expr) => {
        impl Mul<$type> for $name {
            type Output = $name;

            fn mul(self, rhs: $type) -> $name {
                (self.value * Decimal::from(rhs)).into()
            }
        }

        impl Mul<$name> for $type {
            type Output = $name;

            fn mul(self, rhs: $name) -> Self::Output {
                (Decimal::from(self) * rhs.value).into()
            }
        }

        impl MulAssign<$type> for $name {
            fn mul_assign(&mut self, rhs: $type) {
                self.value = (self.value * Decimal::from(rhs));
            }
        }

        impl Div<$type> for $name {
            type Output = $name;

            fn div(self, rhs: $type) -> Self::Output {
                (self.value / Decimal::from(rhs))
                    .round_dp_with_strategy($exponent, RoundingStrategy::BankersRounding)
                    .into()
            }
        }

        impl Div<$name> for $type {
            type Output = $name;

            fn div(self, rhs: $name) -> Self::Output {
                (Decimal::from(self) / rhs.value)
                    .round_dp_with_strategy($exponent, RoundingStrategy::BankersRounding)
                    .into()
            }
        }

        impl DivAssign<$type> for $name {
            fn div_assign(&mut self, rhs: $type) {
                self.value = (self.value / Decimal::from(rhs))
                    .round_dp_with_strategy($exponent, RoundingStrategy::BankersRounding);
            }
        }
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use serde::{Deserialize, Serialize};

    impl_money!(
        #[derive(Serialize, Deserialize)]
        Bitcoin,
        "BTC",
        "9000",
        8,
        "{} $"
    );
    impl_money!(
        #[derive(Serialize, Deserialize)]
        USDollar,
        "USD",
        "840",
        2,
        "$ {}"
    );
    impl_money!(
        #[derive(Serialize, Deserialize)]
        Twd,
        "NT",
        "901",
        0,
        "{} NT"
    );

    #[test]
    fn it_is_sane() {
        // Bitcoin { kind: "BTC", value: 0.00000001 }
        let one_satoshi = Bitcoin::from_minor(1);
        println!("{:?}", one_satoshi);

        // Bitcoin { kind: "BTC", value: 1 }
        let one_bitcoin = Bitcoin::from_major(1);
        println!("{:?}", one_bitcoin);

        // USDollar { kind: "USD", value: 1 }
        let one_quarter = USDollar::from_minor(25);
        println!("{:?}", one_quarter);

        // USDollar { kind: "USD", value: 1 }
        let one_dollar = USDollar::new(1);
        println!("{:?}", one_dollar);

        // compiler error
        // let unknown = one_dollar + one_bitcoin;
    }

    #[test]
    fn it_accepts_other_derives() {}

    #[test]
    fn it_handles_is_zero() {
        let test_cases: Vec<(Bitcoin, bool)> = vec![
            (Bitcoin::new(-1), false),
            (Bitcoin::new(0), true),
            (Bitcoin::new(1), false),
        ];

        for (value, expected) in test_cases {
            assert_eq!(expected, value.is_zero());
        }
    }

    #[test]
    fn it_handles_is_positive() {
        let test_cases: Vec<(Bitcoin, bool)> = vec![
            (Bitcoin::new(-1), false),
            (Bitcoin::new(0), false),
            (Bitcoin::new(1), true),
        ];

        for (value, expected) in test_cases {
            assert_eq!(expected, value.is_positive());
        }
    }

    #[test]
    fn it_handles_is_negative() {
        let test_cases: Vec<(Bitcoin, bool)> = vec![
            (Bitcoin::new(-1), true),
            (Bitcoin::new(0), false),
            (Bitcoin::new(1), false),
        ];

        for (value, expected) in test_cases {
            assert_eq!(expected, value.is_negative());
        }
    }

    #[test]
    fn it_handles_add() {
        let test_cases: Vec<(Bitcoin, Bitcoin, Bitcoin)> = vec![
            (Bitcoin::new(5), Bitcoin::new(5), Bitcoin::new(10)),
            (Bitcoin::new(10), Bitcoin::new(5), Bitcoin::new(15)),
            (Bitcoin::new(1), Bitcoin::new(-1), Bitcoin::new(0)),
        ];

        for (a, b, expected) in test_cases {
            assert_eq!(expected, a + b);
        }
    }

    #[test]
    fn it_handles_subtract() {
        let test_cases: Vec<(Bitcoin, Bitcoin, Bitcoin)> = vec![
            (Bitcoin::new(5), Bitcoin::new(5), Bitcoin::new(0)),
            (Bitcoin::new(10), Bitcoin::new(5), Bitcoin::new(5)),
            (Bitcoin::new(1), Bitcoin::new(-1), Bitcoin::new(2)),
        ];

        for (a, b, expected) in test_cases {
            assert_eq!(expected, a - b);
        }
    }

    #[test]
    fn it_handles_multiply_i64() {
        let test_cases: Vec<(Bitcoin, i64, Bitcoin)> = vec![
            (Bitcoin::new(5), 5, Bitcoin::new(25)),
            (Bitcoin::new(10), 5, Bitcoin::new(50)),
            (Bitcoin::new(1), -1, Bitcoin::new(-1)),
            (Bitcoin::new(1), 0, Bitcoin::new(0)),
        ];

        for (a, b, expected) in test_cases {
            let mut a_assign = a.clone();
            a_assign *= b;
            assert_eq!(expected, a_assign);
            assert_eq!(expected, a * b);
            assert_eq!(expected, b * a);
        }
    }

    #[test]
    fn it_handles_divide_i64() {
        let test_cases: Vec<(Bitcoin, i64, Bitcoin)> = vec![
            (Bitcoin::new(5), 5, Bitcoin::new(1)),
            (Bitcoin::new(10), 5, Bitcoin::new(2)),
            (Bitcoin::new(1), -1, Bitcoin::new(-1)),
            (Bitcoin::new(10), 3, Bitcoin::from_minor(333333333)),
        ];

        for (a, b, expected) in test_cases {
            let mut a_assign = a.clone();
            a_assign /= b;
            // assert_eq!(expected, a_assign);
            assert_eq!(expected, a / b);
        }
    }

    #[test]
    fn it_should_split_bitcoins() {
        let test_cases: Vec<(Bitcoin, u64, Option<Vec<Bitcoin>>)> = vec![
            (
                Bitcoin::from_minor(100),
                3,
                Some(vec![
                    Bitcoin::from_minor(34),
                    Bitcoin::from_minor(33),
                    Bitcoin::from_minor(33),
                ]),
            ),
            (
                Bitcoin::from_major(1),
                3,
                Some(vec![
                    Bitcoin::from_minor(33333334),
                    Bitcoin::from_minor(33333333),
                    Bitcoin::from_minor(33333333),
                ]),
            ),
            (
                Bitcoin::from_minor(100),
                4,
                Some(vec![
                    Bitcoin::from_minor(25),
                    Bitcoin::from_minor(25),
                    Bitcoin::from_minor(25),
                    Bitcoin::from_minor(25),
                ]),
            ),
            (Bitcoin::from_minor(100), 0, None),
            (
                Bitcoin::from_minor(0),
                3,
                Some(vec![
                    Bitcoin::from_minor(0),
                    Bitcoin::from_minor(0),
                    Bitcoin::from_minor(0),
                ]),
            ),
        ];

        for (value, split, expected) in test_cases {
            let result = value.split(split);
            match result {
                Some(monies) => {
                    let expected_value = expected.unwrap();
                    let mut idx = 0;
                    for money in monies {
                        assert_eq!(expected_value[idx], money);
                        idx += 1;
                    }
                }
                _ => assert_eq!(result, expected),
            }
        }
    }

    #[test]
    fn it_should_split_twd_no_dp() {
        let test_cases: Vec<(Twd, u64, Option<Vec<Twd>>)> = vec![
            (
                Twd::from_major(100),
                3,
                Some(vec![
                    Twd::from_major(34),
                    Twd::from_major(33),
                    Twd::from_major(33),
                ]),
            ),
            (
                Twd::from_minor(100),
                3,
                Some(vec![
                    Twd::from_minor(34),
                    Twd::from_minor(33),
                    Twd::from_minor(33),
                ]),
            ),
            (
                Twd::from_major(1),
                3,
                Some(vec![
                    Twd::from_minor(1),
                    Twd::from_minor(0),
                    Twd::from_minor(0),
                ]),
            ),
            (
                Twd::from_minor(100),
                4,
                Some(vec![
                    Twd::from_minor(25),
                    Twd::from_minor(25),
                    Twd::from_minor(25),
                    Twd::from_minor(25),
                ]),
            ),
            (
                Twd::from_major(100),
                4,
                Some(vec![
                    Twd::from_major(25),
                    Twd::from_major(25),
                    Twd::from_major(25),
                    Twd::from_major(25),
                ]),
            ),
            (Twd::from_minor(100), 0, None),
            (
                Twd::from_minor(0),
                3,
                Some(vec![
                    Twd::from_minor(0),
                    Twd::from_minor(0),
                    Twd::from_minor(0),
                ]),
            ),
        ];

        for (value, split, expected) in test_cases {
            let result = value.split(split);
            match result {
                Some(monies) => {
                    let expected_value = expected.unwrap();
                    let mut idx = 0;
                    for money in monies {
                        assert_eq!(expected_value[idx], money);
                        idx += 1;
                    }
                }
                _ => assert_eq!(result, expected),
            }
        }
    }

    #[test]
    fn it_should_allocate_bitcoins() {
        let test_cases: Vec<(Bitcoin, Vec<u64>, Option<Vec<Bitcoin>>)> = vec![
            (
                Bitcoin::from_major(100),
                vec![50, 50],
                Some(vec![Bitcoin::from_major(50), Bitcoin::from_major(50)]),
            ),
            (
                Bitcoin::from_major(1),
                vec![30, 30, 30],
                Some(vec![
                    Bitcoin::from_minor(33333334),
                    Bitcoin::from_minor(33333333),
                    Bitcoin::from_minor(33333333),
                ]),
            ),
            (
                Bitcoin::from_major(200),
                vec![25, 25, 50],
                Some(vec![
                    Bitcoin::from_major(50),
                    Bitcoin::from_major(50),
                    Bitcoin::from_major(100),
                ]),
            ),
            (
                Bitcoin::from_major(5),
                vec![50, 25, 25],
                Some(vec![
                    Bitcoin::from_minor(250000000),
                    Bitcoin::from_minor(125000000),
                    Bitcoin::from_minor(125000000),
                ]),
            ),
            (Bitcoin::from_major(5), vec![0, 1000], None),
            (Bitcoin::from_major(5), Vec::new(), None),
        ];

        for (value, ratio, expected) in test_cases {
            let result = value.alloc(ratio);
            match result {
                Some(monies) => {
                    for (i, v) in expected.unwrap().iter().enumerate() {
                        assert_eq!(monies[i], *v);
                    }
                }
                _ => assert_eq!(result, expected),
            }
        }
    }

    #[test]
    fn it_should_allocate_twd_no_dp() {
        let test_cases: Vec<(Twd, Vec<u64>, Option<Vec<Twd>>)> = vec![
            (
                Twd::from_major(100),
                vec![50, 50],
                Some(vec![Twd::from_major(50), Twd::from_major(50)]),
            ),
            (
                Twd::from_major(1),
                vec![30, 30, 30],
                Some(vec![
                    Twd::from_minor(1),
                    Twd::from_minor(0),
                    Twd::from_minor(0),
                ]),
            ),
            (
                Twd::from_major(1),
                vec![30, 30, 30],
                Some(vec![
                    Twd::from_major(1),
                    Twd::from_major(0),
                    Twd::from_major(0),
                ]),
            ),
            (
                Twd::from_major(100),
                vec![30, 30, 30],
                Some(vec![
                    Twd::from_major(34),
                    Twd::from_major(33),
                    Twd::from_major(33),
                ]),
            ),
            (
                Twd::from_major(100),
                vec![30, 30, 30],
                Some(vec![
                    Twd::from_minor(34),
                    Twd::from_minor(33),
                    Twd::from_minor(33),
                ]),
            ),
            (
                Twd::from_major(100),
                vec![30, 30, 30],
                Some(vec![
                    Twd::from_minor(34),
                    Twd::from_major(33),
                    Twd::from_minor(33),
                ]),
            ),
            (
                Twd::from_major(200),
                vec![25, 25, 50],
                Some(vec![
                    Twd::from_major(50),
                    Twd::from_major(50),
                    Twd::from_major(100),
                ]),
            ),
            (
                Twd::from_major(5),
                vec![50, 25, 25],
                Some(vec![
                    Twd::from_major(3),
                    Twd::from_minor(1),
                    Twd::from_minor(1),
                ]),
            ),
            (
                Twd::from_major(5),
                vec![50, 25, 25],
                Some(vec![
                    Twd::from_minor(3),
                    Twd::from_minor(1),
                    Twd::from_minor(1),
                ]),
            ),
            (Twd::from_major(5), vec![0, 1000], None),
            (Twd::from_major(5), Vec::new(), None),
        ];

        for (value, ratio, expected) in test_cases {
            let result = value.alloc(ratio);
            match result {
                Some(monies) => {
                    for (i, v) in expected.unwrap().iter().enumerate() {
                        assert_eq!(monies[i], *v);
                    }
                }
                _ => assert_eq!(result, expected),
            }
        }
    }
}
