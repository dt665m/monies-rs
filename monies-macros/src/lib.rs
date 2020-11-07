#![allow(unused_imports)]
pub use monies::{Currency, CurrencyISO};
pub use std::cmp::Ordering;
pub use std::convert::{From, Into};
pub use std::fmt;
pub use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};

pub use lazy_static::lazy_static;
pub use num_traits::{CheckedAdd, CheckedSub, Zero};
pub use paste::paste;
pub use rust_decimal::prelude::*;

#[macro_export]
macro_rules! impl_money {
    ($(#[$derive:meta])* $name:ident, $code:expr, $number:expr, $exponent:expr, $format:literal) => {
        // this is used for allocs so we don't need to make a new decimal each time
        $crate::paste! {
            $crate::lazy_static! {
                static ref [<$name:upper _MIN>]: $crate::Decimal = $crate::Decimal::new(1, $exponent);
            }
        }

        // this line of the macro causes the rust analyzer's formatter to break
        $(#[$derive])*
        #[derive(Eq, PartialEq, Debug, Copy, Clone)]
        pub struct $name($crate::Decimal);

        impl Default for $name {
            fn default() -> Self {
                Self($crate::Decimal::default())
            }
        }

        impl From<$crate::Decimal> for $name {
            fn from(other: $crate::Decimal) -> $name {
                Self(other)
            }
        }

        impl $name {
            pub fn new(value: i64) -> Self {
                Self::from_major(value)
            }

            pub fn from_minor(value: i64) -> Self {
                $crate::Decimal::new(value, $exponent).into()
            }

            pub fn from_major(value: i64) -> Self {
                $crate::Decimal::new(value, 0).into()
            }

            /// This uses rust_decimal's Bankers Rounding Strategy
            pub fn round(&self) -> Self {
                self.0.round_dp($exponent).into()
            }

            pub fn round_up(&self) -> Self {
                self.0
                    .round_dp_with_strategy($exponent, $crate::RoundingStrategy::RoundUp)
                    .into()
            }

            pub fn round_down(&self) -> Self {
                self.0
                    .round_dp_with_strategy($exponent, $crate::RoundingStrategy::RoundDown)
                    .into()
            }
            pub fn round_half_up(&self) -> Self {
                self.0
                    .round_dp_with_strategy($exponent, $crate::RoundingStrategy::RoundHalfUp)
                    .into()
            }

            pub fn round_half_down(&self) -> Self {
                self.0
                    .round_dp_with_strategy($exponent, $crate::RoundingStrategy::RoundHalfDown)
                    .into()
            }

            pub fn split(&self, n: u64) -> Option<Vec<Self>> {
                if n == 0 {
                    return None;
                }

                // we are not using decimal.checked_div() because given our inputs, we should
                // not overflow or have 0 as the divisor.
                // we use decimal.rescale here because we want to truncate, essentially rounding
                // down.  Rescaling in the decimal library is cheaper than rounding
                let mut quotient = self.0 / $crate::Decimal::from(n);
                quotient.rescale($exponent);

                let mut remainder = self.0;
                let mut monies: Vec<Self> = (0..n)
                    .into_iter()
                    .map(|_| {
                        remainder -= quotient;
                        quotient.into()
                    })
                    .collect();

                // Add leftovers to the first parties.
                use $crate::Zero;
                for m in &mut monies {
                    if remainder.is_zero() {
                        break;
                    }
                    $crate::paste! {
                        m.0 += *[<$name:upper _MIN>];
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

                let sum = $crate::Decimal::from(sum);
                let mut total: $crate::Decimal = $crate::Decimal::from(0);
                let mut monies = ratios
                    .iter()
                    .map(|r| {
                        let decimal_ratio = $crate::Decimal::from(*r);
                        let mut share = self.0.checked_mul(decimal_ratio)?;
                        share = share.checked_div(sum)?;
                        share.rescale($exponent);
                        total = total.checked_add(share)?;
                        Some(share.into())
                    })
                    .collect::<Option<Vec<Self>>>()?;

                // Calculate leftover value and divide to first parties.
                use $crate::Zero;
                let mut remainder = self.0 - total;
                if remainder.is_zero() {
                    return Some(monies);
                }

                for m in &mut monies {
                    if remainder.is_zero() {
                        break;
                    }
                    $crate::paste! {
                        m.0 += *[<$name:upper _MIN>];
                        remainder -= *[<$name:upper _MIN>];
                    }
                }
                Some(monies)
            }
        }

        impl $crate::CheckedAdd for $name {
             fn checked_add(&self, rhs: &Self) -> Option<Self> {
                self.0.checked_add(rhs.0).map(Into::into)
            }
        }

        impl $crate::CheckedSub for $name {
            fn checked_sub(&self, rhs: &Self) -> Option<Self> {
                self.0.checked_sub(rhs.0).map(Into::into)
            }
        }

        impl $crate::Zero for $name {
            fn is_zero(&self) -> bool {
                self.0.is_zero()
            }

            fn zero() -> Self {
                $name::from_major(0)
            }
        }

        impl $crate::Currency for $name {
            fn decimal_value(&self) -> &$crate::Decimal {
                &self.0
            }

            fn is_negative(&self) -> bool {
                use $crate::Zero;
                self.0.is_sign_negative() && !self.0.is_zero()
            }

            fn is_positive(&self) -> bool {
                use $crate::Zero;
                self.0.is_sign_positive() && !self.0.is_zero()
            }
        }

        impl $crate::CurrencyISO for $name {
            fn code() -> &'static str {
                $code
            }

            fn number() -> &'static str {
                $number
            }

            fn exponent() -> u32 {
                $exponent
            }
        }

        impl PartialOrd for $name {
            fn partial_cmp(&self, other: &$name) -> Option<$crate::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for $name {
            fn cmp(&self, other: &$name) -> $crate::Ordering {
                self.0.cmp(&other.0)
            }
        }

        impl $crate::fmt::Display for $name {
            fn fmt(&self, f: &mut $crate::fmt::Formatter<'_>) -> $crate::fmt::Result {
                write!(f, $format, self.0)
            }
        }

        impl $crate::Add for $name {
            type Output = $name;

            fn add(self, other: $name) -> $name {
                (self.0 + other.0).into()
            }
        }

        impl $crate::AddAssign for $name {
            fn add_assign(&mut self, other: Self) {
                self.0 = self.0 + other.0;
            }
        }

        impl $crate::Sub for $name {
            type Output = $name;

            fn sub(self, other: $name) -> $name {
                (self.0 - other.0).into()
            }
        }

        impl $crate::SubAssign for $name {
            fn sub_assign(&mut self, other: Self) {
                self.0 = self.0 - other.0;
            }
        }

        impl_money!(@impl_mul_div isize, $name, $exponent);
        impl_money!(@impl_mul_div i32, $name, $exponent);
        impl_money!(@impl_mul_div i64, $name, $exponent);
        impl_money!(@impl_mul_div usize, $name, $exponent);
        impl_money!(@impl_mul_div u32, $name, $exponent);
        impl_money!(@impl_mul_div u64, $name, $exponent);
    };

    // remove @ for formatting.  Yay
    (@impl_mul_div $type:ty, $name:ident, $exponent:expr) => {
        impl $crate::Mul<$type> for $name {
            type Output = $name;

            fn mul(self, rhs: $type) -> $name {
                (self.0 * $crate::Decimal::from(rhs)).into()
            }
        }

        impl $crate::Mul<$name> for $type {
            type Output = $name;

            fn mul(self, rhs: $name) -> Self::Output {
                ($crate::Decimal::from(self) * rhs.0).into()
            }
        }

        impl $crate::MulAssign<$type> for $name {
            fn mul_assign(&mut self, rhs: $type) {
                self.0 = (self.0 * $crate::Decimal::from(rhs));
            }
        }

        impl $crate::Div<$type> for $name {
            type Output = $name;

            fn div(self, rhs: $type) -> Self::Output {
                (self.0 / $crate::Decimal::from(rhs))
                    .round_dp_with_strategy($exponent, $crate::RoundingStrategy::BankersRounding)
                    .into()
            }
        }

        impl $crate::Div<$name> for $type {
            type Output = $name;

            fn div(self, rhs: $name) -> Self::Output {
                ($crate::Decimal::from(self) / rhs.0)
                    .round_dp_with_strategy($exponent, $crate::RoundingStrategy::BankersRounding)
                    .into()
            }
        }

        impl $crate::DivAssign<$type> for $name {
            fn div_assign(&mut self, rhs: $type) {
                self.0 = (self.0 / $crate::Decimal::from(rhs))
                    .round_dp_with_strategy($exponent, $crate::RoundingStrategy::BankersRounding);
            }
        }
    };
}

#[cfg(test)]
mod test {
    use super::prelude::*;
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
