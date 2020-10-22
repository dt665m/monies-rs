use std::convert::From;
use std::ops::{Add, AddAssign, Sub, SubAssign};

use num_traits::{CheckedAdd, CheckedSub, Zero};
use rust_decimal::Decimal;

pub mod prelude {
    pub use super::{Currency, CurrencyISO, SimpleArithmetic};
    pub use num_traits::{CheckedAdd, CheckedSub, Zero};
    pub use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};
}

pub trait SimpleArithmetic:
    Add<Self, Output = Self>
    + AddAssign<Self>
    + Sub<Self, Output = Self>
    + SubAssign<Self>
    + PartialOrd<Self>
    + Ord
    + Sized
    + Copy
    + Default
    + CheckedAdd
    + CheckedSub
    + Zero
    + From<Decimal>
{
}

impl<
        T: Zero
            + Add<Self, Output = Self>
            + AddAssign<Self>
            + Sub<Self, Output = Self>
            + SubAssign<Self>
            + PartialOrd<Self>
            + Ord
            + Sized
            + Copy
            + Default
            + CheckedAdd
            + CheckedSub
            + From<Decimal>,
    > SimpleArithmetic for T
{
}

pub trait Currency {
    fn decimal_value(&self) -> &Decimal;

    fn is_negative(&self) -> bool;

    fn is_positive(&self) -> bool;
}

pub trait CurrencyISO {
    fn code() -> &'static str;

    fn number() -> &'static str;

    fn exponent() -> u32;
}
