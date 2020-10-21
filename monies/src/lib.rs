use num_traits::{CheckedAdd, CheckedSub, Zero};
use rust_decimal::Decimal;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};

pub mod prelude {
    pub use super::{Currency, SimpleArithmetic};
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
    + CheckedAdd
    + CheckedSub
    + Zero
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
            + CheckedAdd
            + CheckedSub,
    > SimpleArithmetic for T
{
}

pub trait Currency: SimpleArithmetic {
    fn decimal_value(&self) -> &Decimal;

    fn is_negative(&self) -> bool;

    fn is_positive(&self) -> bool;

    fn code(&self) -> &'static str;

    fn number(&self) -> &'static str;

    fn exponent(&self) -> u32;
}
