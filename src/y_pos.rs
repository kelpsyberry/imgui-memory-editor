#![allow(clippy::suspicious_arithmetic_impl)]

use core::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Sub, SubAssign};

pub type YPosRaw = u128;
pub type SignedYPosRaw = i128;

const FRACT_BITS: u32 = 8;

macro_rules! impl_bin_ops {
    ($ty: ty, $raw: ty) => {
        impl_bin_ops!(
            $ty, $raw;
            Add, add, AddAssign, add_assign, wrapped;
            Sub, sub, SubAssign, sub_assign, wrapped;
            Mul, mul, MulAssign, mul_assign, wrapped, shifted_res;
            Div, div, DivAssign, div_assign, wrapped, shifted_lhs;
            Rem, rem, RemAssign, rem_assign, wrapped;
            Add, add, AddAssign, add_assign, raw, shifted_rhs;
            Sub, sub, SubAssign, sub_assign, raw, shifted_rhs;
            Mul, mul, MulAssign, mul_assign, raw;
            Div, div, DivAssign, div_assign, raw;
            Rem, rem, RemAssign, rem_assign, raw, shifted_rhs;
            Add, add, AddAssign, add_assign, float f32, f64;
            Sub, sub, SubAssign, sub_assign, float f32, f64;
            Mul, mul, MulAssign, mul_assign, float f32, f64;
            Div, div, DivAssign, div_assign, float f32, f64;
            Rem, rem, RemAssign, rem_assign, float f32, f64;
        );
    };

    ($ty: ty, $raw: ty;) => {};

    (
        $ty: ty, $raw: ty;
        $trait: ident, $fn: ident, $trait_assign: ident, $fn_assign: ident, wrapped; $($remaining: tt)*) => {
        impl $trait for $ty {
            type Output = Self;
            #[inline]
            fn $fn(self, rhs: Self) -> Self::Output {
                Self(self.0.$fn(rhs.0))
            }
        }
        impl $trait_assign for $ty {
            #[inline]
            fn $fn_assign(&mut self, rhs: Self) {
                self.0.$fn_assign(rhs.0);
            }
        }
        impl_bin_ops!($ty, $raw; $($remaining)*);
    };

    (
        $ty: ty, $raw: ty;
        $trait: ident, $fn: ident, $trait_assign: ident, $fn_assign: ident, wrapped, shifted_lhs;
        $($remaining: tt)*
    ) => {
        impl $trait for $ty {
            type Output = Self;
            #[inline]
            fn $fn(self, rhs: Self) -> Self::Output {
                Self((self.0 << FRACT_BITS).$fn(rhs.0))
            }
        }
        impl $trait_assign for $ty {
            #[inline]
            #[allow(clippy::suspicious_op_assign_impl)]
            fn $fn_assign(&mut self, rhs: Self) {
                self.0 = (self.0 << FRACT_BITS).$fn(rhs.0);
            }
        }
        impl_bin_ops!($ty, $raw; $($remaining)*);
    };

    (
        $ty: ty, $raw: ty;
        $trait: ident, $fn: ident, $trait_assign: ident, $fn_assign: ident, wrapped, shifted_res;
        $($remaining: tt)*
    ) => {
        impl $trait for $ty {
            type Output = Self;
            #[inline]
            fn $fn(self, rhs: Self) -> Self::Output {
                Self(self.0.$fn(rhs.0) >> FRACT_BITS)
            }
        }
        impl $trait_assign for $ty {
            #[inline]
            #[allow(clippy::suspicious_op_assign_impl)]
            fn $fn_assign(&mut self, rhs: Self) {
                self.0 = self.0.$fn(rhs.0) >> FRACT_BITS;
            }
        }
        impl_bin_ops!($ty, $raw; $($remaining)*);
    };

    (
        $ty: ty, $raw: ty;
        $trait: ident, $fn: ident, $trait_assign: ident, $fn_assign: ident, raw;
        $($remaining: tt)*
    ) => {
        impl $trait<$raw> for $ty {
            type Output = Self;
            #[inline]
            fn $fn(self, rhs: $raw) -> Self::Output {
                Self(self.0.$fn(rhs))
            }
        }
        impl $trait_assign<$raw> for $ty {
            #[inline]
            fn $fn_assign(&mut self, rhs: $raw) {
                self.0.$fn_assign(rhs);
            }
        }
        impl_bin_ops!($ty, $raw; $($remaining)*);
    };

    (
        $ty: ty, $raw: ty;
        $trait: ident, $fn: ident, $trait_assign: ident, $fn_assign: ident, raw, shifted_rhs;
        $($remaining: tt)*
    ) => {
        impl $trait<$raw> for $ty {
            type Output = Self;
            #[inline]
            fn $fn(self, rhs: $raw) -> Self::Output {
                Self(self.0.$fn(rhs << FRACT_BITS))
            }
        }
        impl $trait_assign<$raw> for $ty {
            #[inline]
            #[allow(clippy::suspicious_op_assign_impl)]
            fn $fn_assign(&mut self, rhs: $raw) {
                self.0.$fn_assign(rhs << FRACT_BITS);
            }
        }
        impl_bin_ops!($ty, $raw; $($remaining)*);
    };

    (
        $ty: ty, $raw: ty;
        $trait: ident, $fn: ident, $trait_assign: ident, $fn_assign: ident,
        float $($float_ty: ty),*;
        $($remaining: tt)*
    ) => {
        $(
            impl $trait<$float_ty> for $ty {
                type Output = Self;
                #[inline]
                fn $fn(self, rhs: $float_ty) -> Self::Output {
                    self.$fn(Self::from(rhs))
                }
            }
        )*
        impl_bin_ops!($ty, $raw; $($remaining)*);
    };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct YPos(pub YPosRaw);

impl YPos {
    #[inline]
    pub fn as_signed(self) -> SignedYPos {
        SignedYPos(self.0 as SignedYPosRaw)
    }

    #[inline]
    pub fn from_int(value: YPosRaw) -> Self {
        YPos(value << FRACT_BITS)
    }

    #[inline]
    pub fn saturating_add(self, other: Self) -> Self {
        YPos(self.0.saturating_add(other.0))
    }

    #[inline]
    pub fn saturating_sub(self, other: Self) -> Self {
        YPos(self.0.saturating_sub(other.0))
    }

    #[inline]
    pub fn div_into_int(self, other: Self) -> YPosRaw {
        self.0 / other.0
    }

    #[inline]
    pub fn div_ceil_into_int(self, other: Self) -> YPosRaw {
        (self.0 + other.0 - 1) / other.0
    }
}

impl From<f32> for YPos {
    #[inline]
    fn from(value: f32) -> Self {
        YPos((value as f64 * (1 << FRACT_BITS) as f64) as YPosRaw)
    }
}

impl From<YPos> for f32 {
    #[inline]
    fn from(value: YPos) -> Self {
        ((value.0 as f64) / (1 << FRACT_BITS) as f64) as f32
    }
}

impl From<f64> for YPos {
    #[inline]
    fn from(value: f64) -> Self {
        YPos((value * (1 << FRACT_BITS) as f64) as YPosRaw)
    }
}

impl From<YPos> for f64 {
    #[inline]
    fn from(value: YPos) -> Self {
        (value.0 as f64) / (1 << FRACT_BITS) as f64
    }
}

impl_bin_ops!(YPos, YPosRaw);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SignedYPos(pub SignedYPosRaw);

impl SignedYPos {
    #[inline]
    pub fn as_unsigned(self) -> YPos {
        YPos(self.0 as YPosRaw)
    }
}

impl From<SignedYPosRaw> for SignedYPos {
    #[inline]
    fn from(int: SignedYPosRaw) -> Self {
        SignedYPos(int << FRACT_BITS)
    }
}

impl From<SignedYPos> for SignedYPosRaw {
    #[inline]
    fn from(value: SignedYPos) -> Self {
        value.0 >> FRACT_BITS
    }
}

impl From<f32> for SignedYPos {
    #[inline]
    fn from(value: f32) -> Self {
        SignedYPos((value as f64 * (1 << FRACT_BITS) as f64) as SignedYPosRaw)
    }
}

impl From<SignedYPos> for f32 {
    #[inline]
    fn from(value: SignedYPos) -> Self {
        ((value.0 as f64) / (1 << FRACT_BITS) as f64) as f32
    }
}

impl From<f64> for SignedYPos {
    #[inline]
    fn from(value: f64) -> Self {
        SignedYPos((value * (1 << FRACT_BITS) as f64) as SignedYPosRaw)
    }
}

impl From<SignedYPos> for f64 {
    #[inline]
    fn from(value: SignedYPos) -> Self {
        (value.0 as f64) / (1 << FRACT_BITS) as f64
    }
}

impl_bin_ops!(SignedYPos, SignedYPosRaw);
