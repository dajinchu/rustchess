

use std::ops::{Add, Mul};
#[derive(Copy, Clone, Debug)]
pub struct Vec2D {
    /// Squares to move north
    pub vertical: i8,
    /// Squares to move east
    pub horizontal: i8,
}

impl const Add for Vec2D {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Vec2D {
            vertical: self.vertical + rhs.vertical,
            horizontal: self.horizontal + rhs.horizontal,
        }
    }
}

impl const Mul<i8> for Vec2D {
    type Output = Self;

    fn mul(self, rhs: i8) -> Self::Output {
        Vec2D {
            vertical: self.vertical * rhs,
            horizontal: self.horizontal * rhs,
        }
    }
}

impl const Mul<Vec2D> for i8 {
    type Output = Vec2D;

    fn mul(self, rhs: Vec2D) -> Self::Output {
        Vec2D {
            vertical: rhs.vertical * self,
            horizontal: rhs.horizontal * self,
        }
    }
}

#[macro_export]
macro_rules! vec2d {
    ( $v:expr, $h:expr ) => {{
        Vec2D {
            vertical: $v,
            horizontal: $h,
        }
    }};
}

pub(crate) use vec2d;