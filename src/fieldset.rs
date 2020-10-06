#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code

/*!
   Implement sets of fields using 64-bit integers
*/

use std::ops::Add;
use std::ops::Mul;
use std::ops::Not;
use std::ops::Sub;

#[rustfmt::skip]
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Field {
      A1 , B1 , C1 , D1 , E1 , F1 , G1 , H1
    , A2 , B2 , C2 , D2 , E2 , F2 , G2 , H2
    , A3 , B3 , C3 , D3 , E3 , F3 , G3 , H3
    , A4 , B4 , C4 , D4 , E4 , F4 , G4 , H4
    , A5 , B5 , C5 , D5 , E5 , F5 , G5 , H5
    , A6 , B6 , C6 , D6 , E6 , F6 , G6 , H6
    , A7 , B7 , C7 , D7 , E7 , F7 , G7 , H7
    , A8 , B8 , C8 , D8 , E8 , F8 , G8 , H8
}

#[rustfmt::skip]
static ALLFIELDS: [Field; 64] = [
      Field::A1 , Field::B1 , Field::C1 , Field::D1 , Field::E1 , Field::F1 , Field::G1 , Field::H1
    , Field::A2 , Field::B2 , Field::C2 , Field::D2 , Field::E2 , Field::F2 , Field::G2 , Field::H2
    , Field::A3 , Field::B3 , Field::C3 , Field::D3 , Field::E3 , Field::F3 , Field::G3 , Field::H3
    , Field::A4 , Field::B4 , Field::C4 , Field::D4 , Field::E4 , Field::F4 , Field::G4 , Field::H4
    , Field::A5 , Field::B5 , Field::C5 , Field::D5 , Field::E5 , Field::F5 , Field::G5 , Field::H5
    , Field::A6 , Field::B6 , Field::C6 , Field::D6 , Field::E6 , Field::F6 , Field::G6 , Field::H6
    , Field::A7 , Field::B7 , Field::C7 , Field::D7 , Field::E7 , Field::F7 , Field::G7 , Field::H7
    , Field::A8 , Field::B8 , Field::C8 , Field::D8 , Field::E8 , Field::F8 , Field::G8 , Field::H8
];

impl From<u8> for Field {
    fn from(u: u8) -> Field {
        ALLFIELDS[(u & 0x3f) as usize]
    }
}

impl Into<u8> for Field {
    fn into(self) -> u8 {
        self as u8
    }
}

impl Into<usize> for Field {
    fn into(self) -> usize {
        self as usize
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct BitSet {
    bits: u64,
}

impl BitSet {
    pub const fn empty() -> BitSet {
        BitSet { bits: 0 }
    }
    #[inline]
    pub const fn singleton(m: Field) -> BitSet {
        BitSet { bits: 1 << m as u8 }
    }
    #[inline]
    pub const fn member(self, m: Field) -> bool {
        self.bits & BitSet::singleton(m).bits != 0
    }
    #[inline]
    pub const fn union(self, other: BitSet) -> BitSet {
        BitSet {
            bits: self.bits | other.bits,
        }
    }
    #[inline]
    /// the set of elements that are members of both sets
    pub const fn intersection(self, other: BitSet) -> BitSet {
        BitSet {
            bits: self.bits & other.bits,
        }
    }
    #[inline]
    pub const fn difference(self, other: BitSet) -> BitSet {
        BitSet {
            bits: self.bits & !other.bits,
        }
    }
}

impl Add for BitSet {
    type Output = Self;
    fn add(self, other: Self) -> Self::Output {
        self.union(other)
    }
}

impl Sub for BitSet {
    type Output = Self;
    fn sub(self, other: Self) -> Self::Output {
        self.difference(other)
    }
}

impl Mul for BitSet {
    type Output = Self;
    fn mul(self, other: Self) -> Self::Output {
        self.intersection(other)
    }
}

impl Not for BitSet {
    type Output = Self;
    fn not(self) -> Self::Output {
        BitSet { bits: !self.bits }
    }
}

impl From<u64> for BitSet {
    fn from(bits: u64) -> BitSet {
        BitSet { bits }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn a1_is_0() {
        assert_eq!(Field::A1 as u8, 0);
    }

    #[test]
    fn seven_is_H1() {
        assert_eq!(Field::from(7u8), Field::H1);
    }

    #[test]
    /// Field::from(f as u8) == f
    fn from_to() {
        for f in ALLFIELDS.iter() {
            assert_eq!(*f, Field::from(*f as u8));
        }
    }

    #[test]
    fn test_ops_empty() {
        let b = BitSet {
            bits: 0x1234_5678_9ABC_DEF0,
        };
        let e = BitSet::empty();
        assert_eq!(b, b.union(e));
        assert_eq!(e, b.intersection(e));
        assert_eq!(b, b.difference(e));
    }
}
