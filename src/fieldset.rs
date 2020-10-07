#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code

/*!
   Implement sets of fields using 64-bit integers
*/

use std::fmt::Display;
use std::fmt::Formatter;
use std::ops::Add;
use std::ops::Mul;
use std::ops::Not;
use std::ops::Sub;
use std::str::FromStr;

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

impl Field {
    pub fn rank(self) -> u8 {
        1 + (self as u8 >> 3)
    }
    pub fn file(self) -> char {
        (('a' as u8) + (self as u8 & 7)) as char
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}{}", self.file(), self.rank())
    }
}

impl FromStr for Field {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lower = s.to_lowercase();
        let mut iter = lower.chars();
        if let Some(c) = iter.next() {
            if c >= 'a' && c <= 'h' {
                let file = c as u32 - 'a' as u32;
                if let Some(c) = iter.next() {
                    if c >= '1' && c <= '9' {
                        let rank = c as u32 - '1' as u32;
                        match iter.next() {
                            Some(_) => {
                                Err(String::from("cannot parse field from more than 2 chars"))
                            }
                            None => Ok(Field::from((rank << 3) as u8 + file as u8)),
                        }
                    } else {
                        Err(String::from("cannot parse field, rank is not 1..8"))
                    }
                } else {
                    Err(String::from("cannot parse field, rank number is missing"))
                }
            } else {
                Err(String::from("cannot parse field, must start with a..h"))
            }
        } else {
            Err(String::from("cannot parse field from empty string"))
        }
    }
}

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
    /// The empty BitSet
    pub const fn empty() -> BitSet {
        BitSet { bits: 0 }
    }
    /// A BitSet that contains all fields
    pub const fn all() -> BitSet {
        BitSet { bits: !0 }
    }
    #[inline]
    /// A BitSet that contains just the given field
    pub const fn singleton(m: Field) -> BitSet {
        BitSet { bits: 1 << m as u8 }
    }
    #[inline]
    /// tue if and only if the given field is a member of this BitSet
    pub const fn member(self, m: Field) -> bool {
        self.bits & BitSet::singleton(m).bits != 0
    }
    #[inline]
    /// The set of fields that are members of this set or members of the other set
    pub const fn union(self, other: BitSet) -> BitSet {
        BitSet {
            bits: self.bits | other.bits,
        }
    }
    #[inline]
    /// The set of fields that are members of both sets
    pub const fn intersection(self, other: BitSet) -> BitSet {
        BitSet {
            bits: self.bits & other.bits,
        }
    }
    /// The set of fields that are mebers of this set and non-members of the other set
    #[inline]
    pub const fn difference(self, other: BitSet) -> BitSet {
        BitSet {
            bits: self.bits & !other.bits,
        }
    }
    #[inline]
    /// the number of elements in this set
    pub const fn card(self) -> u32 {
        self.bits.count_ones()
    }
    /// the smallest field that is member of this set
    /// will panic when given the empty set
    /// This is, in some sense, the inverse of singleton, becaue it is the case that
    /// `singleton(x).bitIndex() == x
    pub fn bitIndex(self) -> Field {
        if self.bits == 0 {
            panic!("bitIndex called on empty set");
        } else {
            Field::from(self.bits.trailing_zeros() as u8)
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

pub struct BitSetIterator {
    set: u64,
}

impl Iterator for BitSetIterator {
    type Item = Field;
    fn next(&mut self) -> Option<Self::Item> {
        if self.set == 0 {
            None
        } else {
            let u = self.set.trailing_zeros();
            self.set = self.set ^ (1u64 << u);
            Some(Field::from(u as u8))
        }
    }
}

impl IntoIterator for BitSet {
    type Item = Field;
    type IntoIter = BitSetIterator;

    fn into_iter(self) -> Self::IntoIter {
        BitSetIterator { set: self.bits }
    }
}

impl Display for BitSet {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let v: Vec<String> = self.into_iter().map(|f| f.to_string()).collect();
        write!(f, "[{}]", &v[..].join(", "))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::Rng;
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

    #[test]
    fn test_iter() {
        assert_eq!(
            "[]",
            format!("{:?}", BitSet::empty().into_iter().collect::<Vec<_>>())
        );
        assert_eq!(
            "[A1, B1, C1]",
            format!(
                "{:?}",
                (!BitSet::empty()).into_iter().take(3).collect::<Vec<_>>()
            )
        );
    }

    #[test]
    fn test_fold() {
        let mut rng = rand::thread_rng();
        let long: u64 = rng.gen();
        let set1 = BitSet::from(long);
        eprintln!("set1 = {}", set1);
        let set2 = set1
            .into_iter()
            .fold(BitSet::empty(), |acc, f| acc + BitSet::singleton(f));
        assert_eq!(set1, set2);
    }

    #[test]
    fn test_union() {
        let mut rng = rand::thread_rng();
        let long1: u64 = rng.gen();
        let long2: u64 = rng.gen();
        let set1 = BitSet::from(long1);
        let set2 = BitSet::from(long2);
        let union = set1 + set2;
        assert!(union.into_iter().all(|f| set1.member(f) || set2.member(f)));
    }

    #[test]
    fn test_intersection() {
        let mut rng = rand::thread_rng();
        let long1: u64 = rng.gen();
        let long2: u64 = rng.gen();
        let set1 = BitSet::from(long1);
        let set2 = BitSet::from(long2);
        let it = set1 * set2;
        assert!(it.into_iter().all(|f| set1.member(f) && set2.member(f)));
    }

    #[test]
    fn test_diff() {
        let mut rng = rand::thread_rng();
        let long1: u64 = rng.gen();
        let long2: u64 = rng.gen();
        let set1 = BitSet::from(long1);
        let set2 = BitSet::from(long2);
        let diff = set1 - set2;
        assert!(diff.into_iter().all(|f| set1.member(f) && !set2.member(f)));
    }

    #[test]
    fn test_bitIndex() {
        let all = BitSet::all();
        assert_eq!(64, all.card());
        for f in all.into_iter() {
            assert_eq!(f, BitSet::singleton(f).bitIndex());
        }
    }

    #[test]
    #[should_panic]
    fn test_empty_bitIndex() {
        assert!(BitSet::empty().bitIndex() == Field::A7);
    }
}
