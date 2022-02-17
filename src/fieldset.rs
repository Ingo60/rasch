#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code
#![allow(non_upper_case_globals)]

/*!
   Implement sets of fields using 64-bit integers
*/

use std::fmt::Formatter;
use std::fmt::{Debug, Display};
use std::iter::FromIterator;
use std::ops::Add;
use std::ops::Mul;
use std::ops::Not;
use std::ops::Sub;
use std::str::FromStr;

#[rustfmt::skip]
//                  Board Geometry
//      8        7        6       5         4        3       2        1
//  hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba
//  10101010 01010101 11001100 00110011 01100110 11001100 00110011 01100110

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Zone {
    EDGE, OUTER, CENTER
}
pub use Zone::*;

/// ```
/// use rasch::fieldset as F;
/// assert_eq!(F::edgeFields | F::innerFields | F::outerFields, !0u64);
/// assert_eq!(F::edgeFields & F::innerFields & F::outerFields, 0u64);
/// ```
pub const edgeFields: u64 = 0xff81_8181_8181_81ff_u64;
pub const outerFields: u64 = 0x007e_7e66_667e_7e00_u64;
pub const innerFields: u64 = 0x0000_0018_1800_0000_u64;

#[rustfmt::skip]
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

pub use Field::*;

#[rustfmt::skip]
pub const ALLFIELDS: [Field; 64] = [
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
    /// The **rank** of the field.
    ///
    /// ```
    /// use rasch::fieldset::Field::*;
    /// assert_eq!(A1.rank(), 1);
    /// assert_eq!(H1.rank(), 1);
    /// assert_eq!(A8.rank(), 8);
    /// assert_eq!(H8.rank(), 8);
    /// ```
    pub fn rank(self) -> u8 {
        1 + (self as u8 >> 3)
    }
    /// The **file** of the field.
    ///
    /// ```
    /// use rasch::fieldset::Field::*;
    /// assert_eq!(A1.file(), 'a');
    /// assert_eq!(H1.file(), 'h');
    /// assert_eq!(A8.file(), 'a');
    /// assert_eq!(H8.file(), 'h');
    /// ```
    pub fn file(self) -> char {
        (b'a' + (self as u8 & 7)) as char
    }
    /// The zone the field is part of.
    /// ```
    /// use rasch::fieldset::Field::*;
    /// use rasch::fieldset::Zone::*;
    /// assert_eq!(A1.zone(), EDGE);
    /// assert_eq!(D8.zone(), EDGE);
    /// assert_eq!(B7.zone(), OUTER);
    /// assert_eq!(E4.zone(), CENTER);
    /// ```
    pub fn zone(self) -> Zone {
        let bits = 1 << self as u64;
        if bits & edgeFields != 0 {
            EDGE
        } else if bits & outerFields != 0 {
            OUTER
        } else {
            CENTER
        }
    }
    /// ```
    /// use rasch::fieldset::Field::*;
    ///
    /// assert_eq!(E7.show(), "e7");
    /// ```
    pub fn show(self) -> String {
        self.file().to_string() + &self.rank().to_string()
    }

    /// Make a Field from rank and file
    pub fn fromFR(f: char, r: u8) -> Field {
        match f.to_ascii_lowercase() {
            'a' => match r {
                1 => A1,
                2 => A2,
                3 => A3,
                4 => A4,
                5 => A5,
                6 => A6,
                7 => A7,
                8 => A8,
                w => panic!("illegal rank {}", w),
            },
            'b' => match r {
                1 => B1,
                2 => B2,
                3 => B3,
                4 => B4,
                5 => B5,
                6 => B6,
                7 => B7,
                8 => B8,
                w => panic!("illegal rank {}", w),
            },
            'c' => match r {
                1 => C1,
                2 => C2,
                3 => C3,
                4 => C4,
                5 => C5,
                6 => C6,
                7 => C7,
                8 => C8,
                w => panic!("illegal rank {}", w),
            },
            'd' => match r {
                1 => D1,
                2 => D2,
                3 => D3,
                4 => D4,
                5 => D5,
                6 => D6,
                7 => D7,
                8 => D8,
                w => panic!("illegal rank {}", w),
            },
            'e' => match r {
                1 => E1,
                2 => E2,
                3 => E3,
                4 => E4,
                5 => E5,
                6 => E6,
                7 => E7,
                8 => E8,
                w => panic!("illegal rank {}", w),
            },
            'f' => match r {
                1 => F1,
                2 => F2,
                3 => F3,
                4 => F4,
                5 => F5,
                6 => F6,
                7 => F7,
                8 => F8,
                w => panic!("illegal rank {}", w),
            },
            'g' => match r {
                1 => G1,
                2 => G2,
                3 => G3,
                4 => G4,
                5 => G5,
                6 => G6,
                7 => G7,
                8 => G8,
                w => panic!("illegal rank {}", w),
            },
            'h' => match r {
                1 => H1,
                2 => H2,
                3 => H3,
                4 => H4,
                5 => H5,
                6 => H6,
                7 => H7,
                8 => H8,
                w => panic!("illegal rank {}", w),
            },
            _ => panic!("illegal file '{}' ({})", f, f as u32),
        }
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.show())
    }
}

impl FromStr for Field {
    type Err = String;
    /// ```
    /// use rasch::fieldset::Field;
    /// use rasch::fieldset::Field::*;
    /// use std::str::FromStr;
    ///
    /// assert_eq!(Field::from_str("d7"), Ok(D7));
    /// ```
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lower = s.to_lowercase();
        let mut iter = lower.chars();
        if let Some(c) = iter.next() {
            if c >= 'a' && c <= 'h' {
                let file = c as u32 - 'a' as u32;
                if let Some(c) = iter.next() {
                    if c >= '1' && c <= '8' {
                        let rank = c as u32 - '1' as u32;
                        match iter.next() {
                            Some(_) => Err(String::from("cannot parse field from more than 2 chars")),
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

impl From<u64> for Field {
    fn from(u: u64) -> Field {
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct BitSet {
    pub bits: u64,
}

impl BitSet {
    /// The empty BitSet
    pub const fn empty() -> BitSet {
        BitSet { bits: 0 }
    }
    /// `true` if and only if this set is empty
    pub const fn null(self) -> bool {
        self.bits == 0
    }
    /// `true` if and only if this set is not empty
    pub const fn some(self) -> bool {
        self.bits != 0
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
    /// `true` if and only if the given field is a member of this BitSet
    pub const fn member(self, m: Field) -> bool {
        self.bits & BitSet::singleton(m).bits != 0
    }
    #[inline]
    /// The set of fields that are members of this set or members of the
    /// other set
    pub const fn union(self, other: BitSet) -> BitSet {
        BitSet { bits: self.bits | other.bits }
    }
    #[inline]
    /// The set of fields that are members of both sets
    pub const fn intersection(self, other: BitSet) -> BitSet {
        BitSet { bits: self.bits & other.bits }
    }
    /// The set of fields that are mebers of this set and non-members of
    /// the other set
    #[inline]
    pub const fn difference(self, other: BitSet) -> BitSet {
        BitSet { bits: self.bits & !other.bits }
    }
    #[inline]
    /// the number of elements in this set
    pub const fn card(self) -> u32 {
        self.bits.count_ones()
    }
    /// The smallest field that is member of this set.
    ///
    /// Will panic when given the empty set.
    ///
    /// This is, in some sense, the inverse of `singleton`, becaue it is
    /// the case that `singleton(x).bitIndex() == x`
    pub fn bitIndex(self) -> Field {
        if self.bits == 0 {
            panic!("bitIndex called on empty set");
        } else {
            Field::from(self.bits.trailing_zeros() as u8)
        }
    }

    /// `true` if this is a subset of some other set
    pub fn subset(self, other: BitSet) -> bool {
        self * other == self
    }

    /// make a BitSet from a slice of Field
    /// usage: BitSet::new(&[Field::E3, Field::H7])
    /// or: BitSet::new(&v[..]) where v is a Vec<Field>
    /// (but use BitSet::empty() instead of BitSet::new(&[]) and
    /// BitSet::singleton(f) instead of BitSet::new(&[Field::D5])
    pub fn new(flds: &[Field]) -> BitSet {
        flds.iter()
            .fold(BitSet::empty(), |acc, f| acc + BitSet::singleton(*f))
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

/// usage: BitSet.from(0xFF00)
impl From<u64> for BitSet {
    fn from(bits: u64) -> BitSet {
        BitSet { bits }
    }
}

impl Iterator for BitSet {
    type Item = Field;
    fn next(&mut self) -> Option<Self::Item> {
        if self.bits == 0 {
            None
        } else {
            let u = self.bitIndex();
            self.bits ^= BitSet::singleton(u).bits;
            Some(u)
        }
    }
}

impl FromIterator<Field> for BitSet {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Field>,
    {
        let mut acc = BitSet::empty();
        for f in iter {
            // eprintln!("collect {}", f);
            acc = acc + BitSet::singleton(f);
        }
        acc
    }
}

impl<'a> FromIterator<&'a Field> for BitSet {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = &'a Field>,
    {
        let mut acc = BitSet::empty();
        for f in iter {
            // eprintln!("collect {}", *f);
            acc = acc + BitSet::singleton(*f);
        }
        acc
    }
}

impl Display for BitSet {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let v: Vec<String> = self.into_iter().map(|f| f.to_string()).collect();
        write!(f, "[{}]", &v[..].join(", "))
    }
}

impl Debug for BitSet {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let v: Vec<String> = self.into_iter().map(|f| f.to_string()).collect();
        write!(f, "[{}]", &v[..].join(", "))
    }
}

/// short form of BitSet::singleton
pub const fn bit(f: Field) -> BitSet {
    BitSet::singleton(f)
}

/// Mask the fields in the triangle A1 - D1 - D4
/// ```
/// use rasch::fieldset::{bit, A1D1D4_TRIANGLE};
/// use rasch::fieldset::BitSet;
/// use rasch::fieldset::Field::*;
/// assert_eq!(A1D1D4_TRIANGLE,         bit(D4)
///                         + bit(C3) + bit(D3)
///               + bit(B2) + bit(C2) + bit(D2)
///     + bit(A1) + bit(B1) + bit(C1) + bit(D1));
/// ```
pub const A1D1D4_TRIANGLE: BitSet = BitSet { bits: 0x0000_0000_080c_0e0f };

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
        let b = BitSet { bits: 0x1234_5678_9ABC_DEF0 };
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
            format!("{:?}", (!BitSet::empty()).into_iter().take(3).collect::<Vec<_>>())
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

    #[test]
    fn test_parse_field() {
        let result1 = "".parse::<Field>();
        let result2 = "X".parse::<Field>();
        let result3 = "d€".parse::<Field>();
        let result4 = "d9".parse::<Field>();
        let result5 = "d".parse::<Field>();
        let result6 = "a8 doesn't work".parse::<Field>();
        let result7 = "d6".parse::<Field>();
        eprintln!("{:?}", result1);
        assert!(result1.is_err());
        eprintln!("{:?}", result2);
        assert!(result2.is_err());
        eprintln!("{:?}", result3);
        assert!(result3.is_err());
        eprintln!("{:?}", result4);
        assert!(result4.is_err());
        eprintln!("{:?}", result5);
        assert!(result5.is_err());
        eprintln!("{:?}", result6);
        assert!(result6.is_err());
        assert_eq!(result7, Ok(Field::D6));
    }

    #[test]
    fn test_new_bitset() {
        assert_eq!(BitSet::empty(), BitSet::new(&[]));
        let all: Vec<Field> = BitSet::all().into_iter().collect();
        assert_eq!(BitSet::all(), BitSet::new(&all[..]));
        let a5b6c7e8 = BitSet::singleton(Field::A5)
            + BitSet::singleton(Field::B6)
            + BitSet::singleton(Field::C7)
            + BitSet::singleton(Field::E8);
        assert_eq!(
            a5b6c7e8,
            BitSet::new(&[Field::A5, Field::B6, Field::C7, Field::E8])
        );
    }

    #[test]
    fn expenisve_bitset_copy() {
        let a5b6c7e8 = BitSet::singleton(Field::A5)
            + BitSet::singleton(Field::B6)
            + BitSet::singleton(Field::C7)
            + BitSet::singleton(Field::E8);
        assert_eq!(a5b6c7e8, a5b6c7e8.into_iter().collect());
    }

    // const e5: Field = Field::E5;
    use super::Field::E5 as e5;
    // use super::Field::*;

    #[test]
    fn from_array() {
        let bs: BitSet = [e5].iter().map(|r| *r).collect();
        //                                  ^^^^^^^^^^^  this is ugly
        assert_eq!(bs, BitSet::singleton(E5));
    }

    #[test]
    fn fromFR() {
        for f in 'a'..'h' {
            for r in 1..8 {
                let fld = Field::fromFR(f, r);
                assert_eq!(fld.file(), f);
                assert_eq!(fld.rank(), r);
            }
        }
    }
}
