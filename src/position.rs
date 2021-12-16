//!   # The central data structure, the `Position`
//!
//! A `Position` encodes the state of the chess board.
//!
//! * which pieces do exist and where are they placed
//! * who's turn it is
//! * the castling rights of the two players
//! * whether and where a pawn can capture en-passant
//! * the number of half moves since the last pawn move or capture

#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code
#![allow(non_upper_case_globals)] // as well as this
#![allow(non_camel_case_types)]

// use standard packages
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::vec::Vec;
use std::cmp::{min, max};
use std::cmp::Ordering;
use std::fs::File;
use std::io::{Seek, SeekFrom, Read, Write};
use std::collections::{HashMap};


// use std::boxed::Box;
// use std::iter::FromIterator;

use super::fieldset::BitSet;
use super::fieldset::Field;
use super::fieldset::Field::*;
use super::mdb;
use super::zobrist;

/// short form of BitSet::singleton
pub const fn bit(f: Field) -> BitSet { BitSet::singleton(f) }

/// short form of BitSet::bitIndex
pub fn fld(b: BitSet) -> Field { b.bitIndex() }

/// Compute p percent of v
/// ```
/// use rasch::position as P;
/// assert_eq!(P::percent(90, 200), 180);
/// ```
pub fn percent(p: i32, v: i32) -> i32 { (p*v) / 100 }

/// score when BLACK is mate
pub const blackIsMate: i32 = 32768;
/// score when WHITE is mate
pub const whiteIsMate: i32 = -blackIsMate;



#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
#[repr(u32)]
pub enum Player {
    BLACK,
    WHITE,
}

impl From<bool> for Player {
    /// if true then it's WHITE, otherwise FALSE
    fn from(b: bool) -> Player {
        if b {
            WHITE
        } else {
            BLACK
        }
    }
}

impl Player {
    /// the color of the opponent
    #[inline]
    pub fn opponent(self) -> Player {
        match self {
            Player::BLACK => Player::WHITE,
            Player::WHITE => Player::BLACK,
        }
    }

    /// compute -1 or 1 without conditional branch
    ///
    /// ```
    /// use rasch::position as P;
    /// assert_eq!(P::WHITE.factor(), 1);
    /// assert_eq!(P::BLACK.factor(), -1);
    /// ```
    #[inline]
    pub fn factor(self) -> i32 { 2 * (self as i32) - 1 }

    /// a value in favor of this player
    /// 
    /// ```
    /// use rasch::position as P;
    /// assert_eq!(P::WHITE.forP(42), 42);
    /// assert_eq!(P::BLACK.forP(42), -42);
    /// ```
    #[inline]
    pub fn forP(self, v: i32) -> i32 { self.factor() * v }

    /// penalize player on condition by n
    /// If the condition holds, count n in favor of players opponent, else 0
    pub fn penalize(self, cond: bool, n: i32) -> i32 {
        match cond {
            true => self.opponent().forP(n),
            false => 0
        }
    }
}

// pub const BLACK: Player = Player::BLACK;
// pub const WHITE: Player = Player::WHITE;
pub use Player::*;

/// Enumeration of the chess pieces
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
#[repr(u32)]
pub enum Piece {
    EMPTY,
    PAWN,
    KNIGHT,
    BISHOP,
    ROOK,
    QUEEN,
    KING,
}

impl Display for Piece {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result { write!(f, "{}", self.show()) }
}

impl From<u32> for Piece {
    /// The inverse of `piece as u32`.
    /// Will panic! if not in range 0..6
    ///
    /// ```
    /// use rasch::position::Piece;
    ///
    /// assert!((0..6).all(|u| Piece::from(u) as u32 == u));
    /// ```
    fn from(u: u32) -> Piece {
        match u {
            0 => EMPTY,
            1 => PAWN,
            2 => KNIGHT,
            3 => BISHOP,
            4 => ROOK,
            5 => QUEEN,
            6 => KING,
            _ => panic!(format!("can't cast {} to Piece", u)),
        }
    }
}

impl Piece {
    /// Produce a single letter String for a Piece: -PBNRQK
    pub fn show(self) -> String {
        String::from(match self {
            EMPTY => "-",
            PAWN => "P",
            BISHOP => "B",
            KNIGHT => "N",
            ROOK => "R",
            QUEEN => "Q",
            KING => "K",
        })
    }

    /// Encode the 3 bit information (4 in pawnSet, 2 in bishopSet, 1 in
    /// rookSet) into a Piece. Any value >6 results in EMPTY, as well as
    /// 0 (which is proper)
    pub fn encodePBR(pbr: u8) -> Piece {
        match pbr {
            0b001 => ROOK,
            0b010 => BISHOP,
            0b011 => QUEEN,
            0b100 => PAWN,
            0b101 => KING,
            0b110 => KNIGHT,
            _ => EMPTY,
        }
    }

    /// Base score for a piece in centipawns, used in evaluation
    pub fn score(self) -> i32 {
        match self {
            EMPTY => 0,
            PAWN => 100,
            KNIGHT => 300,
            BISHOP => 305,
            ROOK => 550,
            QUEEN => 875,
            KING => 1000

        }
    }

}

pub use Piece::*;

#[rustfmt::skip]
//                  Board Geometry
//      8        7        6       5         4        3       2        1
//  hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba
//  10101010 01010101 11001100 00110011 01100110 11001100 00110011 01100110

/// All the information that is needed for board positions.
///
/// Note that illegal states **are** representable, but
/// - there will be the initial position
/// - and the possibility to apply a `Move`
/// If the initial position and the application of moves is correct
/// and the move generator does indeed generate
/// valid moves only, then it should be impossible to reach an illegal
/// state.

#[derive(Clone, Copy, Debug)]
pub struct Position {
    /// The flags bitset informs about castling rights, en passant
    /// position and who's turn it is. In addition, whether the
    /// kings actually did castle, the 50-moves-rule ply counter and
    /// the distance to root ply counter.
    ///
    /// - If A1 is in the set, it is 'WHITE's turn, otherwise 'BLACK's.
    /// - If en passant is possible, one of  A3..H3 or A6..H6 is in the
    ///   set
    /// - If castling is possible, the corresponding target fields of
    ///   the Kings are in the set. This would be G1 (white kingside),
    ///   C1 (white queenside), G8 (black kingside) or C8 (black
    ///   queenside).
    /// - If castling actually was performed, F1, D1, F8 or G8 are set
    ///   (the field that the 'KING' skipped).
    /// - The bits 'A4' .. 'H4' contain the unsigned half-move counter.
    ///   It is reset to 0 on a 'PAWN' move or a capturing move. For all
    ///   other moves, it is incremented.
    /// - The bits 'A5' .. 'H5' contain the unsigned root-move counter.
    ///   It is incremented with every move, but reset to 0 after a
    ///   usermove was carried out. Hence, in searches, one can see how
    ///   many levels deep in the search we are (distance to root),
    ///   while the "depth" parameter of said search functions gives the
    ///   distance to the horizon.
    ///
    /// Note: only the bit A1, the en passant bits on ranks 3 and 6 and the 
    /// bits encoding castling rights are considered in the zobrist hash and equality checks
    pub flags: BitSet,

    /// the fields occupied by WHITE pieces
    pub whites: BitSet,

    /// For encoding of figures, we use only 3 sets instead of 6, namely
    /// 'Position.pawnSet', Position.bishopSet' and
    /// 'Position.rookSet', with the following convention:
    ///
    /// ```text
    /// Set       P      B     R
    /// PAWN      x      -     -
    /// KNIGHT    x      x     -
    /// BISHOP    -      x     -
    /// ROOK      -      -     x
    /// QUEEN     -      x     x
    /// KING      x      -     x
    /// ```
    ///
    /// For example, if the bit E5 is set in the pawnSet and the
    /// rookSet, but not in the bishopSet, then there is a KING on
    /// E5. If the E5 flag is set in whites, then it is a white
    /// king, else a black king.
    pub pawnSet:   BitSet,
    pub bishopSet: BitSet,
    pub rookSet:   BitSet,

    /// the Zobrist hash key, a special hashing method usually employed
    /// in chess programming
    ///
    /// The following is guaranteed:
    ///
    /// ```text
    /// x.hash != y.hash → x != y
    /// x == y           → x.hash == y.hash
    /// ```
    pub hash: u64,
}

/// Bitmask that lists the fields where `PAWN`s are allowed.
/// They aren't on ranks 1 and 8.
pub const pawnFields: BitSet = BitSet { bits: 0x00ff_ffff_ffff_ff00u64 };

/// Bitmask for selection of the ply (half-move) counter, which is used to support the 50 moves rule.
/// Note that we have room for 256 plies, but only 100 are needed.
///
/// The evaluator will return 0 for a position where the ply counter is 100 or more.
/// Also, the protocol handler will claim a draw when the move played results in a position
/// with ply counter 100 or more.
/// 
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// assert!(P::plyCounterBits.member(F::Field::A4));
/// assert!(P::plyCounterBits.member(F::Field::B4));
/// assert!(P::plyCounterBits.member(F::Field::C4));
/// assert!(P::plyCounterBits.member(F::Field::D4));
/// assert!(P::plyCounterBits.member(F::Field::E4));
/// assert!(P::plyCounterBits.member(F::Field::F4));
/// assert!(P::plyCounterBits.member(F::Field::G4));
/// assert!(P::plyCounterBits.member(F::Field::H4));
/// assert_eq!(P::plyCounterBits.card(), 8);
/// ```
#[rustfmt::skip]
pub const plyCounterBits: BitSet = BitSet { bits: 0xFF00_0000u64 }; // A4..H4

/// how many bits to shift right to get the ply counter
/// ```
/// use rasch::position as P;
/// assert_eq!(P::plyCounterBits.bits >> P::plyCounterShift, 255);
/// ```
pub const plyCounterShift: u32 = 24;

/// Bitmask for selection of the distance to root ply counter
/// This counter is reset to 0 before a seach starts and incremented with every move
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// assert!(P::rootCounterBits.member(F::Field::A5));
/// assert!(P::rootCounterBits.member(F::Field::B5));
/// assert!(P::rootCounterBits.member(F::Field::C5));
/// assert!(P::rootCounterBits.member(F::Field::D5));
/// assert!(P::rootCounterBits.member(F::Field::E5));
/// assert!(P::rootCounterBits.member(F::Field::F5));
/// assert!(P::rootCounterBits.member(F::Field::G5));
/// assert!(P::rootCounterBits.member(F::Field::H5));
/// assert_eq!(P::rootCounterBits.card(), 8);
/// ```
#[rustfmt::skip]
pub const rootCounterBits: BitSet = BitSet { bits: 0xFF_0000_0000 }; // A5..H5

/// number of bits to shift right to get the ply counter
/// ```
/// use rasch::position as P;
/// assert_eq!(P::rootCounterBits.bits >> P::rootCounterShift, 255);
/// ```
pub const rootCounterShift: u32 = 32;

/// A bitmask used to turn all the counter bits off
///
/// ```
/// use rasch::position as P;
/// assert_eq!(P::counterBits, P::rootCounterBits + P::plyCounterBits);
/// ```
pub const counterBits: BitSet = BitSet {
    bits: 0x0000_00FF_FF00_0000,
};

#[rustfmt::skip]
/// Bitmask for selection of the bits that indicate the rights to castle
/// 
/// ```
/// use rasch::position as P;
/// assert_eq!(P::castlingBits, P::whiteCastlingRights + P::blackCastlingRights)
/// ```
pub const castlingBits: BitSet = BitSet { bits: 0x4400_0000_0000_0044 }; // G1, C1, G8, C8

#[rustfmt::skip]
/// Bitmask for selection of the bits that indicate the rights to castle for WHITE
/// 
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// assert!   (P::whiteCastlingRights.member(F::Field::G1));
/// assert!   (P::whiteCastlingRights.member(F::Field::C1));
/// assert_eq!(P::whiteCastlingRights.card(), 2);
/// ```
pub const whiteCastlingRights: BitSet = BitSet { bits: 0x0000_0000_0000_0044 }; // G1, C1

#[rustfmt::skip]
/// Bitmask for selection of the bits that indicate the rights to castle for BLACK
/// 
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// assert!   (P::blackCastlingRights.member(F::Field::G8));
/// assert!   (P::blackCastlingRights.member(F::Field::C8));
/// assert_eq!(P::blackCastlingRights.card(), 2);
/// ```
pub const blackCastlingRights: BitSet = BitSet { bits: 0x4400_0000_0000_0000 }; // G8, C8

#[rustfmt::skip]
/// Bitmask for selection of the bits that indicate the fields that were skipped when the king castled
/// 
/// ```
/// use rasch::position as P;
/// assert_eq!(P::castlingDoneBits, P::whiteHasCastledBits + P::blackHasCastledBits)
/// ```
pub const castlingDoneBits: BitSet = BitSet { bits: 0x2800_0000_0000_0028 }; // F1, D1, F8, D8

#[rustfmt::skip]
/// Bitmask for selection of the bits that tell us whether the white king has castled and whereto.
///
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// assert!   (P::whiteHasCastledBits.member(F::Field::F1));
/// assert!   (P::whiteHasCastledBits.member(F::Field::D1));
/// assert_eq!(P::whiteHasCastledBits.card(), 2);
/// ```
pub const whiteHasCastledBits: BitSet = BitSet { bits: 0x0000_0000_0000_0028 }; // F1, D1

#[rustfmt::skip]
/// Bitmask for selection of the bits that tell us whether the black king has castled and whereto.
/// 
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// assert!   (P::blackHasCastledBits.member(F::Field::F8));
/// assert!   (P::blackHasCastledBits.member(F::Field::D8));
/// assert_eq!(P::blackHasCastledBits.card(), 2);
/// ```
pub const blackHasCastledBits: BitSet = BitSet { bits: 0x2800_0000_0000_0000 }; // F8, D8

/// give the bitmask that can be used to find out whether a given player
/// has castled
pub const fn playerCastledBits(p: Player) -> BitSet {
    match p {
        Player::BLACK => blackHasCastledBits,
        Player::WHITE => whiteHasCastledBits,
    }
}


/// Bitmask to select the Bits that can be targets of an en-passant capture (rank 3 and rank 6).
/// 
/// If any of those is set in the position flags, an en passant capture is possible.
/// 
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// use rasch::fieldset::Field::*;
/// assert!   (P::enPassantBits.member(A3));
/// assert!   (P::enPassantBits.member(B3));
/// assert!   (P::enPassantBits.member(C3));
/// assert!   (P::enPassantBits.member(D3));
/// assert!   (P::enPassantBits.member(E3));
/// assert!   (P::enPassantBits.member(F3));
/// assert!   (P::enPassantBits.member(G3));
/// assert!   (P::enPassantBits.member(H3));
/// assert!   (P::enPassantBits.member(A6));
/// assert!   (P::enPassantBits.member(B6));
/// assert!   (P::enPassantBits.member(C6));
/// assert!   (P::enPassantBits.member(D6));
/// assert!   (P::enPassantBits.member(E6));
/// assert!   (P::enPassantBits.member(F6));
/// assert!   (P::enPassantBits.member(G6));
/// assert!   (P::enPassantBits.member(H6));
/// assert_eq!(P::enPassantBits.card(), 16);
/// ```
pub const enPassantBits: BitSet = BitSet { bits: 0x0000_FF00_00FF_0000 }; // A3-H3, A6-H6

/// Bitmask to select the white pawns that may block an unmoved bishop.
/// Evaluation will penalize if those pawns are blocked themselves as this makes it
/// extremely unlikely that the bishops can be developed. 
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// use rasch::fieldset::Field::*;
/// assert!   (P::whiteBishopBlockingPawns.member(B2));
/// assert!   (P::whiteBishopBlockingPawns.member(D2));
/// assert!   (P::whiteBishopBlockingPawns.member(E2));
/// assert!   (P::whiteBishopBlockingPawns.member(G2));
/// assert_eq!(P::whiteBishopBlockingPawns.card(), 4);
/// ```
pub const whiteBishopBlockingPawns: BitSet = BitSet { bits: 0x0000_0000_0000_5A00 };

/// Bitmask to select the black pawns that may block an unmoved bishop.
/// Evaluation will penalize if those pawns are blocked themselves as this makes it
/// extremely unlikely that the bishops can be developed. 
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// use rasch::fieldset::Field::*;
/// assert!   (P::blackBishopBlockingPawns.member(B7));
/// assert!   (P::blackBishopBlockingPawns.member(D7));
/// assert!   (P::blackBishopBlockingPawns.member(E7));
/// assert!   (P::blackBishopBlockingPawns.member(G7));
/// assert_eq!(P::blackBishopBlockingPawns.card(), 4);
/// ```
pub const blackBishopBlockingPawns: BitSet = BitSet { bits: 0x005A_0000_0000_0000 };

/// Bitmask to select the fields where white officers start out.
/// Evaluation will penalize if those fields are still occupied by officers.
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// use rasch::fieldset::Field::*;
/// assert!   (P::whiteOfficers.member(B1));
/// assert!   (P::whiteOfficers.member(C1));
/// assert!   (P::whiteOfficers.member(F1));
/// assert!   (P::whiteOfficers.member(G1));
/// assert_eq!(P::whiteOfficers.card(), 4);
/// ```
pub const whiteOfficers: BitSet = BitSet { bits: 0x0000_0000_0000_0066 };

/// Bitmask to select the fields where black officers start out.
/// Evaluation will penalize if those fields are still occupied by officers.
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// use rasch::fieldset::Field::*;
/// assert!   (P::blackOfficers.member(B8));
/// assert!   (P::blackOfficers.member(C8));
/// assert!   (P::blackOfficers.member(F8));
/// assert!   (P::blackOfficers.member(G8));
/// assert_eq!(P::blackOfficers.card(), 4);
/// ```
pub const blackOfficers: BitSet = BitSet { bits: 0x6600_0000_0000_0000 };


/// constant to add or subtract 1 from both counters in one go
/// It goes without saying that we must never decrement beyond zero, nor
/// increment beyond 255.
pub const onePly: u64 = 0x1_0100_0000; // A4 and A5

/// This is used when only the root counter must be incremented (on
/// 'PAWN' moves and captures)
pub const onePlyRootOnly: u64 = 0x1_0000_0000; // A5

/// Bit mask that indicates WHITE is to move
pub const whiteToMove: BitSet = bit(A1);
/// Bit mask that indicates BLACK is to move
pub const blackToMove: BitSet = BitSet::empty();

// Zobrist hash value commonly used: A1
// static flagZobristA1: u64 = zobrist::flagZobrist(A1 as u32);
// Zobrist hash value commonly used: C1
// static flagZobristC1: u64 = zobrist::flagZobrist(C1 as u32);
// Zobrist hash value commonly used: G1
// static flagZobristG1: u64 = zobrist::flagZobrist(G1 as u32);
// Zobrist hash value commonly used: C8
// static flagZobristC8: u64 = zobrist::flagZobrist(C8 as u32);
// Zobrist hash value commonly used: G8
// static flagZobristG8: u64 = zobrist::flagZobrist(G8 as u32);

/// Move sequences for castlings consist of 3 moves to get the "who is
/// to move" bit right. Castling rights are taken care of by the
/// ordinary move logic. Since this happens maybe once in a game,
/// efficiency is less important.
pub const whiteShortCastlingMove1: Move = Move::new(WHITE, KING, EMPTY, E1, F1);
pub const whiteShortCastlingMove2: Move = Move::new(WHITE, KING, EMPTY, F1, G1);
pub const whiteShortCastlingMove3: Move = Move::new(WHITE, ROOK, EMPTY, H1, F1);
pub const whiteLongCastlingMove1: Move = Move::new(WHITE, KING, EMPTY, E1, D1);
pub const whiteLongCastlingMove2: Move = Move::new(WHITE, KING, EMPTY, D1, C1);
pub const whiteLongCastlingMove3: Move = Move::new(WHITE, ROOK, EMPTY, A1, D1);
pub const blackShortCastlingMove1: Move = Move::new(BLACK, KING, EMPTY, E8, F8);
pub const blackShortCastlingMove2: Move = Move::new(BLACK, KING, EMPTY, F8, G8);
pub const blackShortCastlingMove3: Move = Move::new(BLACK, ROOK, EMPTY, H8, F8);
pub const blackLongCastlingMove1: Move = Move::new(BLACK, KING, EMPTY, E8, D8);
pub const blackLongCastlingMove2: Move = Move::new(BLACK, KING, EMPTY, D8, C8);
pub const blackLongCastlingMove3: Move = Move::new(BLACK, ROOK, EMPTY, A8, D8);

/// kingside castling for WHITE
pub const castlingShortWhite: Move = Move::new(WHITE, KING, KING, E1, G1);
pub const castlingLongWhite: Move = Move::new(WHITE, KING, QUEEN, E1, C1);
pub const castlingShortBlack: Move = Move::new(BLACK, KING, KING, E8, G8);
pub const castlingLongBlack: Move = Move::new(BLACK, KING, QUEEN, E8, C8);

/// Helper function that turns `false` into `0u64` and `true` into
/// `0xffff_ffff_ffff_ffffu64`
#[inline]
pub const fn boolMask(b: bool) -> u64 {
    match b {
        false => 0,
        true => 0xffff_ffff_ffff_ffffu64,
    }
}

/// Give the set of fields that are targeted by some piece
pub fn pieceTargets(piece: Piece, player: Player, from: Field) -> BitSet {
    match piece {
        PAWN => match player {
            WHITE => mdb::whitePawnTargets(from),
            BLACK => mdb::blackPawnTargets(from),
        },
        KNIGHT => mdb::knightTargets(from),
        BISHOP => mdb::bishopTargets(from),
        ROOK => mdb::rookTargets(from),
        QUEEN => mdb::rookTargets(from) + mdb::bishopTargets(from),
        KING => mdb::kingTargets(from),
        EMPTY => BitSet::empty(),
    }
}

impl Position {
    /// the set of fields that are occupied by PAWNS
    pub fn pawns(&self) -> BitSet { (self.pawnSet - self.bishopSet) - self.rookSet }

    /// the set of fields that are occupied by KNIGHTS
    pub fn knights(&self) -> BitSet { (self.pawnSet * self.bishopSet) - self.rookSet }

    /// the set of fields that are occupied by BISHOPS
    pub fn bishops(&self) -> BitSet { (self.bishopSet - self.pawnSet) - self.rookSet }

    /// the set of fields that are occupied by ROOKS
    pub fn rooks(&self) -> BitSet { (self.rookSet - self.bishopSet) - self.pawnSet }

    /// the set of fields that are occupied by QUEENS
    pub fn queens(&self) -> BitSet { (self.rookSet * self.bishopSet) - self.pawnSet }

    /// the set of fields that are occupied by KINGS
    pub fn kings(&self) -> BitSet { (self.pawnSet * self.rookSet) - self.bishopSet }

    /// get the number of `Move`s applied since the last pawn move or
    /// capture (Castling, despite technically doing 3 moves,
    /// corrects the counter acordingly)
    pub fn getPlyCounter(&self) -> u64 { (self.flags * plyCounterBits).bits >> plyCounterShift }

    /// get the number of `Move`s applied since the last root counter
    /// reset
    pub fn getRootDistance(&self) -> u64 { (self.flags * rootCounterBits).bits >> rootCounterShift }

    /// clear the 50-move ply counter
    pub fn clearPlyCounter(&self) -> Position {
        Position {
            flags: self.flags - plyCounterBits,
            ..*self
        }
    }

    /// clear the root ply counter
    pub fn clearRootPlyCounter(&self) -> Position {
        Position {
            flags: self.flags - rootCounterBits,
            ..*self
        }
    }

    /// Increment the ply counter(s) using either `onePly` or
    /// `onePlyRootOnly`
    pub fn incrPlyCounters(&self, mask: u64) -> Position {
        Position {
            flags: BitSet {
                bits: self.flags.bits + mask,
            },
            ..*self
        }
    }

    /// Decrement both counters
    /// Note: it will never be necessary to decrement only one counter.
    /// Only used after castling, where we actually do 3 ordinary (but
    /// illegal) moves.
    pub fn decrPlyCounters(&self) -> Position {
        Position {
            flags: BitSet {
                bits: self.flags.bits - onePly,
            },
            ..*self
        }
    }

    /// Subtract 2 from both plyCounters und set the ply counter to 0, as castling is irreversible, too
    /// (conveniece for castlings)
    pub fn correctPlyCounterForCastling(&self) -> Position { self.decrPlyCounters().decrPlyCounters().clearPlyCounter() }

    /// the set of occupied fields
    pub fn occupied(&self) -> BitSet { self.pawnSet + self.bishopSet + self.rookSet }

    /// `true` if and only if the given `Field` is not occupied by some
    /// piece
    pub fn isEmpty(&self, f: Field) -> bool { !self.occupied().member(f) }

    /// `true` if and only if no member of the given set is an occupied
    /// field
    pub fn areEmpty(&self, fs: BitSet) -> bool { (self.occupied() * fs).null() }

    /// `true` if and only if the given `Field` is occupied by WHITE
    ///
    /// Note that `false` does **not** imply it's occupied by BLACK, it
    /// could just be empty. But if you already know that the field
    /// **is** occupied, this'll save a few cycles consulting the
    /// pawn, rook and bishop sets.
    pub fn isWhite(&self, f: Field) -> bool { self.whites.member(f) }

    /// tell who's turn it is
    pub fn turn(&self) -> Player {
        if self.flags.member(A1) {
            WHITE
        } else {
            BLACK
        }
    }

    /// fields occupied by WHITE
    pub fn occupiedByWhite(&self) -> BitSet { self.whites }

    /// fields occupied by BLACK
    pub fn occupiedByBlack(&self) -> BitSet { self.occupied() - self.whites }

    /// fields occupied by Player
    pub fn occupiedBy(&self, p: Player) -> BitSet {
        match p {
            Player::WHITE => self.occupiedByWhite(),
            Player::BLACK => self.occupiedByBlack(),
        }
    }

    /// fields occupied by the player who's turn it is
    pub fn occupiedByActive(&self) -> BitSet { self.occupiedBy(self.turn()) }

    /// Tell us the piece that occupies some Field.
    #[allow(clippy::many_single_char_names)] // because we are no snowflakes
    pub fn pieceOn(&self, f: Field) -> Piece {
        let s = bit(f);
        let p = if (s * self.pawnSet).null() { 0 } else { 4 };
        let b = if (s * self.bishopSet).null() { 0 } else { 2 };
        let r = if (s * self.rookSet).null() { 0 } else { 1 };
        Piece::encodePBR(p + b + r)
    }

    /// Tell us the player occupying some Field.
    pub fn playerOn(&self, f: Field) -> Option<Player> {
        if self.occupied().member(f) {
            Some(Player::from(self.isWhite(f)))
        } else {
            None
        }
    }

    /// compute the hash
    pub fn computeZobrist(&self) -> u64 {
        let flagz = ((self.flags - counterBits) - castlingDoneBits)
            // .into_iter()
            .fold(0u64, |acc, f| acc ^ super::zobrist::flagZobrist(f as usize));
        self.occupied()
            // .into_iter()
            .fold(flagz, |acc, f| {
                let player = Player::from(self.isWhite(f));
                let piece = self.pieceOn(f);
                acc ^ zobrist::ppfZobrist(player as usize, piece as usize, f as usize)
            })
    }

    /// rehash the Position, will be done once with each move
    ///
    /// ```
    /// use rasch::mdb;
    /// use rasch::position as P;
    /// use rasch::fieldset::Field::*;
    /// use rasch::position::Player::*;
    /// use rasch::position::Piece::*;
    /// mdb::initStatic();
    /// let p = P::initialBoard();
    /// assert_eq!(0xc49244f1909c1fabu64, p.hash);
    /// let m = P::Move::new(WHITE, PAWN, EMPTY, E2, E4);
    /// let p2 = p.apply(m);
    /// let p3 = p2.rehash();
    /// assert_eq!(p2.hash, p3.hash);
    /// ```
    pub fn rehash(&self) -> Position {
        Position {
            hash: self.computeZobrist(),
            ..*self
        }
    }

    /// Like `attacked`, but reports attacks by `PAWN`s only. O(1)
    pub fn attackedByPawns(&self, wo: Field, durch: Player) -> BitSet {
        let attackers = match durch {
            WHITE => mdb::targetOfWhitePawns(wo),
            BLACK => mdb::targetOfBlackPawns(wo),
        };
        match durch {
            WHITE => (self.pawns() * attackers) * self.whites,
            BLACK => (self.pawns() * attackers) - self.whites,
        }
    }

    /// Like `attacked`, but reports attacks by `KNIGHT`s only. O(1)
    pub fn attackedByKnights(&self, wo: Field, durch: Player) -> BitSet {
        let attackers = self.knights() * mdb::knightTargets(wo);
        match durch {
            WHITE => (self.knights() * attackers) * self.whites,
            BLACK => (self.knights() * attackers) - self.whites,
        }
    }

    /// Like `attacked`, but reports attacks by `KINGS`s only. O(1)
    pub fn attackedByKings(&self, wo: Field, durch: Player) -> BitSet {
        let attackers = self.kings() * mdb::kingTargets(wo);
        match durch {
            WHITE => (self.kings() * attackers) * self.whites,
            BLACK => (self.kings() * attackers) - self.whites,
        }
    }

    /// Like `attacked`, but reports attacks by `BISHOP`s only. Needs 1
    /// iteration through attacker's bishops
    pub fn attackedByBishops(&self, wo: Field, durch: Player) -> BitSet {
        let bishops = match durch {
            // find the attacker's bishops
            WHITE => self.bishops() * self.whites,
            BLACK => self.bishops() - self.whites,
        };
        self.validBishopTargets(wo, bishops)
    }

    /// Like `attacked`, but reports attacks by `ROOK`s only. Needs 1
    /// iteration through attacker's rooks.
    pub fn attackedByRooks(&self, wo: Field, durch: Player) -> BitSet {
        let rooks = match durch {
            // find the attacker's rooks
            WHITE => self.rooks() * self.whites,
            BLACK => self.rooks() - self.whites,
        };
        self.validRookTargets(wo, rooks)
    }

    /// Like `attacked`, but reports attacks by `QUEEN`s only. Needs 2
    /// iterations through attacker's queens.
    pub fn attackedByQueens(&self, wo: Field, durch: Player) -> BitSet {
        let queens = match durch {
            // find the attacker's queens
            WHITE => self.queens() * self.whites,
            BLACK => self.queens() - self.whites,
        };
        self.validRookTargets(wo, queens) + self.validBishopTargets(wo, queens)
    }

    /// Like `attackedByBishops` but reports also QUEENs  attacking with
    /// BISHOP moves. Needs 1 iteration through attacker's queens and
    /// bishops.
    pub fn attackedByBishopsOrQueens(&self, wo: Field, durch: Player) -> BitSet {
        let pieces = match durch {
            // find the attacker's bishops and queens
            WHITE => (self.bishops() + self.queens()) * self.whites,
            BLACK => (self.bishops() + self.queens()) - self.whites,
        };
        self.validBishopTargets(wo, pieces)
    }

    /// Like `attackedByRooks` but reports also QUEENs  attacking with
    /// ROOK moves. Needs 1 iteration through attacker's queens and
    /// rooks.
    pub fn attackedByRooksOrQueens(&self, wo: Field, durch: Player) -> BitSet {
        let pieces = match durch {
            // find the attacker's rooks and queens
            WHITE => (self.rooks() + self.queens()) * self.whites,
            BLACK => (self.rooks() + self.queens()) - self.whites,
        };
        self.validRookTargets(wo, pieces)
    }

    /// Tell if the indicated field is attacked by a piece of the
    /// indicated player. This is faster than
    ///
    /// ```text
    ///     !position.attacked(f,p).null()
    /// ```
    ///
    /// because it can short circuit whenever an attacker is found. And
    /// of course it tries the pieces first that have an O(1) time
    /// complexity. In the worst case, when the attack is from a ROOK or
    /// QUEEN it'll need 2 iterations.
    pub fn isAttacked(&self, wo: Field, durch: Player) -> bool { !self.isNotAttacked(wo, durch) }

    /// the opposite of isAttacked
    pub fn isNotAttacked(&self, wo: Field, durch: Player) -> bool {
        self.attackedByPawns(wo, durch).null()
            && self.attackedByKnights(wo, durch).null()
            && self.attackedByKings(wo, durch).null()
            && self.attackedByBishopsOrQueens(wo, durch).null()
            && self.attackedByRooksOrQueens(wo, durch).null()
    }

    /// Given a `Position`, a `Field` and a `Player` computes the set of
    /// fields that are occupied  by a `Piece` of `Player`
    /// that could move to the given `Field` (and capture).
    ///
    /// Note that computation for `QUEEN`, `ROOK` and `BISHOP` attackers
    /// require two iterations. Therefore, whenever one just needs
    /// to know *whether* the `Field` is attacked or not, use
    /// `isAttacked`
    ///
    /// ```
    /// use rasch::mdb;
    /// use rasch::position as P;
    /// use rasch::fieldset as F;
    /// use rasch::fieldset::Field::*;
    /// use rasch::position::Player::*;
    /// use rasch::position::Piece::*;
    /// mdb::initStatic();
    /// let p = P::initialBoard();
    /// eprintln!("should be {}", p.attacked(C3, WHITE));
    /// assert_eq!(p.attacked(C3, WHITE), [B1, B2, D2].iter().collect());
    /// ```

    pub fn attacked(&self, wo: Field, durch: Player) -> BitSet {
        self.attackedByPawns(wo, durch)
            + self.attackedByKnights(wo, durch)
            + self.attackedByKings(wo, durch)
            + self.attackedByBishopsOrQueens(wo, durch)
            + self.attackedByRooksOrQueens(wo, durch)
    }

    /// Tell if `Players` KING stands in check.
    pub fn inCheck(&self, wer: Player) -> bool {
        let king = match wer {
            WHITE => self.kings() * self.whites,
            BLACK => self.kings() - self.whites,
        };
        // an invalid position without kings will crash fld()
        self.isAttacked(fld(king), wer.opponent())
    }

    /// Tell if current position is ok in that the **passive** player is
    /// not in check. Can be used after having applied a user's move
    /// or a speculative move to see if it was possible at all.
    ///
    /// ```
    /// use rasch::mdb;
    /// use rasch::position as P;
    /// mdb::initStatic();
    /// assert!(P::initialBoard().notInCheck());
    /// ```
    pub fn notInCheck(&self) -> bool {
        let player = self.turn();
        let otherking = match player {
            WHITE => self.kings() - self.whites,
            BLACK => self.kings() * self.whites,
        };
        // an invalid position without kings will crash fld()
        self.isNotAttacked(fld(otherking), player)
    }

    /// Where does a BISHOP on a given field attack in the current
    /// position, taking actual board status in account? Result can
    /// be restricted to certain set of fields to save iteration
    /// cycles or all results can be kept using BitSet::all()
    ///
    /// To find this out, we must loop through the set of general
    /// targets (restricted by the mask) and check if the
    /// intermediate fields, if any, are all empty.
    ///
    /// Note that the current position must not actually have a BISHOP
    /// on the from field. Indeed, this function can be used in
    /// determining whether a certain field is attacked by a BISHOP.
    /// This is so because BISHOP moves are symmetric. That is, if a
    /// BISHOP standing on the from field could attack a BISHOP
    /// somewhere this means the from field is also attacked by that
    /// BISHOP.
    pub fn validBishopTargets(&self, from: Field, mask: BitSet) -> BitSet {
        let targets = mdb::bishopTargets(from) * mask;
        // chances are we can avoid all the iteration overhead
        if targets.null() {
            targets
        } else {
            targets.filter(|to| self.areEmpty(mdb::canBishop(from, *to))).collect()
        }
    }

    /// like `validBishopTargets` but for ROOKs
    pub fn validRookTargets(&self, from: Field, mask: BitSet) -> BitSet {
        let targets = mdb::rookTargets(from) * mask;
        if targets.null() {
            targets
        } else {
            targets.filter(|to| self.areEmpty(mdb::canRook(from, *to))).collect()
        }
    }

    /// Give the least attacking piece, if any
    pub fn leastAttacker(&self, wo: Field, durch: Player) -> Piece {
        match () {
            () if self.attackedByPawns(wo, durch).some() => PAWN,
            () if self.attackedByKnights(wo, durch).some() => KNIGHT,
            () if self.attackedByBishops(wo, durch).some() => BISHOP,
            () if self.attackedByRooks(wo, durch).some() => ROOK,
            () if self.attackedByQueens(wo, durch).some() => QUEEN,
            () if self.attackedByKings(wo, durch).some() => KING,
            () => EMPTY,
        }
    }

    /// Apply a move in a position and return the resulting position.
    /// The move should be one of the moves generated for this position.
    pub fn apply(&self, mv: Move) -> Position {
        /* eprintln!(
            "applying {:?} {:?} {:?} {} {}",
            mv.player(),
            mv.piece(),
            mv.promote(),
            mv.from(),
            mv.to()
        );
        eprintln!("to {}", self); */
        if mv.piece() == KING && mv.promote() != EMPTY {
            self.applyCastling(mv)
        } else {
            self.applyOrdinary(mv)
        }
    }

    /// Helper function to place pieces on a board, removing any that
    /// occupy the indicated fields. Can be given EMPTY to remove.
    ///
    /// Not for public use as it leaves the hash in an
    /// illegal state.
    pub fn place(&self, player: Player, piece: Piece, mask: BitSet) -> Position {
        let whites = match player {
            WHITE if piece != EMPTY => self.whites + mask,
            _other => self.whites - mask,
        };
        let pawnSet = match piece {
            PAWN | KING | KNIGHT => self.pawnSet + mask,
            _other => self.pawnSet - mask,
        };
        let bishopSet = match piece {
            BISHOP | QUEEN | KNIGHT => self.bishopSet + mask,
            _other => self.bishopSet - mask,
        };
        let rookSet = match piece {
            ROOK | QUEEN | KING => self.rookSet + mask,
            _other => self.rookSet - mask,
        };
        Position {
            whites,
            pawnSet,
            bishopSet,
            rookSet,
            ..*self
        }
    }

    /// Helper to set a certain bit in flags
    pub fn setFlag(&self, f: Field) -> Position {
        Position {
            flags: self.flags + bit(f),
            ..*self
        }
    }

    /// Helper to unset a bunch of flags
    pub fn unsetFlags(&self, flgs: BitSet) -> Position {
        Position {
            flags: self.flags - flgs,
            ..*self
        }
    }

    /// Apply a castling move. Used only by `apply`
    fn applyCastling(&self, mv: Move) -> Position {
        let castled = match mv.player() {
            WHITE => match mv.promote() {
                KING => self
                    .applyOrdinary(whiteShortCastlingMove1)
                    .applyOrdinary(whiteShortCastlingMove2)
                    .applyOrdinary(whiteShortCastlingMove3)
                    .setFlag(F1),
                _other => self
                    .applyOrdinary(whiteLongCastlingMove1)
                    .applyOrdinary(whiteLongCastlingMove2)
                    .applyOrdinary(whiteLongCastlingMove3)
                    .setFlag(D1),
            },
            BLACK => match mv.promote() {
                KING => self
                    .applyOrdinary(blackShortCastlingMove1)
                    .applyOrdinary(blackShortCastlingMove2)
                    .applyOrdinary(blackShortCastlingMove3)
                    .setFlag(F8),
                _other => self
                    .applyOrdinary(blackLongCastlingMove1)
                    .applyOrdinary(blackLongCastlingMove2)
                    .applyOrdinary(blackLongCastlingMove3)
                    .setFlag(D8),
            },
        };
        castled.correctPlyCounterForCastling()
    }

    /// Apply an ordinary move. Used only by `apply`
    fn applyOrdinary(&self, mv: Move) -> Position {
        let to = mv.to();
        let from = mv.from();
        // construct the part of the flag that tells who's move it is
        let tomove = BitSet {
            bits: (self.flags * whiteToMove).bits ^ whiteToMove.bits,
        };
        // Construct the counter part of the flags.
        let plies = match mv.piece() {
            PAWN if self.isEmpty(to) => BitSet {
                bits: ((self.flags * counterBits) - plyCounterBits).bits + onePlyRootOnly,
            },
            _capture if !self.isEmpty(to) => BitSet {
                bits: ((self.flags * counterBits) - plyCounterBits).bits + onePlyRootOnly,
            },
            _otherwise => BitSet {
                bits: (self.flags * counterBits).bits + onePly,
            },
        };
        // LCR means "lost castling rights"
        // the castling rights that are lost if this was a KING move
        let kingMoveLCR = match mv.piece() {
            KING => match mv.player() {
                WHITE => whiteCastlingRights,
                BLACK => blackCastlingRights,
            },
            _otherwise => BitSet::empty(),
        };
        // the castling rights that are lost if this was a ROOK move
        // it suffices to check for the from positions, since the same rights
        // are also long lost if there is something else on the ROOK fields
        let rookMoveLCR = match from {
            A8 => bit(C8),
            H8 => bit(G8),
            A1 => bit(C1),
            H1 => bit(G1),
            _other => BitSet::empty(),
        };
        // the castling rights lost if the target of the move if an original
        // ROOK field Again, it matters not if the original ROOK is
        // standing there or not, the rights are lost anyway.
        let rookCaptureLCR = match to {
            A8 => bit(C8),
            H8 => bit(G8),
            A1 => bit(C1),
            H1 => bit(G1),
            _other => BitSet::empty(),
        };
        // All lost castling rights in this move.
        // For example, moving the rook from a8 to a1 would loose C8 and C1
        let lostCastlingRights = kingMoveLCR + rookMoveLCR + rookCaptureLCR;
        // The new castling rights are the difference between the maybe still
        // existing castling rights and the lost castling rights
        let castlingRights = (self.flags * castlingBits) - lostCastlingRights;
        // en passant bit to set, if any
        let enPassantBit = match mv.piece() {
            PAWN if from.rank() == 2 && to as u8 == from as u8 + 16 => bit(Field::from(from as u8 + 8)),
            PAWN if from.rank() == 7 && to as u8 == from as u8 - 16 => bit(Field::from(from as u8 - 8)),
            _otherwise => BitSet::empty(),
        };
        // the new castled flag to set, if any
        // not needed as we do castling in applyCastling
        // let gainedCastledBit = match mv.piece() {
        //     KING => match mv.from() {
        //         E1 => match mv.to() {
        //             G1 => bit(F1),
        //             C1 => bit(D1),
        //             _otherwise => BitSet::empty(),
        //         },
        //         E8 => match mv.to() {
        //             G8 => bit(F8),
        //             C8 => bit(D8),
        //             _otherwise => BitSet::empty(),
        //         },
        //         _otherwise => BitSet::empty(),
        //     },
        //     _otherwise => BitSet::empty(),
        // };

        // The new part of flags that tells who has castled already.
        let hasCastledFlags = self.flags * castlingDoneBits; // + gainedCastledBit;
                                                             // Fields that will be empty after this move.
        let fromMask = bit(from)
            + match mv.promote() {
                // compute field of piece captured by en passant
                PAWN if mv.player() == WHITE => bit(Field::from(to as u8 - 8)),
                PAWN if mv.player() == BLACK => bit(Field::from(to as u8 + 8)),
                _otherwise => BitSet::empty(),
            };
        // The following must be xor'ed out from the hash
        let fromHash = zobrist::ppfZobrist(mv.player() as usize, mv.piece() as usize, from as usize)
            ^ match mv.promote() {
                // compute zobrist of captured piece
                PAWN if mv.player() == WHITE => {
                    zobrist::ppfZobrist(mv.player().opponent() as usize, PAWN as usize, to as usize - 8)
                }
                PAWN if mv.player() == BLACK => {
                    zobrist::ppfZobrist(mv.player().opponent() as usize, PAWN as usize, to as usize + 8)
                }
                _otherwise => 0,
            };
        // field that will be set after the move
        let toMask = bit(to);
        // the value that must be xor'ed out of the hash
        let toHash = match self.pieceOn(to) {
            EMPTY => 0,
            p => zobrist::ppfZobrist(mv.player().opponent() as usize, p as usize, to as usize),
        };
        // the piece to place on to
        let piece = match mv.piece() {
            PAWN if mv.promote() >= KNIGHT => mv.promote(),
            _otherwise => mv.piece(),
        };
        // the fields occupied by WHITE after the move
        let whites = match mv.player() {
            WHITE => (self.whites - fromMask) + toMask,
            BLACK => self.whites - (fromMask + toMask),
        };
        // updated pawn set
        let pawnSet = match piece {
            PAWN | KNIGHT | KING => self.pawnSet - fromMask + toMask,
            _otherwise => self.pawnSet - fromMask - toMask,
        };
        // updated bishop set
        let bishopSet = match piece {
            BISHOP | KNIGHT | QUEEN => self.bishopSet - fromMask + toMask,
            _otherwise => self.bishopSet - fromMask - toMask,
        };
        // updated bishop set
        let rookSet = match piece {
            ROOK | KING | QUEEN => self.rookSet - fromMask + toMask,
            _otherwise => self.rookSet - fromMask - toMask,
        };
        // current en passant hash value, if any, needs to get xor'ed out
        let currentEPHash = match self.flags * enPassantBits {
            currentEP if currentEP.some() => zobrist::flagZobrist(fld(currentEP) as usize),
            _otherwise => 0,
        };
        // hash for new en passant bit, if any, needs to get xor'ed in
        let enPassantHash = match enPassantBit {
            bits if bits.some() => zobrist::flagZobrist(fld(enPassantBit) as usize),
            _otherwise => 0,
        };
        // new hash
        let hash = self.hash
            ^ zobrist::flagZobrist(A1 as usize)   // flip the A1 hash
            ^ (boolMask((self.flags * lostCastlingRights).member(C1)) & zobrist::flagZobrist(C1 as usize))
            ^ (boolMask((self.flags * lostCastlingRights).member(G1)) & zobrist::flagZobrist(G1 as usize))
            ^ (boolMask((self.flags * lostCastlingRights).member(C8)) & zobrist::flagZobrist(C8 as usize))
            ^ (boolMask((self.flags * lostCastlingRights).member(G8)) & zobrist::flagZobrist(G8 as usize))
            ^ currentEPHash
            ^ enPassantHash
            ^ fromHash
            ^ zobrist::ppfZobrist(mv.player() as usize, piece as usize, mv.to() as usize)
            ^ toHash;
        Position {
            flags: tomove + castlingRights + hasCastledFlags + enPassantBit + plies,
            whites,
            pawnSet,
            bishopSet,
            rookSet,
            hash,
        }
    }

    /// Apply a "null move", that is, pretend it is the opponents move without changing anything on the board
    /// Note that this may result in an "impossible board".
    /// For example, if the active player stands in check, applying a null move would result in a board
    /// where the KING could be captured. Such a move, then, must not be applied, as certain functions
    /// will panic when there is no king.
    pub fn applyNull(&self) -> Position {
        // current en passant hash value, if any, needs to get xor'ed out
        let currentEPHash = match self.flags * enPassantBits {
            currentEP if currentEP.some() => zobrist::flagZobrist(fld(currentEP) as usize),
            _otherwise => 0,
        };
        Position {
            flags: BitSet { bits: (self.flags.bits ^ whiteToMove.bits) + onePlyRootOnly } - enPassantBits,
            hash: self.hash ^ zobrist::flagZobrist(A1 as usize) ^ currentEPHash,
            .. *self
        }
    } 

    /// generate the castling moves possible in this position
    fn castlingMoves(&self, vec: &mut Vec<Move>) {
        match self.turn() {
            WHITE => {
                let short = self.flags.member(G1)                   // short castling permitted, implies ROOK is on H1
                            && self.areEmpty(bit(G1) + bit(F1))     // fields between KING & ROOK are empty
                            && self.isNotAttacked(E1, BLACK)        // KING is not in check
                            && self.isNotAttacked(F1, BLACK)        // nor is the skipped field attacked
                            && self.isNotAttacked(G1, BLACK); // nor is the target field attacked
                let long = self.flags.member(C1)                    // long castling permitte, implies ROOK is on A1
                            && self.areEmpty(bit(D1)+bit(C1)+bit(B1))    // fields in betwen are empty
                            && self.isNotAttacked(E1, BLACK)
                            && self.isNotAttacked(D1, BLACK)
                            && self.isNotAttacked(C1, BLACK);
                if short {
                    vec.push(castlingShortWhite);
                }
                if long {
                    vec.push(castlingLongWhite);
                }
            }
            BLACK => {
                let short = self.flags.member(G8)                   // short castling permitted, implies ROOK is on H8
                            && self.areEmpty(bit(G8) + bit(F8))     // fields between KING & ROOK are empty
                            && self.isNotAttacked(E8, WHITE)        // KING is not in check
                            && self.isNotAttacked(F8, WHITE)        // nor is the skipped field attacked
                            && self.isNotAttacked(G8, WHITE); // nor is the target field attacked
                let long = self.flags.member(C8)                    // long castling permitte, implies ROOK is on A1
                            && self.areEmpty(bit(D8)+bit(C8)+bit(B8))    // fields in betwen are empty
                            && self.isNotAttacked(E8, WHITE)
                            && self.isNotAttacked(D8, WHITE)
                            && self.isNotAttacked(C8, WHITE);
                if short {
                    vec.push(castlingShortBlack);
                }
                if long {
                    vec.push(castlingLongBlack);
                }
            }
        };
    }

    /// Given a position and a field, generate the possible moves for
    /// the piece standing there.
    /// 
    /// The generated moves may still be illegal because the expose the king for check!
    fn genMove(&self, vec: &mut Vec<Move>, from: Field) {
        let piece = self.pieceOn(from);
        let player = self.turn();
        let validTargets = pieceTargets(piece, player, from) - self.occupiedByActive();
        let mut validTargets = validTargets.bits;
        
        while validTargets != 0 {
            let to = Field::from(validTargets.trailing_zeros() as u8);
            validTargets ^= 1 << to as u64;
            let mv = Move::new(player, piece, EMPTY, from, to);
            match piece {
                PAWN => {
                    let es = match player {
                        WHITE => mdb::canWhitePawn(from, to),
                        BLACK => mdb::canBlackPawn(from, to),
                    };
                    let promotion = player == WHITE && to.rank() == 8 || player == BLACK && to.rank() == 1;
                    let valid = es.some() && self.areEmpty(es)
                            || es.null() // it's a capturing move
                                &&     (self.occupiedBy(player.opponent()).member(to) 
                                    || (self.flags*enPassantBits).member(to));
                    if valid {
                        if promotion {                            
                            vec.push(Move::new(player, PAWN, QUEEN, from, to)); 
                            vec.push(Move::new(player, PAWN, ROOK, from, to));
                            vec.push(Move::new(player, PAWN, BISHOP, from, to));
                            vec.push(Move::new(player, PAWN, KNIGHT, from, to));
                        }
                        else if es.null() && self.isEmpty(to) {  // capturing en-passant
                            let mv = Move::new(player, PAWN, PAWN, from, to);
                            vec.push(mv);
                        }
                        else {
                            vec.push(mv);
                        }
                    };
                },
                BISHOP => if self.areEmpty(mdb::canBishop(from, to)) 
                              { vec.push(mv); },
                ROOK   => if self.areEmpty(mdb::canRook(from, to)) 
                              { vec.push(mv); },
                QUEEN  => if   self.areEmpty(mdb::canBishop(from, to)) 
                            || self.areEmpty(mdb::canRook(from, to))
                              { vec.push(mv); },
                _other => vec.push(mv),
            };
        }
    }

    /// List of possible non castling moves in a given position.
    /// 
    /// **Note** Moves who are in fact invalid because they leave the king in check
    ///  are not yet filtered out.
    fn rawMoves(&self, vec: &mut Vec<Move>) {
        let mut occ = self.occupiedByActive().bits; 
        while occ != 0 {
            let from = Field::from(occ.trailing_zeros() as u8);
            occ ^= 1 << from as u64;
            self.genMove(vec, from);
        }
    }

    /// List of possible moves in a given position.
    /// Verified to not leave the king of the moving player in check.
    pub fn moves(&self) -> Vec<Move> {
        let mut vec = Vec::with_capacity(64);
        self.castlingMoves(&mut vec);
        self.rawMoves(&mut vec);
        // vec
        let mut result = Vec::with_capacity(64);
        for m in vec {
            if self.apply(m).notInCheck() { result.push(m); }
        }
        result
    }

    /// Positions qualify as "in opening"  if there are 
    /// at least 12 pawns and both sides have still castling rights
    pub fn inOpening(&self) -> bool {
        self.pawns().card() >= 12 
            && (self.flags * whiteCastlingRights).some() 
            && (self.flags * blackCastlingRights).some()
    }

    /// Positions qualify as "in endgame" if there are 
    /// less than 11 pieces or less than 5 pawns
    pub fn inEndgame(&self) -> bool {
        self.pawns().card() < 5
            || self.occupied().card() < 11
    }

    /// Compensate for the immobility penalty caused by pieces near the king
    /// Own pieces adjacent to the king are preferred
    pub fn coveredKing(&self, player: Player) -> i32 {
        // we assume there is a king for this player ;-)
        let kingIndex = fld(self.kings() * self.occupiedBy(player));
        let targets   = mdb::kingTargets(kingIndex);
        (self.occupiedBy(player.opponent()) * targets).card() as i32 * 5
        + (self.occupiedBy(player) * targets).card() as i32 * 6
    }

    /// Compute penalty for blocked pawns that are likely to block the (undeveloped) bishops.
    /// That is, for white, there shouldn't be any pieces on B3,D3,E3 or G3 before pawns.
    pub fn penaltyBlockedBishopBlockingPawns(&self, player: Player) -> i32 {
        // actual pawns on the fields in questions
        let pawns = self.pawns() * match player {
            WHITE => whiteBishopBlockingPawns, BLACK => blackBishopBlockingPawns
        };
        // restrict to own pawns, we don't want to penalize a white rook standing on E3
        // when a black pawn stands on E2, to the contrary
        let myPawns = match player {
            WHITE => pawns * self.whites, BLACK => pawns - self.whites
        };
        // compute the fields directly "before" the pawns, so as to block them
        let before = match player {
            WHITE => BitSet { bits: myPawns.bits << 8 },
            BLACK => BitSet { bits: myPawns.bits >> 8 },
        };
        (before * self.occupied()).card() as i32 * 21
    }

    /// Compute penalty for bad bishops.
    /// A bishop is bad when it has no room to move since it's blocked by pieces
    /// of the same colour.
    pub fn penaltyBadBishops(&self, player: Player) -> i32 {
        let bishops = self.bishops() * self.occupiedBy(player);
        let mut count = 0;
        for from in bishops {
            if (mdb::kingTargets(from) * mdb::bishopTargets(from)).subset(self.occupiedBy(player)) {
                count += 43;
            }
        };
        count
    }

    /// Compute penalty for lazy officers.
    /// In the opening, KNIGHTs and BISHOPs should be moved.
    /// An extra penalty hits for 3 or 4 lazy officers and a moved queen.
    pub fn penaltyLazyOfficers(&self, player: Player) -> i32 {
        let mine = self.occupiedBy(player);
        // let pawns = self.pawns() * mine;
        let rooks = self.rooks() * mine;
        let bishops = self.bishops() * mine;
        let knights = self.knights() * mine;
        let lazyOfficers = match player {
            WHITE => {
                let lazyLeftKnight  = rooks.member(A1) && knights.member(B1);
                let lazyRightKnight = rooks.member(H1) && knights.member(G1);
                let lazyLeftBishop  = rooks.member(A1) && bishops.member(C1);
                let lazyRightBishop  = rooks.member(H1) && bishops.member(F1);
                lazyLeftBishop as i32 
                    + lazyLeftKnight as i32 + lazyRightBishop as i32 + lazyRightKnight as i32
            },
            BLACK => {
                let lazyLeftKnight  = rooks.member(A8) && knights.member(B8);
                let lazyRightKnight = rooks.member(H8) && knights.member(G8);
                let lazyLeftBishop  = rooks.member(A8) && bishops.member(C8);
                let lazyRightBishop  = rooks.member(H8) && bishops.member(F8);
                lazyLeftBishop as i32 
                    + lazyLeftKnight as i32 + lazyRightBishop as i32 + lazyRightKnight as i32
            }
        };
        let busyQueen = match player {
            WHITE => {
                mine.member(D1) && self.pieceOn(D1) != QUEEN
            }
            BLACK => {
                mine.member(D8) && self.pieceOn(D8) != QUEEN
            }
        };
        if busyQueen && lazyOfficers > 2 { 150 } else { lazyOfficers * 30 }
    }

    /// Sum scores for material
    pub fn scoreMaterial(&self, player: Player) -> i32 {
        let mut bits = self.occupiedBy(player).bits;
        let mut sum  = 0;
        while bits != 0 {
            let from = Field::from(bits.trailing_zeros() as u8);
            bits ^= 1 << from as u64;
            let piece = self.pieceOn(from);
            // piece score + zone bonus
            sum += piece.score() + (from.zone() as i32 + 1)*5;
            match piece {
                ROOK | QUEEN | KING if self.inEndgame() => {
                    // compensate for unwillingness to be behind own pawns
                    let file = from.file();
                    let prank = if player == WHITE { 8 } else { 1 };
                    let pto = Field::fromFR(file, prank);
                    let pawns = mdb::canRook(from, pto) * self.pawns() * self.occupiedBy(player);
                    if pawns.some() && (mdb::canRook(from, pawns.bitIndex())*self.occupiedBy(player.opponent())).null() {
                        // there are pawns "before" the rook, king or queen and no aliens in between
                        sum += 50;
                    }
                }
                PAWN => {
                    // let rank = from.rank();
                    let file = from.file();
                    let prank = if player == WHITE { 8 } else { 1 };
                    // let orank = if player == WHITE { 2 } else { 7 };
                    let pto = Field::fromFR(file, prank);
                    let togo = mdb::canRook(from, pto) + bit(pto);
                    // the fewer steps to go, the more valuable
                    let freeFactor = if (togo * self.occupiedBy(player.opponent())).null() { 3 } else { 1 };
                    sum += freeFactor * ((1 << (7 - togo.card())) - 2);
                }
                _other => (),
            }
        }
        sum
    }


    /// compute the penalty for a hanging piece, at best 0 when piece is not in danger
    pub fn penaltyForHangingPiece(&self, f: Field, leastAttacker: Piece, leastDefender: Piece) -> i32 {
        let piece = self.pieceOn(f);
        match self.playerOn(f) {
            None => 0,  // wtf???
            Some(player) => {
                let active = player == self.turn();
                let hanging = hangingPenalty(
                                piece, 
                                leastAttacker, 
                                leastDefender != EMPTY);
                if active { percent(33, hanging) } else { hanging }
            }
        }
    }

    /// Compute the penalty for all the hanging pieces of a given player.
    /// We must take care that we don't just add the penalties. This would mean
    /// that it is ok to have the QUEEN hanging when the other player has a ROOK and 4 PAWNs hanging.
    /// Instead, we slightly increase the maximum value to indicate that it is better to
    /// attack, say, a ROOK and a BISHOP than just a ROOK.
    pub fn penalizeHanging(&self, player: Player, byOther: &[Piece;64], bySelf:&[Piece;64]) -> i32 {
        let mut score = 0;
        for f in self.occupiedBy(player) {
            let p = self.penaltyForHangingPiece(f, byOther[f as usize], bySelf[f as usize]);
            if p > 0 { 
                if score > 0 {
                    score = percent(110, max(score,p));
                }
                else {
                    score = p;
                }
            }
        }
        score
    }

    /// Bonus for empty fields that are owned by a player.
    /// A field counts as owned by player A, when the least piece of B
    /// that could go there is more valuable than the least piece of A
    /// that could go there or if B has no piece that could go there.
    /// (Because **if** B goes there, A can capture with a win.)
    ///
    /// In goes without saying that a defended field can never get owned by a KING.
    pub fn bonusOwned(&self, byWhite: &[Piece;64], byBlack: &[Piece;64]) -> i32 {
        let mut unoccupied = (BitSet::all() - self.occupied()).bits;
        let mut acc = 0;
        while unoccupied != 0 {
            let wo = Field::from(unoccupied.trailing_zeros() as u8);
            unoccupied ^= 1 << wo as u64;

            // skip the en passant field
            if (self.flags * enPassantBits).member(wo) { continue; };
            let bonus = (wo.zone() as i32 + 1) * 5;
            let whitePawns = self.attackedByPawns(wo, WHITE).card() as i32;
            let blackPawns = self.attackedByPawns(wo, BLACK).card() as i32;
            let whitePiece = byWhite[wo as usize];
            let blackPiece = byBlack[wo as usize];

            // we must take care of pawns separately, as attacking moves on
            // empty fields are not in the move list (except for en passant)
            if whitePawns > blackPawns {
                    acc += bonus;
            }
            else if blackPawns < whitePawns {
                    acc -= bonus;
            }
            else {
                match whitePiece {
                    EMPTY => if blackPiece != EMPTY { acc -= bonus },
                    PAWN => panic!("cannot happen: PAWN move in WHITE attack vector"),
                    BISHOP | KNIGHT => match blackPiece {
                        EMPTY | ROOK | QUEEN | KING => acc += bonus,
                        BISHOP | KNIGHT => (),
                        PAWN => panic!("cannot happen: PAWN move in BLACK attack vector"),
                    }
                    ROOK => match blackPiece {
                        EMPTY | QUEEN | KING => acc += bonus,
                        ROOK => (),
                        BISHOP | KNIGHT => acc -= bonus,
                        PAWN => panic!("cannot happen: PAWN move in BLACK attack vector"),
                    }
                    QUEEN => match blackPiece {
                        EMPTY | KING => acc += bonus,
                        QUEEN => (),
                        ROOK | BISHOP | KNIGHT => acc -= bonus,
                        PAWN => panic!("cannot happen: PAWN move in BLACK attack vector"),
                    }
                    KING => match blackPiece {
                        EMPTY  => acc += bonus,
                        KING => (),
                        QUEEN | ROOK | BISHOP | KNIGHT => acc -= bonus,
                        PAWN => panic!("cannot happen: PAWN move in BLACK attack vector"),
                    }
                }
            }
        };
        acc
    }

    /// Try to make castling attractive.
    /// Give a bonus of 25 if the player has castled.
    /// If the player hasn't castled yet, but has both castling rights, give a malus of -25.
    /// If he has only one castling right left, give a malus of -50.
    /// And finally, if the player hasn't castled and has no castling rights left, give a malus of -75.
    pub fn scoreCastling(&self, player: Player) -> i32 {
        let hasCastledBits = self.flags * match player {
            BLACK => blackHasCastledBits,
            WHITE => whiteHasCastledBits,
        };
        let castlingRights = self.flags * match player {
            BLACK => blackCastlingRights,
            WHITE => whiteCastlingRights
        };
        if hasCastledBits.some() { 25 }
        else { castlingRights.card() as i32 * 25 - 75 }
    }

    /// Evaluate a position. A positive score indicates a position favourable for white,
    /// while a negative one indicates a position favourable for black.
    /// 
    /// This function does **not** detect mate, stalemate, 
    /// draw by repetition or draw by the 50 moves rule.
    pub fn eval(&self) -> i32 {
        // the raw moves for player
        let mut pMoves = Vec::with_capacity(64);
        self.rawMoves(&mut pMoves);
        self.eval_have_moves(&pMoves)
    }

    /// A faster version of eval that is given the current moves
    pub fn eval_have_moves(&self, pMoves: &Vec<Move>) -> i32 {
        let matWhite = self.scoreMaterial(WHITE);
        let matBlack = self.scoreMaterial(BLACK);
        let matDelta = matWhite - matBlack;
        // The delta increases when the difference gets greater
        // This should result in an unwillingness to exchange pieces by the weaker party
        let matRelation = percent((max(matWhite, matBlack)*100) / min(matWhite, matBlack), matDelta);
        let check = self.inCheck(self.turn());
        let checkBonus = if check { 20 } else { 0 };
        let playerMoves = pMoves.len() as i32;
        // the raw moves for opponent
        let mut oMoves = Vec::with_capacity(64);
        self.applyNull().rawMoves(&mut oMoves);
        let opponentMoves = oMoves.len() as i32;
        // gives for each field the least WHITE attacker
        let mut attacksByWhite = [EMPTY; 64];
        // gives for each field the least BLACK attacker
        let mut attacksByBlack = [EMPTY; 64];
        // compute least attackers once
        for m in pMoves {
            let wo = m.to() as usize;
            let was = m.piece();
            if was == PAWN && self.isEmpty(m.to()) {
                continue;
            }
            if m.player() == WHITE {
                if attacksByWhite[wo] == EMPTY || attacksByWhite[wo] > was {
                    attacksByWhite[wo] = was;
                }
            }
            else {
                if attacksByBlack[wo] == EMPTY || attacksByBlack[wo] > was {
                    attacksByBlack[wo] = was;
                }
            }
        }
        for m in oMoves {
            let wo = m.to() as usize;
            let was = m.piece();
            if was == PAWN && self.isEmpty(m.to()) {
                continue;
            }
            if m.player() == WHITE {
                if attacksByWhite[wo] == EMPTY || attacksByWhite[wo] > was {
                    attacksByWhite[wo] = was;
                }
            }
            else {
                if attacksByBlack[wo] == EMPTY || attacksByBlack[wo] > was {
                    attacksByBlack[wo] = was;
                }
            }
        } 
        
        matRelation // + self.bonusOwned(&attacksByWhite, &attacksByBlack)
        - self.penalizeHanging(WHITE, &attacksByBlack, &attacksByWhite) 
        + self.penalizeHanging(BLACK, &attacksByWhite, &attacksByBlack)
        + self.turn().opponent().forP(checkBonus + 4*opponentMoves)
        + self.turn().forP(4*playerMoves)
        + self.scoreCastling(WHITE) - self.scoreCastling(BLACK)
        + self.coveredKing(WHITE)   - self.coveredKing(BLACK)
        - self.penaltyBlockedBishopBlockingPawns(WHITE) + self.penaltyBlockedBishopBlockingPawns(BLACK)
        - self.penaltyBadBishops(WHITE) + self.penaltyBadBishops(BLACK)
        - self.penaltyLazyOfficers(WHITE) + self.penaltyLazyOfficers(BLACK)
    }

    /// compresses this position
    pub fn compressed(&self) -> CPos { CPos::new(self) }

    /// check position for validity
    /// 1. There must be exactly one king for each player
    /// 2. Pawns are restricted to ranks 2 to 7
    /// 3. only the player to move may be in check
    pub fn valid(&self) -> bool {
        (self.whites * self.kings()).card() == 1
        && (self.kings() - self.whites).card() == 1
        && (self.pawns() - pawnFields).null()
        && self.notInCheck()
    }

    /// Does this position represent a valid endgame position?
    /// 1. It must be a valid position.
    /// 2. There must be at most 4 pieces besides the kings.
    /// 3. No castling rights must be present anymore.
    pub fn validEndgame(&self) -> bool {
        self.valid() && self.occupied().card() < 7 && (self.flags * castlingBits).null()
    }
}

/// Compute the penalty for a hanging piece `hang` that is attacked by some piece `att`
/// depending on whether the hanging piece is defended or not
/// Gives a bonus of 10 percent of the piece value when it is not attacked, but defended
pub fn hangingPenalty(hang: Piece, att: Piece, defended: bool) -> i32 {
    let scoreh = hang.score();
    let scorea = att.score();
    match att {
        EMPTY => if defended { percent(10, scoreh) } else { 0 },
        KING if defended => 0,
        _hanging_king if hang == KING => 0,             // avoid extra bonus for check move
        _otherwise => match defended {
            false => percent(70, scoreh), 
            true if scoreh > scorea => percent(70, scoreh - scorea),
            _other => 0  
        }
    }
}


impl PartialEq for Position {
    fn eq(&self, other: &Position) -> bool {
        self.hash == other.hash 
            && self.flags - counterBits == other.flags - counterBits
            && self.whites == other.whites
            && self.pawnSet == other.pawnSet
            && self.bishopSet == other.bishopSet
            && self.rookSet == other.rookSet
    }
}

impl Eq for Position {}

impl Hash for Position {
    fn hash<H: Hasher>(&self, state: &mut H) { self.hash.hash(state); }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let wmoves = if self.turn() == WHITE { self.moves() } else { self.applyNull().moves() };
        let bmoves = if self.turn() == BLACK { self.moves() } else { self.applyNull().moves() };
        let wvs: Vec<_> = wmoves.iter().map(|x| x.algebraic()).collect();
        let bvs: Vec<_> = bmoves.iter().map(|x| x.algebraic()).collect();
        write!(
            f,
            "P:hash=0x{:x}  flags=0x{:x}  flags{}\n\
             whites{}\n\
             pawns {}\n\
             bishops{}  knights{}\n\
             rooks  {}  queens {} kings{}\n\
             eval={}  occupied={}\n\
             turn={:?}  black in check={}  white in check={}  notInCheck={} valid={}  validEndgame={}\n\
             material       {}  {}\n\
             moves          {}  {}\n\
             castling       {}  {}\n\
             covered king   {}  {}\n\
             bbbPawns       {}  {}\n\
             bad bishops    {}  {}\n\
             lazy officers  {}  {}\n\
             white moves    [{}]\n\
             black moves    [{}]",
            self.hash, self.flags.bits, self.flags - counterBits, self.whites, 
            self.pawns(), 
            self.bishops(), self.knights(),
            self.rooks(), self.queens(), self.kings(),
            self.eval(), self.occupied(),
            self.turn(), self.inCheck(BLACK), self.inCheck(WHITE), self.notInCheck(), self.valid(), self.validEndgame(),
            self.scoreMaterial(WHITE), - self.scoreMaterial(BLACK),
            // -self.penalizeHanging(WHITE), self.penalizeHanging(BLACK),
            4 * wmoves.len() as i32, -4 * bmoves.len() as i32,
            self.scoreCastling(WHITE), - self.scoreCastling(BLACK),
            self.coveredKing(WHITE),   - self.coveredKing(BLACK),
            - self.penaltyBlockedBishopBlockingPawns(WHITE), self.penaltyBlockedBishopBlockingPawns(BLACK),
            - self.penaltyBadBishops(WHITE), self.penaltyBadBishops(BLACK),
            - self.penaltyLazyOfficers(WHITE),  self.penaltyLazyOfficers(BLACK),
            &wvs[..].join(", "), &bvs[..].join(", ")
        )
    }
}

/// Representation of a move
///    
/// Normally, the promote bits are EMPTY, e.g. 0b000.
/// If moving piece is KING, promote may be QUEEN or KING to indicate
/// queenside or kingside castling. If it's a promoting PAWN move,
/// promote may be KNIGHT, BISHOP, ROOK or QUEEN. If it's an en passant
/// capturing, promote will be PAWN.
///
/// We need 19 bits:
///
/// ```text
/// Pfffpppttttttssssss
/// ```
///
/// - [P] 1 bit for player where 0 means BLACK, 1 means WHITE
/// - [fff] 3 bit encoding for moving Piece
/// - [ppp] 3 bit for promotion piece
/// - [tttttt] 6 bit target index
/// - [ssssss] 6 bit source index
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[repr(transparent)]
pub struct Move {
    mv: u32,
}

pub const noMove: Move = Move { mv: 0xffff_ffff };

#[allow(clippy::inconsistent_digit_grouping)]
// we use the unconventional digit grouping to demonstrate the structure
// of the bitfield
impl Move {
    /// `true` if and only if this is not `noMove`
    #[inline]
    pub fn isMove(self) -> bool { self != noMove }

    /// Which player is moving?
    #[inline]
    pub fn player(self) -> Player { Player::from(self.mv & 0b01_000_000_000000_000000u32 != 0) }

    /// What piece is moving?
    #[inline]
    pub fn piece(self) -> Piece { Piece::from((self.mv & 0b00_111_000_000000_000000u32) >> 15) }

    /// What is it promoting into, if any?
    #[inline]
    pub fn promote(self) -> Piece { Piece::from((self.mv & 0b00_000_111_000000_000000u32) >> 12) }

    /// Whereto are we moving?
    #[inline]
    pub fn to(self) -> Field { Field::from(((self.mv & 0b00_000_000_111111_000000u32) >> 6) as u8) }

    /// From whence are we moving?
    #[inline]
    pub fn from(self) -> Field { Field::from((self.mv & 0b111111u32) as u8) }

    /// construct a move from player, piece to move, piece to promote,
    /// from field and to field
    pub const fn new(pl: Player, pc: Piece, pr: Piece, from: Field, to: Field) -> Move {
        Move {
            mv: ((pl as u32) << 18) | ((pc as u32) << 15) | ((pr as u32) << 12) | ((to as u32) << 6) | (from as u32),
        }
    }

    /// Show this Move in algebraic notation
    ///
    /// ```
    /// use rasch::position::Move;
    /// use rasch::position::Piece::*;
    /// use rasch::position::Player::*;
    /// use rasch::fieldset::Field::*;
    ///
    /// assert_eq!(Move::new(WHITE, PAWN, QUEEN, B7, C8).algebraic(), "b7c8q");
    /// ```
    pub fn algebraic(self) -> String {
        if self == noMove { return "????".to_string(); }
        let p = if self.promote() >= KNIGHT && self.piece() == PAWN {
            self.promote().show().to_lowercase()
        } else {
            String::from("")
        };
        self.from().show() + &self.to().show() + &p
    }

    /// Find a move in a list that corresponds to a given string or post
    /// an Err. This will be used when accepting a move to make from
    /// the user.
    ///
    /// This is so that we'll use the moves generated by our move
    /// generator only. Assuming the move generator generates all
    /// possible moves for any position, the move entered **must
    /// be** in the list, or else it is illegal.
    pub fn unAlgebraic(list: &[Move], src: &str) -> Result<Move, String> {
        for mv in list.iter() {
            if mv.algebraic() == src {
                return Ok(*mv);
            }
        }
        let vs: Vec<_> = list.iter().map(|x| x.algebraic()).collect();
        Err(format!("Move {} does not appear in [{}]", src, &vs[..].join(", ")))
    }

    /// Show a move in standard algebraic notation (SAN)
    pub fn showSAN(self, pos: Position) -> String {
        // handle castling first
        if self.piece() == KING && self.promote() != EMPTY {
            if self.promote() == KING { return "O-O".into(); }
            else { return "O-O-O".into(); }
        }
        // no castling
        let moves = pos.moves();
        let mut san = if self.piece() != PAWN { self.piece().show() } else {"".into()};
        let capture = !pos.isEmpty(self.to()) || self.promote() == PAWN;
        let file = moves.iter().any(
            |m|    m.from()   != self.from()
                && m.to()     == self.to()
                && m.player() == self.player() 
                && m.piece()  == self.piece() 
                && m.from().rank() == self.from().rank())
                || (self.piece() == PAWN && capture);
        let rank = moves.iter().any(
            |m|    m.from()   != self.from()
                && m.to()     == self.to()
                && m.player() == self.player() 
                && m.piece()  == self.piece()
                && m.from().file() == self.from().file());
        let prom = if self.promote() > PAWN {
                "=".to_string() + &self.promote().show() 
        } else { "".to_string() };
        if file { san.push(self.from().file()); }
        if rank { san += &format!("{}", self.from().rank()); }
        if capture { san.push('x'); }
        san += &self.to().show();
        san += &prom;
        san
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result { write!(f, "{}", self.algebraic()) }
}

impl Mirrorable for Move {
    fn mirrorH(&self) -> Self {
        let from = self.from().mirrorH();
        let to   = self.to().mirrorH();
        Move::new(self.player(), self.piece(), self.promote(), from, to)
    }
    fn mirrorV(&self) -> Self {
        let from = self.from().mirrorV();
        let to   = self.to().mirrorV();
        Move::new(self.player(), self.piece(), self.promote(), from, to)
    }
}


pub fn showMoves(moves: &[Move]) -> String {
    moves.iter().map(|x| x.algebraic()).collect::<Vec<_>>().join(" ")
}

pub fn showMovesSAN(moves: &[Move], start: Position) -> String {
    let mut sans = vec![];
    let mut pos  = start;
    for m in moves {
        sans.push(m.showSAN(pos));
        pos = pos.apply(*m);
    }
    sans.join(" ")
}

/// An empty board where it is WHITE's turn and all castling rights are set.
/// Anything else is zero.
/// 
/// You need to place at least two KINGS onto this board before it is valid.
pub fn emptyBoard() -> Position {
    Position {
        hash:      0,
        flags:     bit(A1) + castlingBits,
        whites:    BitSet::empty(),
        pawnSet:   BitSet::empty(),
        bishopSet: BitSet::empty(),
        rookSet:   BitSet::empty(),
    }
}

pub fn initialBoard() -> Position {
    let empty = emptyBoard();
    empty
        .place(BLACK, PAWN, [A7, B7, C7, D7, E7, F7, G7, H7].iter().collect())
        .place(WHITE, PAWN, [A2, B2, C2, D2, E2, F2, G2, H2].iter().collect())
        .place(BLACK, BISHOP, [C8, F8].iter().collect())
        .place(WHITE, BISHOP, [C1, F1].iter().collect())
        .place(BLACK, KNIGHT, [B8, G8].iter().collect())
        .place(WHITE, KNIGHT, [B1, G1].iter().collect())
        .place(BLACK, ROOK, [A8, H8].iter().collect())
        .place(WHITE, ROOK, [A1, H1].iter().collect())
        .place(BLACK, QUEEN, bit(D8))
        .place(WHITE, QUEEN, bit(D1))
        .place(BLACK, KING, bit(E8))
        .place(WHITE, KING, bit(E1))
        .rehash()
}

#[rustfmt::skip]
/// Compressed position for the endgame tablebases.

/// This is good for up to 6 pieces (kings included) and 11 bits worth of flags. 
/// Since a black and a white `KING` *must* be present, we simply note their fields in the lower 2x6 bits.
/// 
/// Each of the other 4 pieces is encoded in 10 bits: a code for player and piece type (4 bits) and the field (6 bits).
/// The encodings are:

/// - 0 - EMPTY
/// - 1/9  - BLACK/WHITE PAWN
/// - 2/10 - BLACK/WHITE KNIGHT
/// - 3/11 - BLACK/WHITE BISHOP
/// - 4/12 - BLACK/WHITE ROOK
/// - 5/13 - BLACK/WHITE QUEEN
/// - 7/15 - BLACK/WHITE PAWN that has just done a double move and can be captured en passant, the en passant field is behind it
///
/// In order to identify equal positions, the pieces must occur from left to right in **ascending field order**. 
/// This is guaranteed when compressing a `Position`.
/// 
/// Care must be taken when reflecting a `CPos` to get the pieces in the correct order and to correct the move index, if any.
/// For this, the from/to fields in the move must be reflected as well and the index of it found in the move list of the new position.
/// 
/// Not all u64 values make for a valid `CPos`. The following fatal errors will abort the program:
/// - trying to compress a `Position` that is not a valid endgame as checked by `Position.validEndgame()`
/// - trying to uncompress a `CPos` that contains a wrong piece code 8, 6 or 14
/// - trying to uncompress a `CPos` where any two field numbers are equal. That is, every piece and the kings must have their own unique fields.
/// - trying to reflect a `CPos` with an invalid move index.
// 
// 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111
//                                                                         -- ----    field number of the white king
//                                                                  ---- --           field number of the black king
//                                                          -- ----                   field number of 1st piece
//                                                     -- --                          code of 1st piece
//                                              ---- --                               field number of 2nd piece
//                                         ----                                       code of 2nd piece
//                                 -- ----                                            field number of 3rd piece
//                            -- --                                                   code of 3rd piece
//                     ---- --                                                        field number of 4th piece
//                ----                                                                code of 4th piece
//      mmmm mmmm                                                                     move number to play (255 means: no move)
//  fff                                                                               flags
// t                                                                                  0 = black to move, 1 = white to move
//
// Meaning of the flags:
// 000 - not analyzed yet
// 001 - white wins (by playing the indicated move or black is mate)
// 010 - black wins (by playing the indicated move or white is mate)
// 011 - it's a stalemate
// 100 - white can reach stalemate by playing the indicated move
// 101 - black can reach stalemate by playing the indicated move
// 110 - player cannot avoid stalemate
// 111 - player cannot avoid mate
// 
// 
#[derive(Clone, Copy, Debug)]
pub struct CPos {
    pub bits: u64
}

/// Result of retrograde analysis.
/// - MATE is given to all positions where the player cannot make any legal move and is in check.
/// - STALEMATE is given to all positions where the player cannot move, but is not in check. 
/// In addition, this is reported for positions not found in a database search.
/// - CAN_MATE is given to all positions where the player has a move such that 
/// the resulting position is either MATE or CANNOT_AVOID_MATE.
/// - CAN_DRAW is given to all positions that are not CAN_MATE where the player has a move such that
/// the resulting position is either STALEMATE or CANNOT_AVOID_DRAW.
/// - CANNOT_AVOID_DRAW is given to positions, where all moves lead to STALEMATE or CAN_DRAW
/// - CANNOT_AVOID_MATE is givon to positions, where all moves lead to CAN_MATE.
/// 
/// With CAN_MATE and CAN_DRAW, the index of the move to play is recorded in the cposMove bits.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CPosState {
    UNKNOWN,
    MATE,
    STALEMATE,
    CAN_MATE,
    CAN_DRAW,
    CANNOT_AVOID_DRAW,
    CANNOT_AVOID_MATE,
}


impl From<u64> for CPosState {
    fn from(u: u64) -> CPosState {
        match u&7 {
            1 => MATE,
            2 => STALEMATE,
            3 => CAN_MATE,
            4 => CAN_DRAW,
            5 => CANNOT_AVOID_DRAW,
            6 => CANNOT_AVOID_MATE,
            _ => UNKNOWN,
        }
    }
}

pub use CPosState::*;

/// Mask the flag bits in a CPos
pub const cposFlags : u64 = 0x7000_0000_0000_0000u64;
/// Shift for flag bits in CPos
pub const cposFlagShift : u32 = cposFlags.trailing_zeros();
/// Mask the move bits in a CPos
pub const cposMove : u64 = 0x0ff0_0000_0000_0000u64;
/// Shift for the move flags in a CPos
pub const cposMoveShift : u32 = cposMove.trailing_zeros();
/// Mask the bits that count in comparisions
pub const cposComp : u64 = !(cposFlags|cposMove);
/// Mask the code for piece 4
pub const cposCode4 : u64 = 0x000f_0000_0000_0000u64;
/// Mask the code for piece 3
pub const cposCode3 : u64 = cposCode4 >> 10;
/// Mask the code for piece 2
pub const cposCode2 : u64 = cposCode3 >> 10;
/// Mask the code for piece 1
pub const cposCode1 : u64 = cposCode2 >> 10;
/// Mask the field of piece 4
pub const cposFld4 : u64 = 0x0000_fc00_0000_0000u64;
/// Maskt the field of piece 3
pub const cposFld3 : u64 = cposFld4 >> 10;
/// Maskt the field of piece 2
pub const cposFld2 : u64 = cposFld3 >> 10;
/// Maskt the field of piece 1
pub const cposFld1 : u64 = cposFld2 >> 10;
/// Mask the field of the black king
pub const cposWhiteKing : u64 = 0x3f;
/// Mask the field of the white king
pub const cposBlackKing : u64 = cposWhiteKing << 6;
/// list of masks that extract field numbers
pub const cposFieldMasks : [u64; 6] = [cposBlackKing, cposWhiteKing, cposFld1, cposFld2, cposFld3, cposFld4];


/// Mask the lower left quarter fields (A..D, 1..4)
pub const lowerLeftQuarter :  BitSet = BitSet { bits: 0x0000_0000_0f0f_0f0f };
/// Mask the lower right quarter fields (E..H, 1..4)
pub const lowerRightQuarter : BitSet = BitSet { bits: 0x0000_0000_f0f0_f0f0 };
/// Mask the fields in the lower half A1..H4
pub const lowerHalf: BitSet = BitSet { bits: lowerLeftQuarter.bits | lowerRightQuarter.bits };
/// Mask the fields on files A to D
pub const leftHalf: BitSet = BitSet { bits: 0x0f0f_0f0f_0f0f_0f0fu64 };



impl CPos {

    /// Compress an ordinary position, which must be a valid endgame position
    pub fn new(pos: &Position) -> CPos {

        // this makes sure that we never use more than 40 bits for encoding of pieces
        if !pos.validEndgame() {
            panic!("not an endgame position {}", pos);
        }

        let mut pieces = 0;
        let mut kings = 0;
        // shift the A1 flag to the front and indicate no valid move
        let flags = (pos.flags.bits << 63) | cposMove;   
        
        // `BitSet` iterator guarantees fields in ascending order
        for f in pos.occupied() {
            let pOn = pos.pieceOn(f);
            if  pOn != KING {
                let color = if pos.whites.member(f) { 8 } else { 0 };
                let pcode = match pOn {
                    PAWN if f.rank() == 4 && color == 8  && pos.flags.member(Field::from(f as u8 - 8)) => 15,
                    PAWN if f.rank() == 5 && color == 0  && pos.flags.member(Field::from(f as u8 + 8)) => 7,
                    piece => color | piece as u64,
                };
                pieces <<= 4;
                pieces |= pcode;
                pieces <<= 6;
                pieces |= f as u64;
            }
            else  {
                if pos.whites.member(f) { kings |= f as u64; } else { kings |= (f as u64) << 6; }
            }
        }
        CPos { bits: flags | (pieces << 12) | kings }
    }

    /// re-construct Position
    pub fn uncompressed(&self) -> Position {
        let mut pos = Position {
            flags: BitSet::empty(), whites: BitSet::empty(), pawnSet: BitSet::empty(), rookSet: BitSet::empty(), bishopSet: BitSet::empty(), hash: 0

        };
        // establish whose turn it is
        if self.bits >> 63 == 1 { pos.flags = whiteToMove; }
        // place the kings
        pos = pos.place(WHITE, KING, bit(Field::from((self.bits & 0x3f) as u8)))
                .place(BLACK, KING, bit(Field::from(((self.bits >> 6) & 0x3f) as u8)));
        let mut pcs = self.bits >> 12;
        for _i in 0..4 {
            let f = Field::from((pcs&0x3f) as u8);
            pcs >>= 6;
            let c = if pcs & 8 == 0 { BLACK } else { WHITE };
            let p = match pcs&7 {
                0 => EMPTY,
                1 => PAWN,
                2 => KNIGHT,
                3 => BISHOP,
                4 => ROOK,
                5 => QUEEN,
                7 => {
                    if c == WHITE { pos.flags = pos.flags + bit(Field::from(f as u8 - 8)) }
                    else { pos.flags = pos.flags + bit(Field::from(f as u8 + 8)) }
                    PAWN
                }
                _other => {
                    panic!("illegal piece code {} in compressed position", pcs&7);
                    // EMPTY
                }
            };
            if p != EMPTY {
                if pos.isEmpty(f) {
                    pos = pos.place(c, p, bit(f)); 
                } else {
                    panic!("Double occupation of field {} in compressed position.", f);
                }
            }
            pcs >>= 4;
        }
        pos
    }

    pub fn signature(&self) -> String {
        let mut result = String::from("");
        let pos = self.uncompressed();
        result.push('K');
        // look for white queens, etc.
        for _i in 0 .. (pos.queens()  * pos.whites).card() { result.push('Q'); }
        for _i in 0 .. (pos.rooks()   * pos.whites).card() { result.push('R'); }
        for _i in 0 .. (pos.bishops() * pos.whites).card() { result.push('B'); }
        for _i in 0 .. (pos.knights() * pos.whites).card() { result.push('N'); }
        for _i in 0 .. (pos.pawns()   * pos.whites).card() { result.push('P'); }
        result.push('-');
        result.push('K');
        for _i in 0 .. (pos.queens()  - pos.whites).card() { result.push('Q'); }
        for _i in 0 .. (pos.rooks()   - pos.whites).card() { result.push('R'); }
        for _i in 0 .. (pos.bishops() - pos.whites).card() { result.push('B'); }
        for _i in 0 .. (pos.knights() - pos.whites).card() { result.push('N'); }
        for _i in 0 .. (pos.pawns()   - pos.whites).card() { result.push('P'); }
        result
    }


    /// Does this position have pawns?
    pub fn hasPawns(&self) -> bool {
        let f = (self.bits & cposCode1) >> cposCode1.trailing_zeros();
        if f == 1 || f == 9 || f == 7 || f == 15 { true }
        else {
            let f = (self.bits & cposCode2) >> cposCode2.trailing_zeros();
            if f == 1 || f == 9 || f == 7 || f == 15 { true }
            else {
                let f = (self.bits & cposCode3) >> cposCode3.trailing_zeros();
                if f == 1 || f == 9 || f == 7 || f == 15 { true }
                else {
                    let f = (self.bits & cposCode4) >> cposCode4.trailing_zeros();
                    f == 1 || f == 9 || f == 7 || f == 15
                }
            }
        }
    }

    /// get the state from the CPos
    pub fn state(&self) -> CPosState {
        CPosState::from((self.bits & cposFlags) >> cposFlagShift)
    }

    /// make an identical CPos with a new state
    pub fn withState(&self, ns: CPosState) -> CPos {
        CPos { bits: (self.bits & !cposFlags) | ((ns as u64) << cposFlagShift ) }
    }

    /// get the move index
    pub fn moveIndex(&self) -> usize {
        ((self.bits & cposMove) >> cposMoveShift) as usize
    }

    /// set the move index
    pub fn withMoveIndex(&self, idx: usize) -> CPos {
        CPos { bits: (self.bits & !cposMove) | ((idx as u64) << cposMoveShift ) }
    }

    /// get the field number of the white king
    pub fn whiteKing(&self) -> Field {
        Field::from((self.bits & cposWhiteKing) as u8)
    }

    /// get a bitset with the psoition of the white king set
    pub fn whiteKingBit(&self) -> BitSet {
        bit(self.whiteKing())
    }

    /// Make a canonical CPos for lookup in the DB
    /// A canonical CPos has the white king in the left half and, if the position has no pawns,
    /// in the lower left quarter.
    
    pub fn canonical(&self) -> CPos {
        let kf = self.whiteKing();
        if self.hasPawns() {
            if leftHalf.member(kf) { *self }
            else {
                self.mirrorV()
            }
        }
        else {
            if lowerLeftQuarter.member(kf) { *self }
            else if lowerHalf.member(kf) { self.mirrorV() }
            else if leftHalf.member(kf) {
                self.mirrorH() 
            }
            else /* upper right quarter */ {
                self.mirrorH().mirrorV()
            }
        }
    }

    // read a CPos at the current position
    pub fn read(file: &mut File) -> Result<CPos, std::io::Error> {
        let mut buf = [0u8; 8];
        file.read_exact(&mut buf)?;
        Ok( CPos { bits: u64::from_be_bytes(buf) } )
    }

    /// read a CPos at some seek position
    pub fn read_at(file: &mut File, wo: SeekFrom) -> Result<CPos, std::io::Error> {
        file.seek(wo)?;
        CPos::read(file)
    }
    
    /// write a CPos at the current file position
    pub fn write(&self, file: &mut File) -> Result<(), std::io::Error> {
        let buf = self.bits.to_be_bytes();
        file.write_all(&buf)
    }

    /// write a CPos all sequentially
    pub fn write_seq(&self, file: &mut std::io::BufWriter<File>) -> Result<(), std::io::Error> {
        let buf = self.bits.to_be_bytes();
        file.write_all(&buf)
    }

    /// write a CPos at some seek position
    pub fn write_at(&self, file: &mut File, wo: SeekFrom)  -> Result<(), std::io::Error> {
        file.seek(wo)?;
        self.write(file)
    }

    /// find a CPos in a sorted vector
    pub fn lookup(&self, vec: &Vec<CPos>) -> Option<CPos> {
        match vec.binary_search(&self.canonical()) {
            Ok(p) => Some(vec[p]),
            Err(_) => None,
        }
    }

    /// find a CPos in a sorted vector that holds the positions for a certain signature
    /// or look in the file system
    pub fn find(&self, vec: &Vec<CPos>, sig: &str, hash: &mut HashMap<String, Box<File>>) -> Result<CPos, String> {
        if self.signature() == sig {
            match self.lookup(vec) {
                None => Err(String::from("not found in memory")),
                Some(c) => Ok(c),
            }
        }
        else {
            let path = format!("egtb/{}.egtb", self.signature());
            let hentry = hash.get_mut(&self.signature());
            let mut file = match hentry {
                Some(f) => f,
                None => {
                    let rfile = File::open(&path);
                    let wtf = Box::new (match rfile  {
                        Err(ioe) => return Err(format!("could not open EGTB file {} ({})", path, ioe)),
                        Ok(opened) => {
                            opened
                            
                            // 
                            // hash.get(&self.signature()).unwrap()
                        }
                    });
                    hash.insert(self.signature(), wtf);
                    hash.get_mut(&self.signature()).unwrap()    
                }
            };
            
            
            // let mut file = opened;
            match file.seek(SeekFrom::End(0)) {
                Err(ioe) => Err(format!("error seeking EGTB file {} ({})", path, ioe)),
                Ok(u) => {
                    let mut upper = u / 8;
                    // eprintln!("There are {} positions in {}", u, self.signature());
                    let mut lower = 0;
                    let cpos = self.canonical();
                    while lower < upper {
                        let mid = lower + (upper-lower) / 2;
                        match CPos::read_at(&mut file, SeekFrom::Start(8*mid)) {
                            Err(ioe) => { 
                                return Err(format!("error reading EGTB file {} at {} ({})", 
                                    path, 8*mid, ioe)); 
                            }
                            Ok(c) => {
                                if      c == cpos      { return Ok(c); }
                                else if c <  cpos      { lower = mid + 1; }
                                else /* c >  cpos */   { upper = mid; }
                            }
                        }
                    }
                    // pretend we found a DRAW
                    Ok(self.withState(STALEMATE))
            
                }
            }
        }
    }
 }

impl PartialEq for CPos {
    fn eq(&self, other: &CPos) -> bool {
        self.bits & cposComp == other.bits & cposComp
    }
}

impl Eq for CPos {}

impl PartialOrd for CPos {
    fn partial_cmp(&self, other: &CPos) -> Option<Ordering> {
        (self.bits & cposComp).partial_cmp(&(other.bits & cposComp))
    }
    fn lt(&self, other: &CPos) -> bool {
        (self.bits & cposComp) < (other.bits & cposComp)
    }
    fn le(&self, other: &CPos) -> bool {
        (self.bits & cposComp) <= (other.bits & cposComp)
    }
    fn gt(&self, other: &CPos) -> bool {
        (self.bits & cposComp) > (other.bits & cposComp)
    }
    fn ge(&self, other: &CPos) -> bool {
        (self.bits & cposComp) >= (other.bits & cposComp)
    }
}

impl Ord for CPos {
    fn cmp(&self, other: &CPos) -> Ordering {
        (self.bits & cposComp).cmp(&(other.bits & cposComp))
    }
}

impl Hash for CPos {
    fn hash<H: Hasher>(&self, state: &mut H)  { (self.bits & cposComp).hash(state); }
}


pub trait Mirrorable where Self: Sized {
    fn mirrorH(&self) -> Self;
    fn mirrorV(&self) -> Self;
}


impl Mirrorable for Field {
    /// Mirror the field on horizontal middle axis.
    /// Changes to corresponding rank on other side, e.g. c6 <-> c3
    fn mirrorH(&self) -> Field { Field::fromFR(self.file(), 9 - self.rank()) }
    /// Mirror the field on the vertical middle axis.
    /// Changes to corresponding file on the other side, e.g. c3 <-> f3
    fn mirrorV(&self) -> Field { Field::fromFR((b'h' as u8 - self.file() as u8 + b'a' as u8) as char, self.rank()) }
}

impl Mirrorable for CPos {
    fn mirrorH(&self) -> CPos {
        if self.hasPawns() {
            panic!("can't mirror a position with pawns horizontally");
        };
        let mut bits = self.bits;
        for m in &cposFieldMasks {
            if     *m == cposFld1 && (bits&cposCode1) == 0 
                || *m == cposFld2 && (bits&cposCode2) == 0 
                || *m == cposFld3 && (bits&cposCode3) == 0 
                || *m == cposFld4 && (bits&cposCode4) == 0 { /* don't mirror unused fields */ } 
            else {
                let f = Field::from(((bits & m) >> m.trailing_zeros()) as u8).mirrorH();
                bits &= !m;
                bits |= (f as u64) << m.trailing_zeros();
            }
        }
        CPos { bits }.uncompressed().compressed()
    }
    fn mirrorV(&self) -> CPos {
        // we can always do this
        let mut bits = self.bits;
        for m in &cposFieldMasks {
            if     *m == cposFld1 && (bits&cposCode1) == 0 
                || *m == cposFld2 && (bits&cposCode2) == 0 
                || *m == cposFld3 && (bits&cposCode3) == 0 
                || *m == cposFld4 && (bits&cposCode4) == 0 { /* don't mirror unused fields */ } 
            else {
                let f = Field::from(((bits & m) >> m.trailing_zeros()) as u8).mirrorV();
                bits &= !m;
                bits |= (f as u64) << m.trailing_zeros();
            }
        }
        CPos { bits }.uncompressed().compressed()
    }
}



#[cfg(test)]
mod tests {
    use crate::position::*;
    // use crate::fieldset::Field::*;
    #[test]
    fn compress_decompress_identity() {
    let p = emptyBoard()
        .place(WHITE, KING, bit(E4))
        .place(WHITE, ROOK, bit(D4))
        .place(BLACK, KING, bit(E8))
        .place(BLACK, PAWN, bit(E7))
        .unsetFlags(castlingBits + whiteToMove)
        .rehash();
    assert!(p.validEndgame());
    assert_eq!(p, p.compressed().uncompressed().rehash());
    }
}
