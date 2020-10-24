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

// use standard packages
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::vec::Vec;
use std::cmp::{min, max};
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

/// Compute the penalty for a hanging piece `hang` that is attacked by some piece `att`
/// depending on whether the hanging piece is defended or not
pub fn hangingPenalty(hang: Piece, att: Piece, defended: bool) -> i32 {
    let scoreh = hang.score();
    let scorea = att.score();
    match att {
        EMPTY => 0,
        KING if defended => 0,
        _otherwise => match defended {
            false => percent(70, scoreh), 
            true if scoreh > scorea => percent(70, scoreh - scorea),
            _other => 0  
        }
    }
}

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
    /// Note: the counter bits and the F1, D1, F8 and G8 bits are not
    /// reflected in the hash key.
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
    fn setFlag(&self, f: Field) -> Position {
        Position {
            flags: self.flags + bit(f),
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
        vec.into_iter().filter(|&m| self.apply(m).notInCheck()).collect()
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
    /// The penalty does not apply anymore once the player has castled.
    pub fn penaltyLazyOfficers(&self, player: Player) -> i32 {
        let castled = match player {
            WHITE => self.flags * whiteHasCastledBits,
            BLACK => self.flags * blackHasCastledBits
        };
        if castled.some() { return 0; }
        let officers = match player {
            WHITE => whiteOfficers,
            BLACK => blackOfficers
        };
        (officers * (self.bishops() + self.knights()) * self.occupiedBy(player)).card() as i32 * 17
    }

    /// Sum scores for material
    pub fn scoreMaterial(&self, player: Player) -> i32 {
        self.occupiedBy(player).map(|f| self.pieceOn(f).score()).sum()
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
                if active { percent(25, hanging) } else { hanging }
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
        let matWhite = self.scoreMaterial(WHITE);
        let matBlack = self.scoreMaterial(BLACK);
        let matDelta = matWhite - matBlack;
        // The delta increases when the difference gets greater
        // This should result in an unwillingness to exchange pieces by the weaker party
        let matRelation = percent((max(matWhite, matBlack)*100) / min(matWhite, matBlack), matDelta);
        let check = self.inCheck(self.turn());
        let checkBonus = if check { 25 } else { 0 };
        // the raw moves for player
        let mut pMoves = Vec::with_capacity(64);
        self.rawMoves(&mut pMoves);
        let playerMoves = pMoves.len() as i32;
        // the raw moves for opponent
        let mut oMoves = Vec::with_capacity(64);
        self.applyNull().rawMoves(&mut oMoves);
        let opponentMoves = oMoves.len() as i32;
        // gives for each field the least WHITE attacker
        let mut attacksByWhite = [EMPTY; 64];
        // gives for each field the least BLACK attacker
        let mut attacksByBlack = [EMPTY; 64];
        for m in pMoves {
            let wo = m.to() as usize;
            let was = m.piece();
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
        
        matRelation
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
            "P:hash=0x{:x}  flags={:x}  flags{}\n\
             whites{}\n\
             pawns {}\n\
             bishops{}  knights{}\n\
             rooks  {}  queens {} kings{}\n\
             eval={}  check={}\n\
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
            self.eval(), self.inCheck(self.turn()),
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
/// We need 20 bits:
///
/// ```text
/// KPfffpppttttttssssss
/// ```
///
/// - [K] This bit indicates whether this move caused a cut and is thus
///   a so called killer move.
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
    /// Tell if this is a killer move
    #[inline]
    pub fn killer(self) -> bool { self.mv & 0b10_000_000_000000_000000u32 != 0 }

    /// Make this a killer move
    pub fn flagKiller(self) -> Move { Move { mv: self.mv | 0b10_000_000_000000_000000u32}}

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
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result { write!(f, "{}", self.algebraic()) }
}


pub fn showMoves(moves: &[Move]) -> String {
    moves.iter().map(|x| x.algebraic()).collect::<Vec<_>>().join(" ")
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
