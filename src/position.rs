#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code
#![allow(non_upper_case_globals)] // as well as this

//!   # The central data structure, the `Position`
//!
//! A `Position` encodes the state of the chess board.
//!
//! * which pieces do exist and where are they placed
//! * who's turn it is
//! * the castling rights of the two players
//! * whether and where a pawn can capture en-passant
//! * the number of half moves since the last pawn move or capture

use std::hash::Hash;
use std::hash::Hasher;

use super::fieldset::BitSet;
use super::fieldset::Field;
use super::fieldset::Field::*;

/// short form of BitSet::singleton

pub const fn bit(f: Field) -> BitSet { BitSet::singleton(f) }
/// short form of BitSet::bitIndex

pub fn fld(b: BitSet) -> Field { b.bitIndex() }
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
#[repr(u32)]

pub enum Player {
    BLACK,
    WHITE,
}

impl Player {
    /// the color of the opponent

    pub fn opponent(self) -> Player {

        match self {
            Player::BLACK => Player::WHITE,
            Player::WHITE => Player::BLACK,
        }
    }

    /// compute -1 or 1 without conditional branch

    pub fn factor(self) -> i32 { 2 * (self as i32) - 1 }
}

pub const BLACK: Player = Player::BLACK;

pub const WHITE: Player = Player::WHITE;

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
/// valid moves only, then it should be impossible to reach an illegal state.

#[derive(Clone, Copy, Debug)]

pub struct Position {
    /// The flags bitset informs about castling rights, en passant position and
    /// who's turn it is. In addition, whether the kings actually did
    /// castle, the 50-moves-rule ply counter and the distance to root ply
    /// counter.
    ///
    /// - If A1 is in the set, it is 'WHITE's turn, otherwise 'BLACK's.
    /// - If en passant is possible, one of  A3..H3 or A6..H6 is in the set
    /// - If castling is possible, the corresponding target fields of the Kings
    ///   are in the set. This would be G1 (white kingside), C1 (white
    ///   queenside), G8 (black kingside) or C8 (black queenside).
    /// - If castling actually was performed, F1, D1, F8 or G8 are set (the
    ///   field that the 'KING' skipped).
    /// - The bits 'A4' .. 'H4' contain the unsigned half-move counter. It is
    ///   reset to 0 on a 'PAWN' move or a capturing move. For all other moves,
    ///   it is incremented.
    /// - The bits 'A5' .. 'H5' contain the unsigned root-move counter. It is
    ///   incremented with every move, but reset to 0 after a usermove was
    ///   carried out. Hence, in searches, one can see how many levels deep in
    ///   the search we are (distance to root), while the "depth" parameter of
    ///   said search functions gives the distance to the horizon.
    ///
    /// Note: the counter bits and the F1, D1, F8 and G8 bits are not reflected
    /// in the hash key.
    flags: BitSet,

    /// the fields occupied by WHITE pieces
    whites: BitSet,

    /// For encoding of figures, we use only 3 sets instead of 6, namely        
    /// 'Position.pawnSet', Position.bishopSet' and 'Position.rookSet', with the
    /// following convention:
    ///
    /// ```ignore
    /// Set       P      B     R
    /// PAWN      x      -     -
    /// KNIGHT    x      x     -
    /// BISHOP    -      x     -
    /// ROOK      -      -     x
    /// QUEEN     -      x     x
    /// KING      x      -     x
    /// ```
    ///
    /// For example, if the bit E5 is set in the pawnSet and the rookSet, but
    /// not in the bishopSet, then there is a KING on E5. If the E5 flag is
    /// set in whites, then it is a white king, else a black king.
    pawnSet:   BitSet,
    bishopSet: BitSet,
    rookSet:   BitSet,

    /// the Zobrist hash key, a special hashing method usually employed in chess
    /// programming
    ///
    /// The following is guaranteed:
    ///
    /// ```ignore
    /// x.zobrist != y.zobrist → x != y
    /// x == y                 → x.zobrist == y.zobrist
    /// ```
    zobrist: u64,
}

/// Bitmask for selection of the ply (half-move) counter, which is used to support the 50 moves rule.
/// Note that we have room for 256 plies, but only 100 are needed.
///
/// The evaluator will return 0 for a position where the ply counter is 100 or more.
/// Also, the protocol handler will claim a draw when the move played results in a position
/// with ply counter 100 or more.
#[rustfmt::skip]
pub const plyCounterBits: BitSet = BitSet { bits: 0xFF00_0000u64 }; // A4..H4

/// how many bits to shift right to get the ply counter

pub const plyCounterShift: u32 = 24;

/// Bitmask for selection of the distance to root ply counter
/// This counter is reset to 0 before a seach starts and incremented with every move
#[rustfmt::skip]
pub const rootCounterBits: BitSet = BitSet { bits: 0xFF_0000_0000 }; // A5..H5

/// number of bits to shift right to get the ply counter

pub const rootCounterShift: u32 = 32;

/// A bitmask used to turn all the counter bits off

pub const counterBits: BitSet = plyCounterBits.union(rootCounterBits);

#[rustfmt::skip]
/// Bitmask for selection of the bits that indicate the rights to castle
pub const castlingBits: BitSet = BitSet { bits: 0x4400_0000_0000_0044 }; // G1, C1, G8, C8

#[rustfmt::skip]
/// Bitmask for selection of the bits that indicate the fields that were skipped when the king castled
pub const castlingDoneBits: BitSet = BitSet { bits: 0x2800_0000_0000_0028 }; // F1, D1, F8, D8

#[rustfmt::skip]
/// Bitmask for selection of the bits that tell us whether the white king has castled and whereto.
pub const whiteHasCastledBits: BitSet = BitSet { bits: 0x0000_0000_0000_0028 }; // F1, D1

#[rustfmt::skip]
/// Bitmask for selection of the bits that tell us whether the black king has castled and whereto.
pub const blackHasCastledBits: BitSet = BitSet { bits: 0x2800_0000_0000_0000 }; // F8, D8

/// give the bitmask that can be used to find out whether a given player has
/// castled
pub const fn playerCastledBits(p: Player) -> BitSet {

    match p {
        Player::BLACK => blackHasCastledBits,
        Player::WHITE => whiteHasCastledBits,
    }
}

#[rustfmt::skip]
/// Bitmask to select the Bits that can be targets of an en-passant capture (rank 3 and rank 6)
/// If any of those is set in the position flags, an en passant capture is possible
pub const enPassantBits: BitSet = BitSet { bits: 0x0000_FF00_00FF_0000 }; // A3-H3, A6-H6

/// constant to add or subtract 1 from both counters in one go
/// It goes without saying that we must never decrement beyond zero, nor
/// increment beyond 255.
pub const onePly: u64 = 0x1_0100_0000; // A4 and A5

/// This is used when only the root counter must be incremented (on 'PAWN' moves
/// and captures)
pub const onePlyRootOnly: u64 = 0x1_0000_0000; // A5

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
    /// get the number of `Move`s applied since the last pawn move or capture
    /// (Castling, despite technically doing 3 moves, corrects the counter
    /// acordingly)

    pub fn getPlyCounter(&self) -> u64 { (self.flags * plyCounterBits).bits >> plyCounterShift }
    /// get the number of `Move`s applied since the last root counter reset

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

    /// Increment the ply counter(s) using either `onePly` or `onePlyRootOnly`

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
    /// Only used after castling, where we actually do 3 ordinary (but illegal)
    /// moves.
    pub fn decrPlyCounters(&self) -> Position {

        Position {
            flags: BitSet {
                bits: self.flags.bits - onePly,
            },
            ..*self
        }
    }

    /// subtract 2 from the plyCounters (conveniece for castlings)

    pub fn correctPlyCounterForCastling(&self) -> Position {

        self.decrPlyCounters().decrPlyCounters()
    }

    /// the set of occupied fields

    pub fn occupied(&self) -> BitSet { self.pawnSet + self.bishopSet + self.rookSet }
    /// `true` if and only if the given `Field` is not occupied by some piece

    pub fn isEmpty(&self, f: Field) -> bool { !self.occupied().member(f) }
    /// `true` if and only if no member of the given set is an occupied field

    pub fn areEmpty(&self, fs: BitSet) -> bool { (self.occupied() * fs).null() }
    /// tell who's turn it is

    pub fn turn(&self) -> Player {

        if self.flags.member(A1) {

            WHITE
        } else {

            BLACK
        }
    }

    /// fields occupied by WHITE

    pub fn occupiedByWhite(&self) -> BitSet { self.occupied() * self.whites }
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
}

impl PartialEq for Position {
    fn eq(&self, other: &Position) -> bool {

        self.flags - counterBits == other.flags - counterBits
            && self.whites == other.whites
            && self.pawnSet == other.pawnSet
            && self.bishopSet == other.bishopSet
            && self.rookSet == other.rookSet
    }
}

impl Eq for Position {}

impl Hash for Position {
    fn hash<H: Hasher>(&self, state: &mut H) { self.zobrist.hash(state); }
}
