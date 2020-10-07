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

use super::fieldset::BitSet;
use super::fieldset::Field;
use super::fieldset::Field::*;

/// short form of BitSet::singleton
pub const fn bit(f: Field) -> BitSet {
    BitSet::singleton(f)
}

/// short form of BitSet::bitIndex
pub fn fld(b: BitSet) -> Field {
    b.bitIndex()
}

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
///

#[derive(Clone, Copy, Debug)]
struct Position {
    /// Encodes castling, en passant position and who's turn it is.
    /// In addition, whether the kings actually did castle, the 50-moves-rule ply counter
    /// and the distance to root ply counter.
    ///
    /// - If A1 is in the set, it is 'WHITE's turn, otherwise 'BLACK's.
    /// - If en passant is possible, one of  A3..H3 or A6..H6 is in the set
    /// - If castling is possible, the corresponding target fields of the Kings
    ///   are in the set. This would be G1 (white kingside), C1 (white queenside),
    ///   G8 (black kingside) or C8 (black queenside).
    /// - If castling actually was performed, F1, D1, F8 or G8 are set (the field
    ///   that the 'KING' skipped).
    /// - The bits 'A4' .. 'H4' contain the unsigned half-move counter. It is reset to 0
    ///   on a 'PAWN' move or a capturing move. For all other moves, it is incremented.
    /// - The bits 'A5' .. 'H5' contain the unsigned root-move counter. It is incremented with
    ///   every move, but reset to 0 after a usermove was carried out. Hence, in searches,
    ///   one can see how many levels deep in the search we are (distance to root), while the
    ///   "depth" parameter of said search functions gives the distance to the horizon.
    ///
    /// Note: the counter bits and the F1, D1, F8 and G8 bits are not reflected in the hash key.
    flags: BitSet,

    /// the fields occupied by WHITE pieces
    whites: BitSet,

    /// For encoding of figures, we use only 3 sets instead of 6, namely        
    /// 'Position.pawnSet', Position.bishopSet' and 'Position.rookSet', with the following
    /// convention:
    ///
    /// ```
    /// Set       P      B     R
    /// PAWN      x      -     -
    /// KNIGHT    x      x     -
    /// BISHOP    -      x     -
    /// ROOK      -      -     x
    /// QUEEN     -      x     x
    /// KING      x      -     x
    /// ```
    ///
    /// For example, if the bit E5 is set in the pawnSet and the rookSet, but not in the
    /// bishopSet, then there is a KING on E5. If the E5 flag is set in whites,
    /// then it is a white king, else a black king.
    pawnSet: BitSet,
    bishopSet: BitSet,
    rookSet: BitSet,

    /// the Zobrist hash key, a special hashing method usually employed in chess programming
    ///
    /// The following is guaranteed:
    ///
    /// ```
    /// x.zobrist != y.zobrist → x != y
    /// x == y                 → x.zobrist == y.zobrist
    /// ```
    zobrist: BitSet,
}

/// Bitmask for selection of the ply (half-move) counter, which is used to support the 50 moves rule.
/// Note that we have room for 256 plies, but only 100 are needed.
///
/// The evaluator will return 0 for a position where the ply counter is 100 or more.
/// Also, the protocol handler will claim a draw when the move played results in a position
/// with ply counter 100 or more.
#[rustfmt::skip]
static plyCounterBits: BitSet = BitSet { bits: 0xFF00_0000u64 }; // A4..H4;

/// how many bits to shift right to get the ply counter
const plyCounterShift: u32 = 24;
