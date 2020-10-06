#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code
#![allow(non_upper_case_globals)] // as well as this

/*!
# Move Data Base

Functions for setting up tables that can later be used to find moves fast.

There is no table for Queen moves, we simply take the sum of Bishop and Rook moves.

The functions here don't take into account the general rule that the target field must not
have a figure of the same color. For example, a Bishop on c1 will always be able to
"move" to b2 and d2. This allows generation of possible moves as well as detection of coverage.

For Bishop/Rook, we return a bitmap of the fields that need to be free for the move to be
valid. Hence, 0 means the move is possible, and -1 means it is not possible (unless the board was completly empty).

*/

use super::fieldset::*;

//                  Geometry
//      8        7        6       5         4        3       2        1
//  hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba
//  10101010 01010101 11001100 00110011 01100110 11001100 00110011 01100110
//
// A bit in a bitset corresponds to the index (0..63) that is equal to the number of
// lower significance bits.
// Example: e2 = 1000000000000(2) = 0x1000L = index 12
//          index 12 -> 1L << 12  = 0x1000L

///     the eight row
const OBEN: u64 = 0xff00000000000000u64;

///     the H file
const RECHTS: u64 = 0x8080808080808080u64;

///     the A file
const LINKS: u64 = 0x0101010101010101u64;

///     the first row
const UNTEN: u64 = 0x00000000000000ffu64;

///     when added to an index that is not on the H file, yields the index to the right, i.e. e5 -> f5
const EAST: i32 = 1;

///     when added to an index that is not on the A file, yields the index to the left, i.e. e5 -> d5
const WEST: i32 = -1;

///     when added to an index that is not on the first row, yields the index down one row, i.e e5 -> e4
const SOUTH: i32 = -8;

///     when added to an index that is not on the last row, yields the index up one row, i.e e5 -> e6
const NORTH: i32 = 8;

///     e5 -> f6
const NE: i32 = NORTH + EAST;

///     e5 -> f4
const SE: i32 = SOUTH + EAST;

///     e5 ->  d4
const SW: i32 = SOUTH + WEST;

///     e5 ->  d6
const NW: i32 = NORTH + WEST;

/// indicate a move that is not affected by other figures
const LEGAL: u64 = 0;

/// indicate an illegal move
const ILLEGAL: u64 = !0;

/**
 Moves for Bishops

 This table, as well as all other ...FromTo tables,
 is indexed with `(from<<6)+to`, where from and to are field numbers 0..63

 Each entry is a bit set that indicates which fields must be empty for making the move legal.
 Thus 0 means the move is legal, not considering occupation of the target field,
 whereas when all bits are set it indicates it is not legal
 since it cannot be the case that all fields are empty.

 Note that we don't have tables for queens, knights and kings.
 This is because queen moves are like either rook or bishop moves,
 Hence the queenFromTo values would simply be the union of rookFromTo and bishopFromTo values.

 Moves of knights and kings, on the other hand, can never be affected by the placement of
 other pieces, hence all values would be LEGAL or ILLEGAL.

*/
static mut bishopFromTo: [u64; 64 * 64] = [ILLEGAL; 64 * 64];
static mut rookFromTo: [u64; 64 * 64] = [ILLEGAL; 64 * 64];
static mut whitePawnFromTo: [u64; 64 * 64] = [ILLEGAL; 64 * 64];
static mut blackPawnFromTo: [u64; 64 * 64] = [ILLEGAL; 64 * 64];

/**
 Set of fields that can in principle be reached directly
 by a certain piece from a certain field.

 This is indexed by a field number 0..63

 The value can be used to find relevant entries in the ...FromTo tables, if applicable,
 and finally check the free fields in the current position.
*/
static mut bishopTo: [u64; 64] = [0; 64];
static mut rookTo: [u64; 64] = [0; 64];
static mut knightTo: [u64; 64] = [0; 64];
static mut kingTo: [u64; 64] = [0; 64];
static mut whitePawnTo: [u64; 64] = [0; 64];
static mut blackPawnTo: [u64; 64] = [0; 64];

/**
The inverse of whitePawnTo/blackPawnTo: if there is a black/white pawn on any of the
indicated fields, then the given field is attacked.
We need this only for pawns, since the moves of all other pieces are symmetric, that is,
for example, the set of fields an index can be attacked from with a bishop is the same as
the set of fields a bishop on index can go to.
*/
static mut whitePawnFrom: [u64; 64] = [0; 64];
static mut blackPawnFrom: [u64; 64] = [0; 64];

/**
     tell if we can go into a certain direction from some field
*/
pub fn canGo(from: u64, direction: i32) -> bool {
    match direction {
        EAST => (from & RECHTS) == 0,
        WEST => (from & LINKS) == 0,
        NORTH => (from & OBEN) == 0,
        SOUTH => (from & UNTEN) == 0,
        NE => (from & (OBEN | RECHTS)) == 0,
        SE => (from & (UNTEN | RECHTS)) == 0,
        NW => (from & (OBEN | LINKS)) == 0,
        SW => (from & (UNTEN | LINKS)) == 0,
        _ => false,
    }
}

/// Go from some field in a certain direction
/// * fld - a singleton set that denotes the field in question
/// * direction - one of NORTH, EAST, SOUTH, WEST, NE, NW, SE, SW
/// Returns the field that is one step from the given field in a given direction,
///       if there is such a field, otherwise, nonsense results.
/// This should only be done when canGo returns true

pub fn goTowards(fld: u64, direction: i32) -> u64 {
    if direction < 0 {
        fld >> (-direction)
    } else {
        fld << direction
    }
}

/// tell if we cab go 2 steps in the indicated direction
pub fn canGo2(from: u64, direction: i32) -> bool {
    canGo(from, direction) && canGo(goTowards(from, direction), direction)
}

/// convert a singleton to an index
/// (actually, it looks only for the least significant 1 bit)
/// beware not to pass 0, because then the index becomes 64
pub fn setToIndex(singleton: u64) -> usize {
    singleton.trailing_zeros() as usize
}

pub unsafe fn genPawn() {
    let mut from = 0x100u64; // no pawns on base row
    while from < 0x0100_0000_0000_0000u64 {
        if canGo(from, NORTH) {
            let to = goTowards(from, NORTH);
            whitePawnTo[setToIndex(from)] |= to;
            whitePawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = to;
            if (from & 0xff00u64) != 0 && canGo(to, NORTH) {
                // A2..H2 second rank
                let mask = to;
                let to = goTowards(to, NORTH);
                whitePawnTo[setToIndex(from)] |= to;
                whitePawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = to | mask;
            }
        }
        if canGo(from, SOUTH) {
            let to = goTowards(from, SOUTH);
            blackPawnTo[setToIndex(from)] |= to;
            blackPawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = to;
            if (from & 0x00ff000000000000u64) != 0 && canGo(to, SOUTH) {
                // A7..H7 seventh rank
                let mask = to;
                let to = goTowards(to, SOUTH);
                blackPawnTo[setToIndex(from)] |= to;
                blackPawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = to | mask;
            }
        }
        if canGo(from, NE) {
            let to = goTowards(from, NE);
            whitePawnTo[setToIndex(from)] |= to;
            whitePawnFrom[setToIndex(to)] |= from;
            whitePawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = LEGAL;
        }
        if canGo(from, NW) {
            let to = goTowards(from, NW);
            whitePawnTo[setToIndex(from)] |= to;
            whitePawnFrom[setToIndex(to)] |= from;
            whitePawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = LEGAL;
        }
        if canGo(from, SE) {
            let to = goTowards(from, SE);
            blackPawnTo[setToIndex(from)] |= to;
            blackPawnFrom[setToIndex(to)] |= from;
            blackPawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = LEGAL;
        }
        if canGo(from, SW) {
            let to = goTowards(from, SW);
            blackPawnTo[setToIndex(from)] |= to;
            blackPawnFrom[setToIndex(to)] |= from;
            blackPawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = LEGAL;
        }
        from <<= 1;
    }
}

pub fn canWhitePawn(from: Field, to: Field) -> BitSet {
    unsafe { BitSet::from(whitePawnFromTo[((from as usize) << 6) + (to as usize)]) }
}
