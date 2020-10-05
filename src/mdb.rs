#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code
#![allow(non_upper_case_globals)] // as well as this

/**
# Move Data Base

Functions for setting up tables that can later be used to find moves fast.

There is no table for Queen moves, we simply take the sum of Bishop and Rook moves.

The functions here don't take into account the general rule that the target field must not
have a figure of the same color. For example, a Bishop on c1 will always be able to
"move" to b2 and d2. This allows generation of possible moves as well as detection of coverage.

For Bishop/Rook, we return a bitmap of the fields that need to be free for the move to be
valid. Hence, 0 means the move is possible, and -1 means it is not possible (unless the board was completly empty).

*/

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

/// indicate a legal move
const LEGAL: u64 = 0;

/// indicate an illegal move
const ILLEGAL: u64 = !0;

/**
 Moves for Bishops

 This table, as well as all other ...FromTo tables,
 is indexed with `(from<<6)+to`, where from and to are field numbers 0..63

 Each entry is a bit set that indicates which fields must be empty for making the move legal.
 Thus 0 means the move is legal (except when target field is occupied by a piece of the same color),
 whereas when all bits are set it indicates it is not legal (since it cannot be the case that all fields are empty).

 Note that we don't have tables for queens, knights and kings.
 This is because queen moves are like either rook or bishop moves.
 Knights and kings, on the other hand, can never be blocked, hence it suffices
 to look in the kingTo table, for example, to find the set of fields a king can go and check if the target field is a member.
*/
static mut bishopFromTo: [u64; 64 * 64] = [ILLEGAL; 64 * 64];
static mut rookFromTo: [u64; 64 * 64] = [ILLEGAL; 64 * 64];
static mut whitePawnFromTo: [u64; 64 * 64] = [ILLEGAL; 64 * 64];
static mut blackPawnFromTo: [u64; 64 * 64] = [ILLEGAL; 64 * 64];
