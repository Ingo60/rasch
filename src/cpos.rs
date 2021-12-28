#![allow(non_camel_case_types)]
//! A `CPos` is a compressed representation of a `Position`.
//! A `Signature` is the kind of an end game, for example "KQB-KNB".
//! A canonic end game is one, where `WHITE` has the better pieces.
//! The non-canonic ones are not worth extra storage space,
//! as they can be reduced to canonic ones by simply changing the colour of the pieces
//! and mirroring the board horizontally if pawns are in the game.

use std::{
    cmp::Ordering,
    collections::HashMap,
    env, fmt,
    fmt::{Display, Formatter},
    fs::File,
    hash::{Hash, Hasher},
    io::{BufReader, ErrorKind::UnexpectedEof, Read, Seek, SeekFrom, Write},
};

use crate::basic::{Move, Piece, Player};
use crate::fieldset::{BitSet, Field};
use crate::position::{bit, Position, LEFT_HALF, LOWER_HALF, WHITE_TO_MOVE};
use Field::*;
use Piece::*;
use Player::*;

pub type EgtbMap = HashMap<Signature, Box<(File, u64, CPos)>>;

/// Signature of a CPos
///
/// This is needed to employ the symmetries on the chess board in order to save lots of disk space.
/// We define a "canonical" position as follows:
/// - if there are no PAWNs in the game, the white KING is in the lower left corner (rank 1..4, file A..D)
/// - if there are PAWNs in the game, the white KING is in the left half (rank 1..8, files A..D)
///
/// Only canonical positions will be in the database.
/// All other positions with the same sets of pieces can be reduced to a canonical one by
/// - mirroring the board on the vertical axis if the king is in the right half
/// - and then, if there are no PAWNs in the game, mirroring the board on the horizontal axis
///
/// But this is not all. For example, it turns out that it doesn't matter if we have a WHITE KING on A1,
/// a BLACK KING on C2 and a BLACK ROOK on C8 or if we change the BLACK pieces to WHITE ones and the WHITE ones to BLACK
/// ones: Whatever the player with ROOK and KING is, will be able to mate if it is his turn while the other player
/// is doomed to move his KING to A2 where it is mated in the next move.
/// To get "the same" position with different colors, it is necessary to mirror the board on the horizontal axis
/// whenever pawns are present. In addition, when querying the database, care must be taken to
/// exchange the states for BLACK and WHITE whenever it was necessary to exchange colors.
///
/// Every canonical position has thus up to 3 equivalent positions where the pieces have the same colors and up to
/// 4 equivalent positions where the colours are chhanged. Taking into account that every position in the EGTB has
/// two states for the different players to move, we map up to 16 positions to one and the same 64 bit word.
/// (If there are pawns, it is only 4, though).
///
/// The question is only: which of the two positions described earlier will be in the database? We will extend our
/// definition by these rules:
/// - WHITE has the more valuable pieces, where for the sake of simplicity,
/// a queen beats any number of rooks, bishops, knights and pawns, a rook beats any number of bishops, knights and pawns,
/// and so forth. But, of course, 2 rooks are better than 1, as long as no queens are present.
/// Thus, the following are canonic signatures: `KQP-KRR` `KQ-KRRR`, whereas `KBB-KQ` is not.
/// - If both BLACK and WHITE have the same number and kinds of pieces there will be one table where both colour symmetric
/// positions are contained, so no savings here (for now).
/// To decide the question, whether a position must get colour changed on searching, we have this `Signature` type,
/// that can be created fast from a `CPos`, can be compared fast and can be used to derive a unique file name for the
/// EGTB file.
///

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Signature {
    /// encoding is 6 bits for each piece type, where the most significant bits hold the number of queens and the least
    /// significant ones the number of pawns.
    white: u32,
    /// encoding is 6 bits for each piece type, where the most significant bits hold the number of queens and the least
    /// significant ones the number of pawns.
    black: u32,
}

impl Signature {
    /// the signature for K-K
    pub const SIGNATURE_KK: Signature = Signature { white: 0, black: 0 };
    pub const PAWN_BITS: u32 = 0x3f;
    pub const PAWN_SHIFT: u32 = 0;
    pub const KNIGHT_BITS: u32 = Signature::PAWN_BITS << 6;
    pub const KNIGHT_SHIFT: u32 = 6;
    pub const BISHOP_BITS: u32 = Signature::KNIGHT_BITS << 6;
    pub const BISHOP_SHIFT: u32 = 12;
    pub const ROOK_BITS: u32 = Signature::BISHOP_BITS << 6;
    pub const ROOK_SHIFT: u32 = 18;
    pub const QUEEN_BITS: u32 = Signature::ROOK_BITS << 6;
    pub const QUEEN_SHIFT: u32 = 24;
    pub fn new(cpos: CPos) -> Signature {
        let mut white = 0u32;
        let mut black = 0u32;
        // do this 4 times for each piece code
        // (yes, this is how an unrolled loop looks)
        match (cpos.bits & CPos::CODE1) >> CPos::CODE1_SHIFT {
            1 | 7 => {
                black += 1 /* << sigPawnShift */;
            }
            9 | 15 => {
                white += 1 /* << sigPawnShift */;
            }
            2 => {
                black += 1 << Signature::KNIGHT_SHIFT;
            }
            3 => {
                black += 1 << Signature::BISHOP_SHIFT;
            }
            4 => {
                black += 1 << Signature::ROOK_SHIFT;
            }
            5 => {
                black += 1 << Signature::QUEEN_SHIFT;
            }
            10 => {
                white += 1 << Signature::KNIGHT_SHIFT;
            }
            11 => {
                white += 1 << Signature::BISHOP_SHIFT;
            }
            12 => {
                white += 1 << Signature::ROOK_SHIFT;
            }
            13 => {
                white += 1 << Signature::QUEEN_SHIFT;
            }
            _ => {}
        }
        match (cpos.bits & CPos::CODE2) >> CPos::CODE2_SHIFT {
            1 | 7 => {
                black += 1 /* << sigPawnShift */;
            }
            9 | 15 => {
                white += 1 /* << sigPawnShift */;
            }
            2 => {
                black += 1 << Signature::KNIGHT_SHIFT;
            }
            3 => {
                black += 1 << Signature::BISHOP_SHIFT;
            }
            4 => {
                black += 1 << Signature::ROOK_SHIFT;
            }
            5 => {
                black += 1 << Signature::QUEEN_SHIFT;
            }
            10 => {
                white += 1 << Signature::KNIGHT_SHIFT;
            }
            11 => {
                white += 1 << Signature::BISHOP_SHIFT;
            }
            12 => {
                white += 1 << Signature::ROOK_SHIFT;
            }
            13 => {
                white += 1 << Signature::QUEEN_SHIFT;
            }
            _ => {}
        }
        match (cpos.bits & CPos::CODE3) >> CPos::CODE3_SHIFT {
            1 | 7 => {
                black += 1 /* << sigPawnShift */;
            }
            9 | 15 => {
                white += 1 /* << sigPawnShift */;
            }
            2 => {
                black += 1 << Signature::KNIGHT_SHIFT;
            }
            3 => {
                black += 1 << Signature::BISHOP_SHIFT;
            }
            4 => {
                black += 1 << Signature::ROOK_SHIFT;
            }
            5 => {
                black += 1 << Signature::QUEEN_SHIFT;
            }
            10 => {
                white += 1 << Signature::KNIGHT_SHIFT;
            }
            11 => {
                white += 1 << Signature::BISHOP_SHIFT;
            }
            12 => {
                white += 1 << Signature::ROOK_SHIFT;
            }
            13 => {
                white += 1 << Signature::QUEEN_SHIFT;
            }
            _ => {}
        }
        match (cpos.bits & CPos::CODE4) >> CPos::CODE4_SHIFT {
            1 | 7 => {
                black += 1 /* << sigPawnShift */;
            }
            9 | 15 => {
                white += 1 /* << sigPawnShift */;
            }
            2 => {
                black += 1 << Signature::KNIGHT_SHIFT;
            }
            3 => {
                black += 1 << Signature::BISHOP_SHIFT;
            }
            4 => {
                black += 1 << Signature::ROOK_SHIFT;
            }
            5 => {
                black += 1 << Signature::QUEEN_SHIFT;
            }
            10 => {
                white += 1 << Signature::KNIGHT_SHIFT;
            }
            11 => {
                white += 1 << Signature::BISHOP_SHIFT;
            }
            12 => {
                white += 1 << Signature::ROOK_SHIFT;
            }
            13 => {
                white += 1 << Signature::QUEEN_SHIFT;
            }
            _ => {}
        }
        Signature { white, black }
    }
    pub fn from_vec(vec: &Vec<(Player, Piece)>) -> Signature {
        let mut white = 0u32;
        let mut black = 0u32;
        for pp in vec {
            match pp {
                (WHITE, QUEEN) => {
                    white += 1 << Signature::QUEEN_SHIFT;
                }
                (WHITE, ROOK) => {
                    white += 1 << Signature::ROOK_SHIFT;
                }
                (WHITE, BISHOP) => {
                    white += 1 << Signature::BISHOP_SHIFT;
                }
                (WHITE, KNIGHT) => {
                    white += 1 << Signature::KNIGHT_SHIFT;
                }
                (WHITE, PAWN) => {
                    white += 1 << Signature::PAWN_SHIFT;
                }
                (BLACK, QUEEN) => {
                    black += 1 << Signature::QUEEN_SHIFT;
                }
                (BLACK, ROOK) => {
                    black += 1 << Signature::ROOK_SHIFT;
                }
                (BLACK, BISHOP) => {
                    black += 1 << Signature::BISHOP_SHIFT;
                }
                (BLACK, KNIGHT) => {
                    black += 1 << Signature::KNIGHT_SHIFT;
                }
                (BLACK, PAWN) => {
                    black += 1 << Signature::PAWN_SHIFT;
                }
                _other => {}
            }
        }
        Signature { white, black }
    }
    /// If the `Signature` of a `CPos` is **not** canonic, then the corresponding CPos will have the
    /// colours of the pieces changed and a possible search result needs the flags switched.
    pub fn is_canonic(&self) -> bool {
        self.white >= self.black
    }
    pub fn mk_canonic(&self) -> Signature {
        if self.is_canonic() {
            *self
        } else {
            Signature { white: self.black, black: self.white }
        }
    }
    pub fn white_queens(&self) -> u32 {
        (self.white & Signature::QUEEN_BITS) >> Signature::QUEEN_SHIFT
    }
    pub fn white_rooks(&self) -> u32 {
        (self.white & Signature::ROOK_BITS) >> Signature::ROOK_SHIFT
    }
    pub fn white_bishops(&self) -> u32 {
        (self.white & Signature::BISHOP_BITS) >> Signature::BISHOP_SHIFT
    }
    pub fn white_knights(&self) -> u32 {
        (self.white & Signature::KNIGHT_BITS) >> Signature::KNIGHT_SHIFT
    }
    pub fn white_pawns(&self) -> u32 {
        self.white & Signature::PAWN_BITS
    }
    pub fn black_queens(&self) -> u32 {
        (self.black & Signature::QUEEN_BITS) >> Signature::QUEEN_SHIFT
    }
    pub fn black_rooks(&self) -> u32 {
        (self.black & Signature::ROOK_BITS) >> Signature::ROOK_SHIFT
    }
    pub fn black_bishops(&self) -> u32 {
        (self.black & Signature::BISHOP_BITS) >> Signature::BISHOP_SHIFT
    }
    pub fn black_knights(&self) -> u32 {
        (self.black & Signature::KNIGHT_BITS) >> Signature::KNIGHT_SHIFT
    }
    pub fn black_pawns(&self) -> u32 {
        self.black & Signature::PAWN_BITS
    }

    pub fn display(&self) -> String {
        let mut result = String::with_capacity(10);
        result.push('K');
        for _ in 0..self.white_queens() {
            result.push('Q');
        }
        for _ in 0..self.white_rooks() {
            result.push('R');
        }
        for _ in 0..self.white_bishops() {
            result.push('B');
        }
        for _ in 0..self.white_knights() {
            result.push('N');
        }
        for _ in 0..self.white_pawns() {
            result.push('P');
        }
        result.push('-');
        result.push('K');
        for _ in 0..self.black_queens() {
            result.push('Q');
        }
        for _ in 0..self.black_rooks() {
            result.push('R');
        }
        for _ in 0..self.black_bishops() {
            result.push('B');
        }
        for _ in 0..self.black_knights() {
            result.push('N');
        }
        for _ in 0..self.black_pawns() {
            result.push('P');
        }
        result
    }
}

impl Display for Signature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(&self.display())
    }
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
    /// not yet analyzed (this must map from and to u0!)
    UNKNOWN,

    /// no moves possible and the king is in check
    MATE,

    /// no moves possible, but not in check
    STALEMATE,

    /// there is at least 1 `KING` move in this position that reaches a position that is `MATE`,
    /// or `CANNOT_AVOID_MATE`
    CAN_MATE_K,

    /// there is at least 1 `KING` move in this position that reaches a position that is `STALEMATE`,
    /// or `CANNOT_AVOID_DRAW`
    CAN_DRAW_K,

    /// For every possible move, the opponent can answer with a move that forces a draw or a mate,
    /// but for at least one move only a draw can be enforced.
    ///
    /// Also given to positions missing from database files. The rationale is that
    /// (assuming this program is correct) if the player can neither force mate but can avoid mate for himself,
    /// then it must be a draw. And this is true for endless repetitions and lack of material.
    /// Hence, the *K-K* table is just an empty file.
    CANNOT_AVOID_DRAW,

    /// For every possible move, the opponent can answer with a move
    /// that leads to mate for this player sooner or later. Sorry!
    CANNOT_AVOID_MATE,

    /// Position is not valid for the player this flags apply to.
    /// This happens when the opposite KING would be in check or
    /// there is a PAWN of this player who is eligible for en-passant
    /// capturing. But this would only be possible when it is the
    /// other player's move.
    INVALID_POS,

    /// Like `CAN_MATE_K` but some piece must be moved.
    /// The piece to move is encoded in bits 0x0030_0000_0000_0000
    CAN_MATE_P,

    /// Like `CAN_DRAW_P` but some piece must be moved.
    /// The piece to move is encoded in bits 0x00C0_0000_0000_0000
    CAN_DRAW_P,
}

impl From<u64> for CPosState {
    /// cast a number to CPosState
    /// ```
    /// assert!((0..10).all(|n| n == rasch::position::CPosState::from(n) as u64))
    /// ```
    fn from(u: u64) -> CPosState {
        match u & 15 {
            0 => UNKNOWN,
            1 => MATE,
            2 => STALEMATE,
            3 => CAN_MATE_K,
            4 => CAN_DRAW_K,
            5 => CANNOT_AVOID_DRAW,
            6 => CANNOT_AVOID_MATE,
            7 => INVALID_POS,
            8 => CAN_MATE_P,
            9 => CAN_DRAW_P,
            _ => UNKNOWN, // to make rustc happy
        }
    }
}

pub use CPosState::*;

/// Compressed position for the endgame tablebases.

/// This is good for up to 6 pieces (kings included) and 11 bits worth of flags.
/// The flags indicate the ability for a player to force a win or a draw, or the inability to avoid a loss or a draw.
///
/// Note that the CPos does not indicate which player is to move. Hence the flags accomodate for both cases.
/// It is as if you enter a room and see a chessboard with a few pieces on it.
/// You don't normally know who is to move (except if some player is in check, then it must be that players move),
/// so you can figure out the odds for both sides.
///
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
/// - 7/15 - BLACK/WHITE PAWN that has just done a double move and can be captured en passant.
///
///
/// In order to identify equal positions, the pieces must occur from left to right in **ascending field order**.
/// This is guaranteed when compressing a `Position`.
///
/// Care must be taken when reflecting a `CPos` to get the pieces in the correct order and to correct the move index, if any.
///
/// Not all u64 values make for a valid `CPos`. The following fatal errors will abort the program:
/// - trying to compress a `Position` that is not a valid endgame as checked by `Position.validEndgame()`
/// - trying to uncompress a `CPos` that contains a wrong piece code 8, 6 or 14
/// - trying to uncompress a `CPos` where any two field numbers are equal. That is, every piece and the kings must have their own unique fields.
/// - trying to uncompress a `CPos` indicating a player to move for whom this position is invalid.
///
/// ```text
///
/// 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111
///                                                                         -- ----    field number of the white king
///                                                                  ---- --           field number of the black king
///                                                          fi eld1                   field number of 1st piece
///                                                     co d1                          code of 1st piece
///                                              fiel d2                               field number of 2nd piece
///                                         cod2                                       code of 2nd piece
///                                 fi eld3                                            field number of 3rd piece
///                            co d3                                                   code of 3rd piece
///                     fiel d4                                                        field number of 4th piece
///                cod4                                                                code of 4th piece
///           xxxx                                                                     reserved (0)
///      ffff                                                                          flags and validity indicator for WHITE
/// ffff                                                                               flags and validity indicator for BLACK
/// ```
///

#[derive(Clone, Copy)]
pub struct CPos {
    pub bits: u64,
}

impl CPos {
    /// Mask the black flag bits in a CPos
    pub const BLACK_FLAGS: u64 = 0xf000_0000_0000_0000u64;
    pub const BLACK_FLAGS_SHIFT: u32 = CPos::BLACK_FLAGS.trailing_zeros();
    /// Mask the white flag bits in a CPos
    pub const WHITE_FLAGS: u64 = 0x0f00_0000_0000_0000u64;
    pub const WHITE_FLAGS_SHIFT: u32 = CPos::WHITE_FLAGS.trailing_zeros();
    /// Mask the flag bits in a CPos
    pub const FLAGS: u64 = CPos::BLACK_FLAGS | CPos::WHITE_FLAGS;
    /// Shift for flag bits in CPos
    pub const FLAG_SHIFT: u32 = CPos::FLAGS.trailing_zeros();
    /// Mask for the indicator which piece to move for `BLACK`
    pub const BLACK_PIECE: u64 = 0x00c0_0000_0000_0000u64;
    pub const BLACK_PIECE_SHIFT: u32 = CPos::BLACK_PIECE.trailing_zeros();
    /// Mask for the indicator which piece to move for `WHITE`
    pub const WHITE_PIECE: u64 = 0x0030_0000_0000_0000u64;
    pub const WHITE_PIECE_SHIFT: u32 = CPos::WHITE_PIECE.trailing_zeros();

    /// Mask the bits that encode piece indexes
    pub const PIECES: u64 = CPos::WHITE_PIECE | CPos::BLACK_PIECE;
    /// Mask the bits that count in comparisions
    pub const COMP: u64 = !(CPos::FLAGS | CPos::PIECES);
    /// Mask the code for piece 4
    pub const CODE4: u64 = CPos::CODE3 << 10;
    /// Mask the code for piece 3
    pub const CODE3: u64 = CPos::CODE2 << 10;
    /// Mask the code for piece 2
    pub const CODE2: u64 = CPos::CODE1 << 10;
    /// Mask the code for piece 1
    pub const CODE1: u64 = 0x0f << 18;
    /// shifts to get the numbers
    pub const CODE1_SHIFT: u32 = CPos::CODE1.trailing_zeros();
    pub const CODE2_SHIFT: u32 = CPos::CODE2.trailing_zeros();
    pub const CODE3_SHIFT: u32 = CPos::CODE3.trailing_zeros();
    pub const CODE4_SHIFT: u32 = CPos::CODE4.trailing_zeros();
    /// Mask the field number of piece 4
    pub const FIELD4: u64 = CPos::FIELD3 << 10;
    /// Maskt the field number of piece 3
    pub const FIELD3: u64 = CPos::FIELD2 << 10;
    /// Maskt the field number of piece 2
    pub const FIELD2: u64 = CPos::FIELD1 << 10;
    /// Maskt the field number of piece 1
    pub const FIELD1: u64 = 0x3f << 12;
    /// Mask the field of the white `KING`
    pub const WHITE_KING: u64 = 0x0000_0000_0000_003f;
    pub const WHITE_KING_SHIFT: u32 = CPos::WHITE_KING.trailing_zeros();
    /// Mask the field of the black `KING`
    pub const BLACK_KING: u64 = 0x0000_0000_0000_0fc0;
    pub const BLACK_KING_SHIFT: u32 = CPos::BLACK_KING.trailing_zeros();

    /// Mask for both kings, useful when we need to swap them
    pub const KINGS: u64 = CPos::WHITE_KING | CPos::BLACK_KING;

    /// Compress an ordinary position, which must be a valid endgame position.
    /// Sets either INVALID_POS or UNKNOWN flags for both players.
    pub fn new(pos: &Position) -> CPos {
        // this makes sure that we never use more than 40 bits for encoding of pieces
        if !pos.validEndgame() {
            panic!("not an endgame position {}", pos);
        }

        let mut pieces = 0;
        let mut kings = 0;

        // figure out validity
        let bstate = if pos.turn() == BLACK {
            if pos.valid() {
                UNKNOWN
            } else {
                INVALID_POS
            }
        } else if pos.validForOpponent() {
            UNKNOWN
        } else {
            INVALID_POS
        };
        let wstate = if pos.turn() == WHITE {
            if pos.valid() {
                UNKNOWN
            } else {
                INVALID_POS
            }
        } else if pos.validForOpponent() {
            UNKNOWN
        } else {
            INVALID_POS
        };
        let bflags = (bstate as u64) << CPos::BLACK_FLAGS_SHIFT;
        let wflags = (wstate as u64) << CPos::WHITE_FLAGS_SHIFT;

        // `BitSet` iterator guarantees fields in ascending order
        for f in pos.occupied() {
            let p_on = pos.pieceOn(f);
            if p_on != KING {
                let color = if pos.whites.member(f) { 8 } else { 0 };
                let pcode = match p_on {
                    PAWN if f.rank() == 4 && color == 8 && pos.flags.member(Field::from(f as u8 - 8)) => 15,
                    PAWN if f.rank() == 5 && color == 0 && pos.flags.member(Field::from(f as u8 + 8)) => 7,
                    piece => color | piece as u64,
                };
                pieces <<= 4;
                pieces |= pcode;
                pieces <<= 6;
                pieces |= f as u64;
            } else {
                if pos.whites.member(f) {
                    kings |= (f as u64) << CPos::WHITE_KING_SHIFT;
                } else {
                    kings |= (f as u64) << CPos::BLACK_KING_SHIFT;
                }
            }
        }
        CPos { bits: bflags | wflags | (pieces << 12) | kings }
    }

    /// Re-construct position
    /// Before using it for anything, check validity and re-hash
    ///
    /// Beware that `cpos.uncompressed().compressed()` looses flags and piece index information.
    pub fn uncompressed(&self, player: Player) -> Position {
        let mut pos = Position {
            flags: if player == WHITE { WHITE_TO_MOVE } else { BitSet::empty() },
            whites: BitSet::empty(),
            pawnSet: BitSet::empty(),
            rookSet: BitSet::empty(),
            bishopSet: BitSet::empty(),
            hash: 0,
        };

        // place the kings
        pos = pos
            .place(WHITE, KING, bit(Field::from((self.bits & 0x3f) as u8)))
            .place(BLACK, KING, bit(Field::from(((self.bits >> 6) & 0x3f) as u8)));
        let mut pcs = self.bits >> 12;
        for _i in 0..4 {
            let f = Field::from((pcs & 0x3f) as u8);
            pcs >>= 6;
            let c = if pcs & 8 == 0 { BLACK } else { WHITE };
            let p = match pcs & 7 {
                0 => EMPTY,
                1 => PAWN,
                2 => KNIGHT,
                3 => BISHOP,
                4 => ROOK,
                5 => QUEEN,
                7 => {
                    if c == WHITE {
                        pos.flags = pos.flags + bit(Field::from(f as u8 - 8))
                    } else {
                        pos.flags = pos.flags + bit(Field::from(f as u8 + 8))
                    }
                    PAWN
                }
                _other => {
                    panic!("illegal piece code {} in compressed position", pcs & 7);
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

    pub fn signature(&self) -> Signature {
        Signature::new(*self)
    }

    pub fn player_at(&self, n: usize) -> Player {
        match n & 3 {
            0 => {
                if (self.bits & CPos::CODE1) >> CPos::CODE1_SHIFT >= 8 {
                    WHITE
                } else {
                    BLACK
                }
            }
            1 => {
                if (self.bits & CPos::CODE2) >> CPos::CODE2_SHIFT >= 8 {
                    WHITE
                } else {
                    BLACK
                }
            }
            2 => {
                if (self.bits & CPos::CODE3) >> CPos::CODE3_SHIFT >= 8 {
                    WHITE
                } else {
                    BLACK
                }
            }
            3 => {
                if (self.bits & CPos::CODE4) >> CPos::CODE4_SHIFT >= 8 {
                    WHITE
                } else {
                    BLACK
                }
            }
            _ => BLACK,
        }
    }

    pub fn piece_at(&self, n: usize) -> Piece {
        match n & 3 {
            0 => match ((self.bits & CPos::CODE1) >> CPos::CODE1_SHIFT) & 7 {
                7 => PAWN,
                p => Piece::from(p as u32),
            },
            1 => match ((self.bits & CPos::CODE2) >> CPos::CODE2_SHIFT) & 7 {
                7 => PAWN,
                p => Piece::from(p as u32),
            },
            2 => match ((self.bits & CPos::CODE3) >> CPos::CODE3_SHIFT) & 7 {
                7 => PAWN,
                p => Piece::from(p as u32),
            },
            3 => match ((self.bits & CPos::CODE4) >> CPos::CODE4_SHIFT) & 7 {
                7 => PAWN,
                p => Piece::from(p as u32),
            },
            _ => EMPTY,
        }
    }

    /// get the state from the CPos
    pub fn state(&self, player: Player) -> CPosState {
        match player {
            WHITE => CPosState::from((self.bits & CPos::WHITE_FLAGS) >> CPos::WHITE_FLAGS_SHIFT),
            BLACK => CPosState::from((self.bits & CPos::BLACK_FLAGS) >> CPos::BLACK_FLAGS_SHIFT),
        }
    }

    /// make an identical CPos with a new state
    pub fn with_state(&self, ws: CPosState, bs: CPosState) -> CPos {
        CPos {
            bits: (self.bits & !CPos::FLAGS)
                | ((ws as u64) << CPos::WHITE_FLAGS_SHIFT)
                | ((bs as u64) << CPos::BLACK_FLAGS_SHIFT),
        }
    }

    /// make an identical CPos with a new state for player
    pub fn with_state_for(&self, player: Player, ns: CPosState) -> CPos {
        if player == WHITE {
            CPos {
                bits: (self.bits & !CPos::WHITE_FLAGS) | ((ns as u64) << CPos::WHITE_FLAGS_SHIFT),
            }
        } else {
            CPos {
                bits: (self.bits & !CPos::BLACK_FLAGS) | ((ns as u64) << CPos::BLACK_FLAGS_SHIFT),
            }
        }
    }

    /// set the piece index for player
    pub fn with_piece_index_for(&self, player: Player, ix: u64) -> CPos {
        match player {
            WHITE => CPos { bits: (self.bits & !CPos::WHITE_PIECE) | ((ix & 3) << CPos::WHITE_PIECE_SHIFT) },
            BLACK => CPos { bits: (self.bits & !CPos::BLACK_PIECE) | ((ix & 3) << CPos::BLACK_PIECE_SHIFT) },
        }
    }

    /// piece CPos::index for player and move
    pub fn piece_index_by_player_mv(&self, player: Player, mv: Move) -> u64 {
        if self.piece_at(0) != EMPTY && self.player_at(0) == player && self.field_at(0) == mv.from() {
            0
        } else if self.piece_at(1) != EMPTY && self.player_at(1) == player && self.field_at(1) == mv.from() {
            1
        } else if self.piece_at(2) != EMPTY && self.player_at(2) == player && self.field_at(2) == mv.from() {
            2
        } else if self.piece_at(3) != EMPTY && self.player_at(3) == player && self.field_at(3) == mv.from() {
            3
        } else {
            0
        }
    }

    /// get the field number of the white king
    pub fn white_king(&self) -> Field {
        Field::from(self.bits & CPos::WHITE_KING)
    }

    /// get the field number of the black king
    pub fn black_king(&self) -> Field {
        Field::from((self.bits & CPos::BLACK_KING) >> CPos::BLACK_KING_SHIFT)
    }

    /// get the index for the white piece to move (0..3)
    pub fn white_piece_index(&self) -> u64 {
        (self.bits & CPos::WHITE_PIECE) >> CPos::WHITE_PIECE_SHIFT
    }

    /// get the index for the black piece to move (0..3)CPos::
    pub fn black_piece_index(&self) -> u64 {
        (self.bits & CPos::BLACK_PIECE) >> CPos::BLACK_PIECE_SHIFT
    }

    /// Get the field for piece1, piece2, piece3 or piece4
    /// Wrong input values are masked away and if the index is not occupied, it will return `A1`
    pub fn field_at(&self, index: u64) -> Field {
        match index & 3 {
            0 => Field::from((self.bits & CPos::FIELD1) >> CPos::FIELD1.trailing_zeros()),
            1 => Field::from((self.bits & CPos::FIELD2) >> CPos::FIELD2.trailing_zeros()),
            2 => Field::from((self.bits & CPos::FIELD3) >> CPos::FIELD3.trailing_zeros()),
            3 => Field::from((self.bits & CPos::FIELD4) >> CPos::FIELD4.trailing_zeros()),
            _other => A1, // be happy, rustcCPos::
        }
    }

    /// Make a canonical CPos for lookup in the DB
    /// A canonical CPos has a canonic signature and
    /// the white king is in the left half and,
    /// if there are now pawns, in the lower half.
    ///
    /// We pass the signature here explicitely to save calls to signature(), this way
    /// a client can store it when it is needed more than once.
    /// It is to be understood that only calls semantically equivalent to
    /// ```
    /// x.canonical(x.signature)
    /// ```
    /// make any sense.

    pub fn canonical(&self, sig: Signature) -> CPos {
        let has_pawns = sig.white_pawns() + sig.black_pawns() > 0;
        let mut bits = self.bits;
        let mut mirrored = false; // keep track of mirroring
        let mut this = if sig.is_canonic() {
            *self
        } else {
            // we need to flip the WHITE/BLACK bits on occupied positions
            if bits & CPos::CODE1 != 0 {
                bits ^= 8 << CPos::CODE1_SHIFT;
            }
            if bits & CPos::CODE2 != 0 {
                bits ^= 8 << CPos::CODE2_SHIFT;
            }
            if bits & CPos::CODE3 != 0 {
                bits ^= 8 << CPos::CODE3_SHIFT;
            }
            if bits & CPos::CODE4 != 0 {
                bits ^= 8 << CPos::CODE4_SHIFT;
            }
            // we also need to exchange kings and piece codes
            // we assume here that fixed black bits are further left than the corresponding white ones
            // safer, but slower, would be `(bits & CPos::BLACK_KING) >> CPos::BLACK_KING_SHIFT << CPos::WHITE_KING_SHIFT)`
            bits = (bits & !(CPos::KINGS|CPos::PIECES))     // clear affected bits
                        | ((bits & CPos::KINGS)>>6)           // move the black king
                        | ((bits & CPos::WHITE_KING)<<6)       // move the white king
                        | ((bits & CPos::BLACK_PIECE) >> 2)    // move the black piece index
                        | ((bits & CPos::WHITE_PIECE) << 2)    // move the white piece index
                        ;
            if has_pawns {
                // we need to mirror the board horizontally when pawns are present
                mirrored = true;
                CPos { bits }.mirror_h()
            } else {
                CPos { bits }
            }
        };
        let kf = this.white_king();
        if !LEFT_HALF.member(kf) {
            mirrored = true;
            this = this.mirror_v();
        }
        // the king is now in the left half
        if !has_pawns && !LOWER_HALF.member(kf) {
            this = this.mirror_h();
            mirrored = true;
        }
        if mirrored {
            this.ordered()
        } else {
            this
        }
    }

    /// convenience for `x.canonical(x.signature)`
    pub fn mk_canonical(&self) -> CPos {
        self.canonical(self.signature())
    }

    /// Returns this CPos with the flags flipped
    /// This is needed on a search result of a position that needed color changes to become canonical
    pub fn flipped_flags(&self) -> CPos {
        CPos {
            bits: (self.bits & !CPos::FLAGS) // the bits with all flags zeroed
            | ((self.bits & CPos::BLACK_FLAGS) >> 4) // add the black flags in the white flag bits
            | ((self.bits & CPos::WHITE_FLAGS) << 4), // and the white flags in the black flag bits
        }
    }

    /// for cpos pieces only!
    fn swap_pieces(bits: u64, m1: u64, m2: u64) -> u64 {
        // which pieces are swapped?
        let p1 = match m1 & (CPos::CODE1 | CPos::CODE2 | CPos::CODE3 | CPos::CODE4) {
            CPos::CODE1 => 0,
            CPos::CODE2 => 1,
            CPos::CODE3 => 2,
            CPos::CODE4 => 3,
            _else => 42,
        };
        let p2 = match m2 & (CPos::CODE1 | CPos::CODE2 | CPos::CODE3 | CPos::CODE4) {
            CPos::CODE1 => 0,
            CPos::CODE2 => 1,
            CPos::CODE3 => 2,
            CPos::CODE4 => 3,
            _else => 42,
        };
        // which pieces are currently encoded?
        let w_p = (bits & CPos::WHITE_PIECE) >> CPos::WHITE_PIECE_SHIFT; // current white piece
        let b_p = (bits & CPos::BLACK_PIECE) >> CPos::BLACK_PIECE_SHIFT; // current black piece
                                                                         // new values for piece codes
                                                                         // note, they are unaffected when both bitmasks are nCPos::ot cpos fieldsCPos::
                                                                         // or when not indexed pieces are exchanged
        let nw_p = if w_p == p1 {
            p2
        } else if w_p == p2 {
            p1
        } else {
            w_p
        };
        let nb_p = if b_p == p1 {
            p2
        } else if b_p == p2 {
            p1
        } else {
            b_p
        };

        // now make theCPos:: ALU hot
        (bits & !(CPos::PIECES|m1|m2)) // clear affected bits
            | (((bits&m1) >> m1.trailing_zeros()) << m2.trailing_zeros())  // move m1 bits to m2
            | (((bits&m2) >> m2.trailing_zeros()) << m1.trailing_zeros())  // move m2 bits to m1
            | (nw_p << CPos::WHITE_PIECE_SHIFT)  // add in new white piece index
            | (nb_p << CPos::BLACK_PIECE_SHIFT) // add in new black piece index
    }

    /// order position 1 2 and 3 in a CPos, at max 3 swaps
    fn order3(mut bits: u64) -> u64 {
        // move the smallest to position 3
        if ((bits & CPos::FIELD3) >> CPos::FIELD3.trailing_zeros())
            > ((bits & CPos::FIELD2) >> CPos::FIELD2.trailing_zeros())
        {
            bits = CPos::swap_pieces(bits, CPos::CODE3 | CPos::FIELD3, CPos::CODE2 | CPos::FIELD2);
        }
        if ((bits & CPos::FIELD3) >> CPos::FIELD3.trailing_zeros())
            > ((bits & CPos::FIELD1) >> CPos::FIELD1.trailing_zeros())
        {
            bits = CPos::swap_pieces(bits, CPos::CODE3 | CPos::FIELD3, CPos::CODE1 | CPos::FIELD1);
        }
        // ... and bring 1 and 2 in the correct order
        // swap 2 and 1, if 2 is greater
        if ((bits & CPos::FIELD2) >> CPos::FIELD2.trailing_zeros())
            > ((bits & CPos::FIELD1) >> CPos::FIELD1.trailing_zeros())
        {
            bits = CPos::swap_pieces(bits, CPos::CODE2 | CPos::FIELD2, CPos::CODE1 | CPos::FIELD1);
        }
        bits
    }

    /// Order the pieces of a CPos in such a way that field numbers are ascending from left to right.
    /// This is crucial for sorting.
    /// Could be done by uncompressing and compressing, but this should be faster. It does at max 6 swaps.
    pub fn ordered(&self) -> CPos {
        let mut bits = self.bits;
        if (bits & CPos::CODE4) != 0 {
            // swap the minimum to 4 and sort the remaining 3
            if ((bits & CPos::FIELD4) >> CPos::FIELD4.trailing_zeros())
                > ((bits & CPos::FIELD3) >> CPos::FIELD3.trailing_zeros())
            {
                bits = CPos::swap_pieces(bits, CPos::CODE4 | CPos::FIELD4, CPos::CODE3 | CPos::FIELD3);
            }
            if ((bits & CPos::FIELD4) >> CPos::FIELD4.trailing_zeros())
                > ((bits & CPos::FIELD2) >> CPos::FIELD2.trailing_zeros())
            {
                bits = CPos::swap_pieces(bits, CPos::CODE4 | CPos::FIELD4, CPos::CODE2 | CPos::FIELD2);
            }
            if ((bits & CPos::FIELD4) >> CPos::FIELD4.trailing_zeros())
                > ((bits & CPos::FIELD1) >> CPos::FIELD1.trailing_zeros())
            {
                bits = CPos::swap_pieces(bits, CPos::CODE4 | CPos::FIELD4, CPos::CODE1 | CPos::FIELD1);
            }
            CPos { bits: CPos::order3(bits) }
        } else if (bits & CPos::CODE3) != 0 {
            CPos { bits: CPos::order3(bits) }
        } else if (bits & CPos::CODE2) != 0 {
            // swap 2 and 1, if 2 is greater
            if ((bits & CPos::FIELD2) >> CPos::FIELD2.trailing_zeros())
                > ((bits & CPos::FIELD1) >> CPos::FIELD1.trailing_zeros())
            {
                bits = CPos::swap_pieces(bits, CPos::CODE2 | CPos::FIELD2, CPos::CODE1 | CPos::FIELD1);
            }
            CPos { bits }
        } else {
            CPos { bits }
        }
    }

    /// read a CPos at the current position
    pub fn read(file: &mut File) -> Result<CPos, std::io::Error> {
        let mut buf = [0u8; 8];
        file.read_exact(&mut buf)?;
        Ok(CPos { bits: u64::from_be_bytes(buf) })
    }

    /// read a CPos at the current position from a buffered reader
    pub fn read_seq(file: &mut BufReader<File>) -> Result<CPos, std::io::Error> {
        let mut buf = [0u8; 8];
        file.read_exact(&mut buf)?;
        Ok(CPos { bits: u64::from_be_bytes(buf) })
    }

    /// like `read_seq`, but maps Err(UnexpectedEOF) to OK(None) and
    /// Ok(v) to Ok(Some(v))
    pub fn read_seq_with_eof(file: &mut BufReader<File>) -> Result<Option<CPos>, std::io::Error> {
        let mut buf = [0u8; 8];
        match file.read_exact(&mut buf) {
            Ok(_) => Ok(Some(CPos { bits: u64::from_be_bytes(buf) })),
            Err(x) if x.kind() == UnexpectedEof => Ok(None),
            Err(other) => Err(other),
        }
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
    pub fn write_at(&self, file: &mut File, wo: SeekFrom) -> Result<(), std::io::Error> {
        file.seek(wo)?;
        self.write(file)
    }

    /// Find a CPos in the database files.
    ///
    /// Absent I/O errors, the result will be the canonical `CPos` that is the same as the one searched for,
    /// with appropriately set flags. Even if the entry is not in the database, this just means that the state
    /// for both colours is OTHER_DRAW and a canonical CPos with this state is returned.
    ///
    /// The databse consists of a number of files, each named after the signature of the `CPos`s it contains.
    /// To avoid frequent opening and closing of several files, `File` objects of files
    /// that have contributed in searching are remembered in the `hash` object. Only when this hash goes out
    /// of scope or entries getting removed will the corresponding file actually be closed.
    ///
    /// For a one time lookup, one can simply pass an empty hash. Subsequent searches may benefit a little from
    /// already opened files, but a real difference is seen with many thousand lookups only.
    pub fn find(&self, hash: &mut EgtbMap) -> Result<CPos, String> {
        let selfsig = self.signature();
        let canon = self.canonical(selfsig);
        if selfsig.is_canonic() {
            canon.find_canonic(selfsig, hash)
        } else {
            canon
                .find_canonic(selfsig.mk_canonic(), hash)
                .map(|r| r.flipped_flags())
        }
    }

    /// A variant of `find` where the searched `CPos` is guaranteed canonical.
    pub fn find_canonic(self, canonsig: Signature, hash: &mut EgtbMap) -> Result<CPos, String> {
        // We avoid to generate the string form of the signature at all costs
        // This is done only on errors and to find the name of the file to open.
        let blubb = hash.entry(canonsig).or_insert({
            let path = format!(
                "{}/{}.egtb",
                env::var("EGTB").unwrap_or(String::from("egtb")),
                canonsig.display()
            );
            let mut rfile = File::open(&path).map_err(|ioe| format!("could not open EGTB file {} ({})", path, ioe))?;
            let upper = rfile
                .seek(SeekFrom::End(0))
                .map_err(|ioe| format!("error seeking EGTB file {} ({})", path, ioe))?;
            let npos = upper / 8;
            let mid = npos / 2;
            let m_pos = if npos > 0 {
                // this read must not be done for empty EGTBs
                CPos::read_at(&mut rfile, SeekFrom::Start(8 * mid))
                    .map_err(|ioe| format!("error reading EGTB file {} at {} ({})", path, 8 * mid, ioe))?
            } else {
                // "remember" a fake CPos for an empty EGTB (e.g. K-K), it will never make a difference.
                // All searches will terminate immediately because the number of entries is 0.
                CPos { bits: 0 }.with_state(INVALID_POS, INVALID_POS)
            };
            Box::new((rfile, npos, m_pos))
        });

        let maxpos = blubb.1;
        let mid_cpos = blubb.2;
        let mut upper = maxpos;

        let mut lower = 0;
        while lower < upper {
            let mid = lower + (upper - lower) / 2;
            match if mid == maxpos / 2 {
                Ok(mid_cpos)
            } else {
                CPos::read_at(&mut blubb.0, SeekFrom::Start(8 * mid))
            } {
                Err(ioe) => {
                    let path = format!(
                        "{}/{}.egtb",
                        env::var("EGTB").unwrap_or(String::from("egtb")),
                        canonsig.display()
                    );
                    return Err(format!("error reading EGTB file {} at {} ({})", path, 8 * mid, ioe));
                }
                Ok(c) => {
                    if c == self {
                        return Ok(c);
                    } else if c < self {
                        lower = mid + 1;
                    } else
                    /* c >  canon */
                    {
                        upper = mid;
                    }
                }
            }
        }
        // pretend we found a DRAW
        Ok(self.with_state(CANNOT_AVOID_DRAW, CANNOT_AVOID_DRAW))
    }
}

impl fmt::Debug for CPos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&format!("{:?}/{:?}", self.state(BLACK), self.state(WHITE)))?;
        if self.piece_at(3) != EMPTY {
            f.write_str(&format!(
                ", {:?} {} {}",
                self.player_at(3),
                self.piece_at(3),
                self.field_at(3)
            ))?;
        }
        if self.piece_at(2) != EMPTY {
            f.write_str(&format!(
                ", {:?} {} {}",
                self.player_at(2),
                self.piece_at(2),
                self.field_at(2)
            ))?;
        }
        if self.piece_at(1) != EMPTY {
            f.write_str(&format!(
                ", {:?} {} {}",
                self.player_at(1),
                self.piece_at(1),
                self.field_at(1)
            ))?;
        }
        if self.piece_at(0) != EMPTY {
            f.write_str(&format!(
                ", {:?} {} {}",
                self.player_at(0),
                self.piece_at(0),
                self.field_at(0)
            ))?;
        }
        f.write_str(&format!(", BK/WK {}/{}", self.black_king(), self.white_king()))
    }
}

impl PartialEq for CPos {
    fn eq(&self, other: &CPos) -> bool {
        self.bits & CPos::COMP == other.bits & CPos::COMP
    }
}

impl Eq for CPos {}

impl PartialOrd for CPos {
    fn partial_cmp(&self, other: &CPos) -> Option<Ordering> {
        (self.bits & CPos::COMP).partial_cmp(&(other.bits & CPos::COMP))
    }
    fn lt(&self, other: &CPos) -> bool {
        (self.bits & CPos::COMP) < (other.bits & CPos::COMP)
    }
    fn le(&self, other: &CPos) -> bool {
        (self.bits & CPos::COMP) <= (other.bits & CPos::COMP)
    }
    fn gt(&self, other: &CPos) -> bool {
        (self.bits & CPos::COMP) > (other.bits & CPos::COMP)
    }
    fn ge(&self, other: &CPos) -> bool {
        (self.bits & CPos::COMP) >= (other.bits & CPos::COMP)
    }
}

impl Ord for CPos {
    fn cmp(&self, other: &CPos) -> Ordering {
        (self.bits & CPos::COMP).cmp(&(other.bits & CPos::COMP))
    }
}

impl Hash for CPos {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.bits & CPos::COMP).hash(state);
    }
}

pub trait Mirrorable
where
    Self: Sized,
{
    fn mirror_h(&self) -> Self;
    fn mirror_v(&self) -> Self;
}

impl Mirrorable for Field {
    /// Mirror the field on horizontal middle axis.
    /// Changes to corresponding rank on other side, e.g. c6 <-> c3
    fn mirror_h(&self) -> Field {
        Field::fromFR(self.file(), 9 - self.rank())
    }
    /// Mirror the field on the vertical middle axis.
    /// Changes to corresponding file on the other side, e.g. c3 <-> f3
    fn mirror_v(&self) -> Field {
        Field::fromFR((b'h' as u8 - self.file() as u8 + b'a' as u8) as char, self.rank())
    }
}

impl Mirrorable for CPos {
    /// NOTE! After calling this, the fields must be ordered by ascending field numbers,
    /// (see `CPos::ordered`) **if** the position
    /// takes part in comparisions (including hashing!).
    fn mirror_h(&self) -> CPos {
        let mut bits = self.bits;
        // for efficiency, we do this as an unrolled loop
        if bits & CPos::CODE1 != 0 {
            let m = CPos::FIELD1;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_h();
            bits = (bits & !m) | ((f as u64) << s);
        }
        if bits & CPos::CODE2 != 0 {
            let m = CPos::FIELD2;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_h();
            bits = (bits & !m) | ((f as u64) << s);
        }
        if bits & CPos::CODE3 != 0 {
            let m = CPos::FIELD3;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_h();
            bits = (bits & !m) | ((f as u64) << s);
        }
        if bits & CPos::CODE4 != 0 {
            let m = CPos::FIELD4;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_h();
            bits = (bits & !m) | ((f as u64) << s);
        }
        // the kings must be done unconditionally as they are always valid
        {
            let m = CPos::BLACK_KING;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_h();
            bits = (bits & !m) | ((f as u64) << s);
        }
        {
            let m = CPos::WHITE_KING;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_h();
            bits = (bits & !m) | ((f as u64) << s);
        }
        CPos { bits }
    }

    /// NOTE! After calling this, the fields must be ordered by ascending field numbers,
    /// where the leftmost field (field4) has the lowest field number **if** the position
    /// takes part in comparisions (including hashing!).
    fn mirror_v(&self) -> CPos {
        let mut bits = self.bits;
        // for efficiency, we do this as an unrolled loop
        if bits & CPos::CODE1 != 0 {
            let m = CPos::FIELD1;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_v();
            bits = (bits & !m) | ((f as u64) << s);
        }
        if bits & CPos::CODE2 != 0 {
            let m = CPos::FIELD2;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_v();
            bits = (bits & !m) | ((f as u64) << s);
        }
        if bits & CPos::CODE3 != 0 {
            let m = CPos::FIELD3;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_v();
            bits = (bits & !m) | ((f as u64) << s);
        }
        if bits & CPos::CODE4 != 0 {
            let m = CPos::FIELD4;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_v();
            bits = (bits & !m) | ((f as u64) << s);
        }
        // the kings must be done unconditionally as they are always present
        {
            let m = CPos::BLACK_KING;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_v();
            bits = (bits & !m) | ((f as u64) << s);
        }
        {
            let m = CPos::WHITE_KING;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_v();
            bits = (bits & !m) | ((f as u64) << s);
        }
        CPos { bits }
    }
}

impl Mirrorable for Move {
    fn mirror_h(&self) -> Self {
        let from = self.from().mirror_h();
        let to = self.to().mirror_h();
        Move::new(self.player(), self.piece(), self.promote(), from, to)
    }
    fn mirror_v(&self) -> Self {
        let from = self.from().mirror_v();
        let to = self.to().mirror_v();
        Move::new(self.player(), self.piece(), self.promote(), from, to)
    }
}
