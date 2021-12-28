//! # Forsyth-Edwards-Notation
//!
//! from 'https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation Wikipedia':
//!
//! ...
//!
//! Forsyth–Edwards Notation (FEN) is a standard notation for describing
//! a particular board position of a chess game. The purpose of FEN is
//! to provide all the necessary information to restart a game from a
//! particular position.
//!
//! ...
//!
//! A FEN "record" defines a particular game position,
//! all in one text line and using only the ASCII character set.
//! A text file with only FEN data records should have the file
//! extension ".fen".
//!
//! A FEN record contains six fields. The separator between fields is a
//! space. The fields are:
//!
//! 1. Piece placement (from white's perspective). Each rank is
//! described, starting with rank 8 and ending with rank 1; within each
//! rank, the contents of each square are described from file "a"
//! through file "h". Following the Standard Algebraic Notation (SAN),
//! each piece is identified by a single letter taken from the standard
//! English names (pawn = "P", knight = "N", bishop = "B", rook = "R",
//! queen = "Q" and king = "K"). White pieces are designated using
//! upper-case letters ("PNBRQK") while black pieces use lowercase
//! ("pnbrqk"). Empty squares are noted using digits 1 through 8 (the
//! number of empty squares), and "/" separates ranks.
//!
//! 2. Active color. "w" means White moves next, "b" means Black.
//!
//! 3. Castling availability. If neither side can castle, this is "-".
//! Otherwise, this has one or more letters:
//! "K" (White can castle kingside),
//! "Q" (White can castle queenside),
//! "k" (Black can castle kingside), and/or
//! "q" (Black can castle queenside).
//!
//! 4. En passant target square in algebraic notation.
//! If there's no en passant target square, this is "-".
//! If a pawn has just made a two-square move, this is the position
//! "behind" the pawn. This is recorded regardless of whether there is a
//! pawn in position to make an en passant capture.
//!
//! 5. Halfmove clock: This is the number of halfmoves since the last
//! capture or pawn advance. This is used to determine if a draw can be
//! claimed under the fifty-move rule.
//!
//! 6. Fullmove number: The number of the full move. It starts at 1, and
//! is incremented after Black's move.

#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

use std::str;
use std::vec::Vec;

use super::fieldset as F;
use super::fieldset::Field::*;
use super::position as P;
use super::position::Piece::*;
use super::position::Player::*;

#[rustfmt::skip]
pub const fenOrder: [F::Field; 64] = [
    A8, B8, C8, D8, E8, F8, G8, H8,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A1, B1, C1, D1, E1, F1, G1, H1,
];

/// Decode a FEN string into a Position.
/// The counter fields are ignored.
///
/// ```
/// use rasch::position as P;
/// use rasch::fen as FEN;
/// assert_eq!(Ok(P::initialBoard()), FEN::decodeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w kqKQ - 0 1"));
/// ```
pub fn decodeFEN(fenString: &str) -> Result<P::Position, String> {
    let fen = fenString.split(' ').collect::<Vec<_>>();
    if fen.len() != 6 {
        return Err(String::from("cannot decode FEN, it must have 6 space separated fields"));
    }
    let mut pos = P::emptyBoard();
    pos = P::Position { flags: F::BitSet::empty(), ..pos };

    let fen1: Vec<char> = fen[0].chars().collect();
    let fen2 = fen[1];
    let fen3: Vec<char> = fen[2].chars().collect();
    let fen4 = fen[3];
    let mut fp = 0;
    let mut cp = 0;

    'decode1: while cp < fen1.len() && fp < fenOrder.len() {
        let ch = fen1[cp];
        cp += 1;
        match ch {
            '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                fp += ch as usize - '0' as usize;
                continue 'decode1;
            }
            'K' => {
                pos = pos.place(WHITE, KING, P::bit(fenOrder[fp]));
            }
            'Q' => {
                pos = pos.place(WHITE, QUEEN, P::bit(fenOrder[fp]));
            }
            'R' => {
                pos = pos.place(WHITE, ROOK, P::bit(fenOrder[fp]));
            }
            'B' => {
                pos = pos.place(WHITE, BISHOP, P::bit(fenOrder[fp]));
            }
            'N' => {
                pos = pos.place(WHITE, KNIGHT, P::bit(fenOrder[fp]));
            }
            'P' => {
                pos = pos.place(WHITE, PAWN, P::bit(fenOrder[fp]));
            }
            'k' => {
                pos = pos.place(BLACK, KING, P::bit(fenOrder[fp]));
            }
            'q' => {
                pos = pos.place(BLACK, QUEEN, P::bit(fenOrder[fp]));
            }
            'r' => {
                pos = pos.place(BLACK, ROOK, P::bit(fenOrder[fp]));
            }
            'b' => {
                pos = pos.place(BLACK, BISHOP, P::bit(fenOrder[fp]));
            }
            'n' => {
                pos = pos.place(BLACK, KNIGHT, P::bit(fenOrder[fp]));
            }
            'p' => {
                pos = pos.place(BLACK, PAWN, P::bit(fenOrder[fp]));
            }
            '/' => match fenOrder[fp] {
                A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 => {
                    continue 'decode1;
                }
                f => {
                    return Err(format!("Invalid FEN: expected placement for {}, found '{}'", f, ch));
                }
            },
            _ => {
                return Err(format!("Invalid FEN: bad placement character '{}'", ch));
            }
        };
        fp += 1;
    }
    if cp < fen1.len() {
        return Err(format!(
            "Invalid FEN: placement string too long, \"{}\" is superfluous",
            &fen1[cp..].iter().collect::<String>()
        ));
    }
    if fp < fenOrder.len() {
        return Err(format!(
            "Invalid FEN: placement for positions from {} are missing",
            fenOrder[fp]
        ));
    };

    // decode player
    match fen2 {
        "w" => pos = P::Position { flags: pos.flags + P::WHITE_TO_MOVE, ..pos },
        "b" => pos = P::Position { flags: pos.flags - P::WHITE_TO_MOVE, ..pos },
        _ => {
            return Err(format!(
                "Invalid FEN: second field must be 'w' or 'b', found \"{}\"",
                fen2
            ));
        }
    };

    //decode castling
    for ch in fen3.iter() {
        match ch {
            'K' => pos = P::Position { flags: pos.flags + P::bit(G1), ..pos },
            'Q' => pos = P::Position { flags: pos.flags + P::bit(C1), ..pos },
            'k' => pos = P::Position { flags: pos.flags + P::bit(G8), ..pos },
            'q' => pos = P::Position { flags: pos.flags + P::bit(C8), ..pos },
            '-' => pos = P::Position { flags: pos.flags - P::CASTLING_BITS, ..pos },
            _other => {
                return Err(format!("Invalid FEN: illegal char in castling field: '{}'", ch));
            }
        };
    }

    // decode en passant
    if fen4 != "-" {
        let f: F::Field = fen4.parse()?;
        pos = P::Position { flags: pos.flags + P::bit(f), ..pos };
    }
    Ok(pos.rehash())
}

/// Encode a Position as FEN String
///
/// ```
/// use rasch::position as P;
/// use rasch::fen as FEN;
/// assert_eq!(FEN::encodeFEN(&P::initialBoard()), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w QKqk - 0 1");
/// assert_eq!(FEN::decodeFEN(&FEN::encodeFEN(&P::initialBoard())), Ok(P::initialBoard()));
/// ```
pub fn encodeFEN(pos: &P::Position) -> String {
    let fen6 = "1";
    let fen5 = pos.getPlyCounter().to_string();
    let eps = pos.flags * P::EN_PASSANT_BITS;
    let fen4 = if eps.some() { P::fld(eps).to_string() } else { String::from("-") };
    let crs = pos.flags * P::CASTLING_BITS;
    let fen3 = if crs.null() {
        String::from("-")
    } else {
        crs.map(|f| match f {
            C1 => 'Q',
            G1 => 'K',
            C8 => 'q',
            G8 => 'k',
            _ => '?',
        })
        .collect()
    };
    let fen2 = String::from(if pos.turn() == WHITE { "w" } else { "b" });
    let fen1: String = fenOrder
        .iter()
        .map(|f| {
            let prefix = String::from(match f {
                A7 | A6 | A5 | A4 | A3 | A2 | A1 => "/",
                _other => "",
            });
            let p = pos.pieceOn(*f).show();
            let pP = if pos.whites.member(*f) { p } else { p.to_ascii_lowercase() };
            prefix + &pP
        })
        .collect();
    let fen1 = fen1
        .replace("--------", "8")
        .replace("-------", "7")
        .replace("------", "6")
        .replace("-----", "5")
        .replace("----", "4")
        .replace("---", "3")
        .replace("--", "2")
        .replace("-", "1");
    fen1 + " " + &fen2 + " " + &fen3 + " " + &fen4 + " " + &fen5 + " " + fen6
}
