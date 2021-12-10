#![allow(non_snake_case)]
use super::fieldset::*;
use super::position as P;
use super::position::CPos;
// use super::position::Move;
use super::position::Piece;
use super::position::Piece::*;
use super::position::Player;
use super::position::Player::*;
use super::position::Position;

use std::io;
use std::io::Write;

pub type PlayerPiece = (Player, Piece);

/// Decode an endgame signature.
///
/// Convert a string like `KBB-KNP` into a list that contains the
/// pieces taking part in an endgame except for the 2 `Piece::KING`s
/// which are redundant. The result in this case should be a vector with
/// 2 white bishops, a black knight and a black pawn.
///
/// The convention is to list the pieces ordered by descending value.
/// The `'K'`s are optional and the white pieces must be separated from
/// the black ones by a dash. If there is more than one dash ore any
/// other character than `'K`, `'Q'`, `'R'`, `'B'`, `'N'` or `'P'` the
/// siganture is invalid and an empty vector is returned.
///
/// ```
/// use rasch::endgamedb as E;
/// use rasch::position::Player::*;
/// use rasch::position::Piece::*;
///
/// assert_eq!(E::decodeSignature("KRP-KQ".to_string()), vec![(WHITE,ROOK),(WHITE,PAWN),(BLACK,QUEEN)]);
/// assert_eq!(E::decodeSignature("blödsinn".to_string()), vec![]);
/// ```
pub fn decodeSignature(desc: String) -> Vec<PlayerPiece> {
    let mut result = Vec::new();
    let mut wer = WHITE;

    for c in desc.chars() {
        match c.to_ascii_uppercase() {
            'K' => {}
            'Q' => {
                result.push((wer, QUEEN));
            }
            'R' => {
                result.push((wer, ROOK));
            }
            'B' => {
                result.push((wer, BISHOP));
            }
            'N' => {
                result.push((wer, KNIGHT));
            }
            'P' => {
                result.push((wer, PAWN));
            }
            '-' if wer == WHITE => {
                wer = BLACK;
            }
            _ => {
                write!(io::stderr(), "invalid signature '{}': at char '{}'\n", desc, c).expect("working stderr");
                result.clear();
                break;
            }
        }
    }
    result
}

/// Generate an endgame table base
pub fn gen(sig: String) -> Result<(), String> {
    let pps = decodeSignature(sig);
    let pawns = pps.iter().fold(0, |acc, (_, p)| if *p == PAWN { acc + 1 } else { acc });
    if pps.len() == 0 {
        return Err("nothing done, invalid signature?".to_string());
    }
    let pos0 = Position {
        hash:      0,
        flags:     P::whiteToMove,
        whites:    BitSet::empty(),
        bishopSet: BitSet::empty(),
        pawnSet:   BitSet::empty(),
        rookSet:   BitSet::empty(),
    };
    // Places to use for the white king.
    // If there are no pawns, it is enough to compute the positions where
    // the king is in the lower left quarter. The remaining positions
    // can be obtained by reflecting vertically, horizontally or
    // horizontally and vertically.
    // If there are pawns, we can still restrict ourselves to positions
    // where the king is in the left half, since a single vertical
    // reflection is in order.

    #[rustfmt::skip]
    let wKbits = if pawns > 0 {
        BitSet::new(&[
            A1, B1, C1, D1, A2, B2, C2, D2, A3, B3, C3, D3, A4, B4, C4, D4, 
            A5, B5, C5, D5, A6, B6, C6, D6, A7, B7, C7, D7, A8, B8, C8, D8,
        ])
    } else {
        BitSet::new(&[A1, B1, C1, D1, A2, B2, C2, D2, A3, B3, C3, D3, A4, B4, C4, D4])
    };
    // positions of the two kings
    let vecbase = if pawns > 0 { 1806 } else { 903 };
    let vecmax = (0..pps.len()).fold(vecbase, |acc, i| {
        if let (_, PAWN) = pps[i] {
            56 * acc
        } else {
            (62 - i) * acc
        }
    }) * 2;
    println!("We provide for {} positions.", vecmax);
    let mut positions = Vec::with_capacity(vecmax);
    // place the white king
    for wk in wKbits {
        let pos1 = pos0.place(WHITE, KING, P::bit(wk));
        // place the black king
        for bk in BitSet::all() {
            if pos1.isEmpty(bk) {
                let pos2 = pos1.place(BLACK, KING, P::bit(bk));
                if pos2.valid() {
                    complete(&pos2, 0, &pps, &mut positions);
                }
            }
        }
    }
    println!("There are {} possible positions.", positions.len());
    let mut wmoves = 0u64;
    let mut bmoves = 0u64;
    let mut wmates = 0u64;
    let mut bmates = 0u64;
    let mut draws = 0u64;
    for c in positions {
        let p = c.uncompressed();
        let pl = p.turn();
        if pl == WHITE {
            wmoves += 1;
        } else {
            bmoves += 1;
        }
        if p.moves().len() == 0 {
            if p.inCheck(pl) {
                if pl == WHITE {
                    wmates += 1;
                } else {
                    bmates += 1;
                }
            } else {
                draws += 1;
            }
        }
    }
    println!("WHITE to move {}, BLACK to move {}", wmoves, bmoves);
    println!("WHITE is mate {}, BLACK is mate {}, draws {}", wmates, bmates, draws);
    Ok(())
}

fn complete(pos: &Position, index: usize, pps: &Vec<PlayerPiece>, positions: &mut Vec<CPos>) {
    if index >= pps.len() {
        return;
    }
    let last = index + 1 == pps.len();
    let (pl, pc) = pps[index];
    let bits = if pc == PAWN { P::pawnFields } else { BitSet::all() };
    for f in bits {
        if pos.isEmpty(f) {
            let p = pos.place(pl, pc, P::bit(f));
            if last {
                if p.valid() {
                    positions.push(p.compressed());
                }
                let r = p.applyNull();
                if r.valid() {
                    positions.push(r.compressed());
                }
                // for all pawns that could have moved a double step we need to make a
                // position where the player whose turn it is could
                // capture this pawn en passant.
                for pf in p.pawns() {
                    // which player's pawn?
                    let pc = if p.whites.member(pf) { WHITE } else { BLACK };
                    // If this is a white pawn, use the board where it is black's move and
                    // vice versa.
                    let ep = if pc == p.turn() { r } else { p };
                    // It must still be a valid one.
                    if ep.valid() {
                        if pc == WHITE && pf.rank() == 4 {
                            // compute the skipped field and proceed when it is empty
                            // because when it is occupied the pawn couldn't have moved a double
                            // step
                            let sf = Field::from(pf as u8 - 8);
                            if ep.isEmpty(sf) {
                                let epb = ep.setFlag(sf);
                                positions.push(epb.compressed());
                            }
                        }
                        if pc == BLACK && pf.rank() == 5 {
                            let sf = Field::from(pf as u8 + 8);
                            if ep.isEmpty(sf) {
                                let epb = ep.setFlag(sf);
                                positions.push(epb.compressed());
                            }
                        }
                    }
                }
            } else {
                complete(&p, index + 1, pps, positions);
            }
        }
    }
}
