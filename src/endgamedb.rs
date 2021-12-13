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
// use std::iter::FromIterator;

pub type PlayerPiece = (Player, Piece);

/// Helper to print big numbers nicely
pub fn formatted64(u: u64) -> String {
    let mut result = String::with_capacity(64);
    formatu64(u, &mut result);
    result
}

pub fn formattedSZ(u: usize) -> String { formatted64(u as u64) }

fn formatu64(u: u64, result: &mut String) {
    if u < 1000 {
        result.push_str(&u.to_string());
        return;
    }
    formatu64(u / 1000, result);
    let r = u % 1000;
    result.push(',');
    let b = format!("{:03}", r);
    result.push_str(&b);
}

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

pub fn fac(n: usize) -> usize {
    if n == 0 {
        1
    } else {
        n * fac(n - 1)
    }
}
/// over n 0 = 1
/// over n 1 = n
/// over n 2 = n * (n-1) / n!
pub fn over(n: usize, k: usize) -> usize {
    if k == 0 {
        1
    } else if k == 1 {
        n
    } else {
        (n + 1 - k..n + 1).fold(1, |acc, x| x * acc) / fac(k)
    }
}

/// Generate an endgame table base
pub fn gen(sig: String) -> Result<(), String> {
    let pps = decodeSignature(sig);
    let mut wPawns = 0;
    let mut bPawns = 0;
    let mut wKnights = 0;
    let mut bKnights = 0;
    let mut wBishops = 0;
    let mut bBishops = 0;
    let mut wRooks = 0;
    let mut bRooks = 0;
    let mut wQueens = 0;
    let mut bQueens = 0;

    // find count of each colour and kind
    for x in &pps {
        match x {
            (WHITE, PAWN) => {
                wPawns += 1;
            }
            (BLACK, PAWN) => {
                bPawns += 1;
            }
            (WHITE, KNIGHT) => {
                wKnights += 1;
            }
            (BLACK, KNIGHT) => {
                bKnights += 1;
            }
            (WHITE, BISHOP) => {
                wBishops += 1;
            }
            (BLACK, BISHOP) => {
                bBishops += 1;
            }
            (WHITE, ROOK) => {
                wRooks += 1;
            }
            (BLACK, ROOK) => {
                bRooks += 1;
            }
            (WHITE, QUEEN) => {
                wQueens += 1;
            }
            (BLACK, QUEEN) => {
                bQueens += 1;
            }
            (_, _) => {}
        }
    }
    /*
    if pps.len() == 0 {
        return Err("nothing done, invalid signature?".to_string());
    }
    */
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
    let wKbits = if wPawns > 0 || bPawns > 0 { P::leftHalf } else { P::lowerLeftQuarter };
    // positions of the two kings, less than 32*64 or 16*64
    let vecbase = if wPawns > 0 || bPawns > 0 { 1806 } else { 903 };
    println!("{} over {} is {}", 56, 3, over(56, 3));
    println!("{} over {} is {}", 56, 2, over(56, 2));
    println!("{} over {} is {}", 62, 2, over(62, 2));
    let vecmax = 2
        * vecbase
        * over(56 - 1, wPawns)
        * over(56 - 1 - wPawns, bPawns)
        * over(62 - 2 - wPawns - bPawns, wKnights)
        * over(62 - 2 - wPawns - bPawns - wKnights, bKnights)
        * over(62 - 3 - wPawns - bPawns - wKnights - bKnights, wBishops)
        * over(62 - 3 - wPawns - bPawns - wKnights - bKnights - wBishops, bBishops)
        * over(
            62 - 3 - wPawns - bPawns - wKnights - bKnights - wBishops - bBishops,
            wRooks,
        )
        * over(
            62 - 3 - wPawns - bPawns - wKnights - bKnights - wBishops - bBishops - wRooks,
            bRooks,
        )
        * over(
            62 - 4 - wPawns - bPawns - wKnights - bKnights - wBishops - bBishops - wRooks - bRooks,
            wQueens,
        )
        * over(
            62 - 4 - wPawns - bPawns - wKnights - bKnights - wBishops - bBishops - wRooks - bRooks - wQueens,
            bQueens,
        );

    println!("We provide for {} positions.", formattedSZ(vecmax));
    let mut positions = Vec::with_capacity(vecmax);

    // Pass1 - create all positions
    print!("Pass 1 (create all positions) ... ");
    io::stdout().flush().unwrap_or_default();
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
    println!("done: found {} possible positions.", formattedSZ(positions.len()));

    // Pass2 - sort
    print!("Pass 2 (sorting) ... ");
    io::stdout().flush().unwrap_or_default();
    positions.sort_unstable();
    println!("done.");

    // Pass 3 - remove duplicates
    print!("Pass 3 (removing duplicates) ... ");
    io::stdout().flush().unwrap_or_default();
    let mut i = 1;
    for j in 1..positions.len() {
        if positions[j] == positions[j - 1] {
            // do not copy
        } else if i < j {
            positions[i] = positions[j];
            i += 1;
        } else {
            i += 1;
        }
    }
    if i < positions.len() {
        positions.truncate(i);
    }
    println!("completed, remaining positions {}", formattedSZ(positions.len()));
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
    println!(
        "WHITE to move {}, BLACK to move {}",
        formatted64(wmoves),
        formatted64(bmoves)
    );
    println!("WHITE is mate {}, BLACK is mate {}, draws {}", wmates, bmates, draws);
    Ok(())
}

fn complete(pos: &Position, index: usize, pps: &Vec<PlayerPiece>, positions: &mut Vec<CPos>) {
    if index >= pps.len() {
        if pos.valid() {
            positions.push(pos.compressed());
        }
        let r = pos.applyNull();
        if r.valid() {
            positions.push(r.compressed());
        }
        return;
    }
    let last = index + 1 == pps.len();
    let (pl, pc) = pps[index];
    let possible = if pc == PAWN { P::pawnFields } else { BitSet::all() };

    // in the case that we have multiple pieces of the same kind and colour,
    // we avoid duplicates by setting new such pieces only on fields that
    // are greater than the maximum occupied one.
    let used = possible
        * match pos.occupied().fold(None, |acc, x| {
            if pos.pieceOn(x) == pc && pos.whites.member(x) == (pl == WHITE) {
                Some(x)
            } else {
                acc
            }
        }) {
            Some(x) => BitSet::all().filter(|f| f > &x).collect(),
            None => BitSet::all(),
        };
    for f in used {
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
