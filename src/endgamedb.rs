#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

use super::fieldset::*;
use super::position as P;
use super::position::CPos;
// use super::position::Move;
use super::fen::{decodeFEN, encodeFEN};
use super::position::CPosState;
use super::position::CPosState::*;
use super::position::Piece;
use super::position::Piece::*;
use super::position::Player;
use super::position::Player::*;
use super::position::Position;
use crate::position::Mirrorable;

use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::io::BufWriter;
use std::io::ErrorKind::*;
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

/// Play an example end game
pub fn play(fen: &str) -> Result<(), String> {
    let pos0 = decodeFEN(fen)?;
    let mut openFiles: HashMap<String, Box<File>> = HashMap::new();
    playPos(&pos0, &mut openFiles)
}

pub fn playPos(pos: &Position, hash: &mut HashMap<String, Box<File>>) -> Result<(), String> {
    let cpos = if pos.validEndgame() {
        Ok(pos.compressed())
    } else {
        Err(String::from("position is no valid end game."))
    }?;
    let sig = cpos.signature();
    let none: Vec<CPos> = Vec::new();
    let rpos = cpos.find(&none, "leer", hash)?;
    if rpos != cpos {
        println!(
            "{} looked up for 0x{:016x} ({})",
            sig,
            cpos.bits,
            encodeFEN(&cpos.uncompressed())
        );
    }
    println!(
        "{} found canonic 0x{:016x} ({})",
        sig,
        rpos.bits,
        encodeFEN(&rpos.uncompressed())
    );
    let s = rpos.state();
    match s {
        UNKNOWN => Err(String::from("illegal UNKNOWN state for this position")),
        MATE => {
            println!("{:?} to play finds mate", pos.turn());
            Ok(())
        }
        STALEMATE => {
            if pos.moves().len() == 0 {
                println!("{:?} to play finds stale mate", pos.turn());
            } else {
                println!(
                    "{:?} to play finds draw (insufficient material or other reasons)",
                    pos.turn()
                );
            }
            Ok(())
        }
        CAN_MATE | CAN_DRAW => {
            let mv = rpos.uncompressed().moves()[rpos.moveIndex()];
            let omv = if cpos.whiteKing() == rpos.whiteKing() {
                Ok(mv)
            } else if cpos.whiteKing().mirrorH() == rpos.whiteKing() {
                Ok(mv.mirrorH())
            } else if cpos.whiteKing().mirrorV() == rpos.whiteKing() {
                Ok(mv.mirrorV())
            } else if cpos.whiteKing().mirrorV().mirrorH() == rpos.whiteKing() {
                Ok(mv.mirrorH().mirrorV())
            } else {
                Err(format!(
                    "Can't reconstruct white king {} from white king {}",
                    cpos.whiteKing(),
                    rpos.whiteKing()
                ))
            }?;
            println!(
                "{:?} to move will enforce {} with {}",
                pos.turn(),
                if s == CAN_MATE { "mate" } else { "draw" },
                omv.showSAN(*pos)
            );
            let next = pos.apply(omv);
            playPos(&next, hash)
        }
        CANNOT_AVOID_DRAW | CANNOT_AVOID_MATE => {
            let moves = pos.moves();
            let mv = if s == CANNOT_AVOID_MATE {
                moves[0]
            } else {
                let reached = moves.iter().map(|&m| pos.apply(m).compressed()).collect::<Vec<CPos>>();
                let lookups = reached
                    .iter()
                    .map(|cp| cp.find(&none, "leer", hash))
                    .filter(|x| x.is_ok())
                    .map(|x| x.unwrap())
                    .find(|cp| cp.state() != CAN_MATE && cp.state() != MATE);
                match lookups {
                    None => moves[0],
                    Some(cp) => match (0..reached.len()).find(|&i| reached[i].canonical() == cp) {
                        Some(i) => moves[i],
                        None => moves[0],
                    },
                }
            };
            println!(
                "{:?} to move cannot avoid {} so {} {}",
                pos.turn(),
                if s == CANNOT_AVOID_MATE { "mate" } else { "draw" },
                if mv == moves[0] { "arbitrarily" } else { "intentionally" },
                mv.showSAN(*pos)
            );
            let next = pos.apply(mv);
            playPos(&next, hash)
        }
    }
}

/// Provide statistics for an endgame tablebase
pub fn stats(sig: String) -> Result<(), String> {
    let path = format!("egtb/{}.egtb", sig);
    let mut file = match File::open(&path) {
        Err(cant) => return Err(format!("can't open {} ({})", path, cant)),
        Ok(f) => f,
    };

    let mut kinds = vec![0, 0, 0, 0, 0, 0, 0, 0];
    let c0 = CPos { bits: 0 };
    let mut examples = vec![c0, c0, c0, c0, c0, c0, c0, c0];
    loop {
        match CPos::read(&mut file) {
            Err(x) if x.kind() == UnexpectedEof => break,
            Err(x) => return Err(format!("read error ({}) in {}", x, path)),
            Ok(c) => {
                let inx = c.state() as usize;
                kinds[inx] += 1;
                examples[inx] = c;
            }
        }
    }
    for i in 0..7 {
        let s = CPosState::from(i as u64);
        if kinds[i] == 0 {
            println!("{:>12} positions with status {:?}", 0, s);
        } else {
            println!(
                "{:>12} positions with status {:<20} for example {}",
                formattedSZ(kinds[i]),
                format!("{:?}", s),
                encodeFEN(&examples[i].uncompressed())
            );
        }
    }
    Ok(())
}

const maxHashed: usize = 10_000_000;

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
    let mut openFiles: HashMap<String, Box<File>> = HashMap::new();
    let mut posHash: HashMap<CPos, Vec<CPos>> = HashMap::with_capacity(maxHashed);

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
    let sig = if positions.len() > 0 {
        positions[0].signature()
    } else {
        String::from("K-K")
    };
    let path = format!("egtb/{}.egtb", sig);
    match File::open(&path) {
        Err(_) => {}
        Ok(_) => {
            return Err(format!(
                "{} seems to exist already, please remove manually to re-create",
                path
            ))
        }
    };
    println!("    Generating EGTB for {} in {}", sig, path);

    // Pass2 - sort
    print!("{} Pass 2 (sorting) ... ", sig);
    io::stdout().flush().unwrap_or_default();
    positions.sort_unstable();
    println!("done.");

    let mut pass = 2;
    let mut analyzed = vec![0, 0, 0, 0, 0, 0, 0, 0];
    let mut mateonly = true;
    loop {
        let mut cacheHits = 0;
        pass += 1;
        print!("{} Pass {} - analyzing positions ...    0% ", sig, pass);
        io::stdout().flush().unwrap_or_default();

        for i in 0..positions.len() {
            if i % 1_000_000 == 0 || i + 1 == positions.len() {
                print!("\x08\x08\x08\x08\x08\x08 {:3}% ", (i + 1) * 100 / positions.len());
                io::stdout().flush().unwrap_or_default();
            }
            let c = positions[i];
            if c.state() == UNKNOWN {
                let p = c.uncompressed();
                let player = p.turn();
                let mut fromHash = true;
                let mut reached = match posHash.get(&c) {
                    Some(vec) => vec.iter().copied().collect(),
                    None => {
                        fromHash = false;
                        let moves = p.moves();
                        moves
                            .iter()
                            .copied()
                            .map(|m| p.apply(m).compressed())
                            .collect::<Vec<CPos>>()
                    }
                };
                if fromHash {
                    cacheHits += 1;
                }
                if reached.len() == 0 {
                    let s = if p.inCheck(player) { MATE } else { STALEMATE };
                    analyzed[s as usize] += 1;
                    positions[i] = c.withState(s);
                } else {
                    // let mut modified = false;
                    let mut all_can_mate = true;
                    let mut all_can_draw = true;
                    for u in 0..reached.len() {
                        match if reached[u].state() == UNKNOWN {
                            reached[u].find(&positions, &sig, &mut openFiles)
                        } else {
                            Ok(reached[u])
                        } {
                            Err(s) => {
                                panic!(
                                    "could not find reached position because:\n{}\nfen: {}  canonical: {}  hex: \
                                     0x{:016x}",
                                    s,
                                    encodeFEN(&reached[i].uncompressed()),
                                    encodeFEN(&reached[i].canonical().uncompressed()),
                                    reached[i].canonical().bits
                                );
                            }
                            Ok(rp) => {
                                if reached[u].state() != rp.state() {
                                    reached[u] = rp;
                                    // modified = true;
                                }
                                all_can_mate = all_can_mate && rp.state() == CAN_MATE;
                                all_can_draw = all_can_draw && (rp.state() == CAN_DRAW || rp.state() == CAN_MATE);
                                match rp.state() {
                                    MATE | CANNOT_AVOID_MATE => {
                                        if mateonly {
                                            analyzed[CAN_MATE as usize] += 1;
                                            positions[i] = c.withState(CAN_MATE).withMoveIndex(u);

                                            all_can_mate = false;
                                            all_can_draw = false;
                                            break;
                                        }
                                    }
                                    STALEMATE | CANNOT_AVOID_DRAW => {
                                        if !mateonly {
                                            analyzed[CAN_DRAW as usize] += 1;
                                            positions[i] = c.withState(CAN_DRAW).withMoveIndex(u);

                                            all_can_mate = false;
                                            all_can_draw = false;
                                            break;
                                        }
                                    }
                                    _ => (),
                                }
                            }
                        }
                    }
                    if all_can_mate && mateonly {
                        analyzed[CANNOT_AVOID_MATE as usize] += 1;
                        positions[i] = c.withState(CANNOT_AVOID_MATE);
                    } else if !all_can_mate && all_can_draw && !mateonly {
                        analyzed[CANNOT_AVOID_DRAW as usize] += 1;
                        positions[i] = c.withState(CANNOT_AVOID_DRAW);
                    }
                    if positions[i].state() == UNKNOWN && (posHash.len() < posHash.capacity() || fromHash) {
                        posHash.insert(positions[i], reached);
                    } else if fromHash && positions[i].state() != UNKNOWN {
                        posHash.remove(&positions[i]);
                    }
                }
            }
        }
        println!(
            "done. Cache hits {}, hash size {}",
            formattedSZ(cacheHits),
            formattedSZ(posHash.len())
        );

        // are we done yet?
        if !mateonly && analyzed.iter().fold(0, |acc, x| acc + x) == 0 {
            println!("    Construction of end game table completed.");
            break;
        }

        mateonly =
            analyzed[MATE as usize] > 0 || analyzed[CAN_MATE as usize] > 0 || analyzed[CANNOT_AVOID_MATE as usize] > 0;
        for i in 0..analyzed.len() {
            if analyzed[i] != 0 {
                println!(
                    "    Found {} new {:?} positions.",
                    formattedSZ(analyzed[i]),
                    CPosState::from(i as u64)
                );
                analyzed[i] = 0;
            }
        }
    }

    print!("{} Pass {} - writing database ...    0% ", sig, pass + 1);
    io::stdout().flush().unwrap_or_default();
    let file = match File::create(&path) {
        Err(ioe) => {
            return Err(format!("could not create EGTB file {} ({})", path, ioe));
        }
        Ok(f) => f,
    };
    let mut bufWriter = BufWriter::new(file);
    let mut npos = 0usize;
    for i in 0..positions.len() {
        let cpos = positions[i];
        if i % 1_000_000 == 0 || i + 1 == positions.len() {
            print!("\x08\x08\x08\x08\x08\x08 {:3}% ", (i + 1) * 100 / positions.len());
            io::stdout().flush().unwrap_or_default();
        }
        if cpos.state() != UNKNOWN {
            match cpos.write_seq(&mut bufWriter) {
                Err(ioe) => {
                    return Err(format!(
                        "error writing {}th position to EGTB file {} ({})",
                        npos + 1,
                        path,
                        ioe
                    ));
                }
                Ok(_) => {
                    npos += 1;
                }
            }
        }
    }
    bufWriter
        .flush()
        .map_err(|x| format!("couldn't flush buffer ({})", x))?;
    println!("done, {} positions written to EGTB file {}.", formattedSZ(npos), path);
    Ok(())
}

fn complete(pos: &Position, index: usize, pps: &Vec<PlayerPiece>, positions: &mut Vec<CPos>) {
    if index >= pps.len() {
        if pos.valid() {
            positions.push(pos.compressed());
            // println!("{}", encodeFEN(pos));
        }
        let r = pos.applyNull();
        if r.valid() {
            positions.push(r.compressed());
            if encodeFEN(&r) == "8/8/8/8/8/8/8/3K3k b  - 0 1" {
                println!("0x{:016x}", r.compressed().bits);
            }
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

    // alert if we have the position wKd1, bKb1, wQg1 and placing a bQ
    // it looks like h1 is not in used?
    /*
    let alert = pos.kings() * pos.whites == P::bit(D1)
        && pos.kings() - pos.whites == P::bit(B1)
        && pos.queens() * pos.whites == P::bit(G1)
        && pl == BLACK
        && pc == QUEEN;
    if alert {
        println!("\nAlert: placing {:?} {:?} onto {}", pl, pc, used);
    }
    */

    for f in used {
        if pos.isEmpty(f) {
            let p = pos.place(pl, pc, P::bit(f));
            if last {
                if p.valid() {
                    positions.push(p.compressed());
                }
                let r = p.applyNull();
                /*
                if alert && f == H1 {
                    let rc = r.compressed();
                    // let rcc = rc.canonical();
                    println!(
                        "Alert: Position is valid:{}, fen: {}, hex: 0x{:016x}",
                        r.valid(),
                        encodeFEN(&rc.uncompressed()),
                        rc.bits
                    );
                }
                */
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
