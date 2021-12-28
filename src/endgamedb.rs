#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

use super::fen::{decodeFEN, encodeFEN};
use super::fieldset::*;
use super::position as P;
use super::position::CPos;
use super::position::CPosState;
use super::position::CPosState::*;
use super::position::Move;
use super::position::Piece;
use super::position::Piece::*;
use super::position::Player;
use super::position::Player::*;
use super::position::Position;
use super::util::*;
// use crate::position::Mirrorable;

use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::{remove_file, File};
use std::io::BufReader;
use std::io::BufWriter;
use std::io::ErrorKind::*;
use std::io::{Seek, SeekFrom, Write};
// use std::iter::FromIterator;
use std::path::Path;
use std::sync::{atomic, Arc};

pub type PlayerPiece = (Player, Piece);
pub type PosHash = HashMap<usize, Vec<(Move, CPos)>>;

const POSITION_NULL: Position = Position {
    hash: 0,
    flags: P::whiteToMove,
    whites: BitSet::empty(),
    bishopSet: BitSet::empty(),
    pawnSet: BitSet::empty(),
    rookSet: BitSet::empty(),
};

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
pub fn decodeSignature(desc: &str) -> Result<Vec<PlayerPiece>, String> {
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
            _ => return Err(format!("invalid signature '{}': at char '{}'\n", desc, c)),
        }
    }
    Ok(result)
}

/// Play an example end game
pub fn play(fen: &str) -> Result<(), String> {
    let mut pos = decodeFEN(fen)?;
    while pos.getPlyCounter() < 100 {
        match findEndgameMove(&pos) {
            Ok(mv) => {
                pos = pos.apply(mv);
                continue;
            }
            Err(s) if s == "MATE" => return Ok(()),
            Err(s) if s == "STALEMATE" => return Ok(()),
            Err(x) => return Err(x),
        };
    }
    Err(String::from("DRAW (50 moves)"))
}

#[derive(Copy, Debug, Clone, PartialEq, Eq)]
struct Dtm {
    state: P::CPosState,
    pos: P::Position,
}

/// Expand a list of CAN_MATE positions. Replace each position by the ones reached
/// that are MATE or CANNOT_AVOID_MATE
fn expand_can_mate(positions: &Vec<Dtm>, dbhash: &mut P::EgtbMap) -> Vec<Dtm> {
    let mut result = Vec::with_capacity(10 * positions.len());
    for d in positions {
        // eprintln!("# d.pos fen {}", encodeFEN(&d.pos));
        let moves = d.pos.moves();
        let reached: Vec<P::Position> = moves.iter().copied().map(|m| d.pos.apply(m)).collect();
        let states = reached
            .iter()
            .map(|p| p.compressed().find(dbhash).map(|c| c.state(p.turn())))
            .collect::<Vec<_>>();
        for rx in 0..reached.len() {
            match &states[rx] {
                Ok(s @ MATE) | Ok(s @ CANNOT_AVOID_MATE) => result.push(Dtm { state: *s, pos: reached[rx] }),
                Ok(_) => {}
                Err(e) => {
                    eprintln!("{}", e)
                }
            }
        }
    }
    result
}

/// Expand a list of CANNOT_AVOID_MATE positions. Make sure to collect only those that have not been
/// visited yet, nor equivalent positions judged by the canonical compressed position
fn expand_cannot_avoid_mate(positions: &Vec<Dtm>, visited: &mut HashSet<P::CPos>) -> Vec<Dtm> {
    let mut result = Vec::with_capacity(10 * positions.len());
    for d in positions {
        let moves = d.pos.moves();
        let reached: Vec<P::Position> = moves.iter().copied().map(|m| d.pos.apply(m)).collect();
        for r in reached {
            let cu = r.compressed();
            let cc = cu.canonical(cu.signature());
            if let Some(_) = visited.get(&cc) {
                /* noooo */
            } else {
                visited.insert(cc);
                result.push(Dtm { state: CAN_MATE_K, pos: r })
            }
        }
    }
    result
}

/// must absolutely have status CANNOT_AVOID_MATE
fn dist_to_mate(limit: u32, start: P::Position, dbhash: &mut P::EgtbMap) -> Option<u32> /* or die! */ {
    let mut visited: HashSet<CPos> = HashSet::with_capacity(1000);
    let ssig = start.compressed().signature();
    let ccpos = start.compressed().mk_canonical();
    let mut vec0 = vec![Dtm {
        state: CANNOT_AVOID_MATE,
        pos: ccpos.uncompressed(if ssig.isCanonic() { start.turn() } else { start.turn().opponent() }),
    }];
    let mut u = 0u32;
    eprintln!("# dist-to-mate: start={}", encodeFEN(&start));
    eprintln!("# dist-to-mate:   vec={}", encodeFEN(&vec0[0].pos));
    loop {
        let vec1 = expand_cannot_avoid_mate(&vec0, &mut visited);
        vec0 = expand_can_mate(&vec1, dbhash);
        u += 1;
        eprintln!("# dist-to-mate in {}: found {} new CAN-MATE positions.", u, vec0.len());
        match vec0.iter().find(|x| x.state == MATE) {
            Some(_) => break Some(u),
            None => {}
        }
        if u >= limit {
            break None;
        }
    }
}

/// we absolutely must have status CAN_MATE here!
fn move_to_mate(start: &P::Position, dbhash: &mut P::EgtbMap) -> (P::Move, u32) {
    let mut result = None;
    for m in start.moves() {
        let reached = start.apply(m);
        match reached.compressed().find(dbhash).map(|c| c.state(reached.turn())) {
            Ok(MATE) => return (m, 1),
            Ok(CANNOT_AVOID_MATE) => match result {
                None => {
                    for u in 10.. {
                        if let Some(n) = dist_to_mate(u, reached, dbhash) {
                            result = Some((m, n + 1));
                            eprintln!("# move-to-mate: initial move {} in {}", m.showSAN(*start), n + 1);
                            break;
                        };
                    }
                }
                Some((_, r)) => match dist_to_mate(r, reached, dbhash) {
                    Some(n) if n + 1 < r => {
                        eprintln!("# move-to-mate: better move {} in {}", m.showSAN(*start), n + 1);
                        result = Some((m, n + 1))
                    }
                    _ => {}
                },
            },
            _ => {}
        }
    }
    result.unwrap()
}

pub fn findEndgameMove(pos: &Position) -> Result<Move, String> {
    let cpos = if pos.validEndgame() {
        Ok(pos.compressed())
    } else {
        Err(String::from("position is no valid end game."))
    }?;
    let mut hash: P::EgtbMap = HashMap::new();
    let sig = cpos.signature();
    eprintln!("# {} is canonic {}", sig, sig.isCanonic());
    let rpos = cpos.find(&mut hash)?;
    if rpos != cpos {
        eprintln!(
            "# {} looked for    0x{:016x}  {:?}/{:?}  fen: {}",
            sig,
            cpos.bits,
            cpos.state(BLACK),
            cpos.state(WHITE),
            encodeFEN(&cpos.uncompressed(pos.turn()))
        );
    }
    eprintln!(
        "# {} found canonic 0x{:016x}  {:?}/{:?}  fen: {}",
        rpos.signature(),
        rpos.bits,
        rpos.state(BLACK),
        rpos.state(WHITE),
        encodeFEN(&rpos.uncompressed(pos.turn()))
    );

    let s = rpos.state(pos.turn());
    let other = pos.turn().opponent();

    match s {
        UNKNOWN | INVALID_POS => Err(format!("illegal {:?} state for this position", s)),
        MATE => {
            eprintln!("# {:?} to play finds mate", pos.turn());
            Err(String::from("MATE"))
        }
        STALEMATE => {
            eprintln!("# {:?} to play finds stale mate", pos.turn());
            Err(String::from("STALEMATE"))
        }
        CAN_MATE_P | CAN_MATE_K => match move_to_mate(pos, &mut hash) {
            (mv, u) => {
                eprintln!(
                    "# {:?} to play will enforce mate in {:?} with {}",
                    pos.turn(),
                    u,
                    mv.showSAN(*pos)
                );
                Ok(mv)
            }
        },
        CAN_DRAW_P | CAN_DRAW_K => {
            match pos.moves().iter().copied().find(|&mv| {
                pos.apply(mv) // erreichte Position
                    .compressed() // komprimiert
                    .find(&mut hash) // Ok(rp) oder Err()
                    .ok() // Some(rp) oder None
                    .map(|r| match r.state(other) {
                        STALEMATE | CANNOT_AVOID_DRAW => true,
                        _other => false,
                    })
                    .unwrap_or(false)
            }) {
                Some(mv) => {
                    eprintln!(
                        "# {:?} to move will enforce {} with {}",
                        pos.turn(),
                        if s == CAN_MATE_P || s == CAN_MATE_K { "mate" } else { "draw" },
                        mv.showSAN(*pos)
                    );
                    Ok(mv)
                }
                None => Err(format!("{:?} was promised, but cannot be reached", s)),
            }
        }
        CANNOT_AVOID_DRAW | CANNOT_AVOID_MATE => {
            let moves = pos.moves();
            let mv = match moves.iter().copied().find(|&mv| {
                pos.apply(mv) // erreichte Position
                    .compressed() // komprimiert
                    .find(&mut hash) // Ok(rp) oder Err()
                    .ok() // Some(rp) oder None
                    .map(|r| match r.state(other) {
                        CAN_MATE_K | CAN_MATE_P => false,
                        STALEMATE => false,
                        _other => true,
                    })
                    .unwrap_or(false)
            }) {
                None => moves[0],
                Some(mv) => mv,
            };
            eprintln!(
                "# {:?} to move cannot avoid {} so {} {}",
                pos.turn(),
                if s == CANNOT_AVOID_MATE { "mate" } else { "draw" },
                if mv == moves[0] { "arbitrarily" } else { "intentionally" },
                mv.showSAN(*pos)
            );
            Ok(mv)
        }
    }
}

/// Provide statistics for an endgame tablebase
pub fn stats(sig: String) -> Result<(), String> {
    let dbfile = format!("{}/{}", env::var("EGTB").unwrap_or(String::from("egtb")), sig);
    let egtbfile = format!("{}/{}.egtb", env::var("EGTB").unwrap_or(String::from("egtb")), sig);
    let path = if Path::new(&sig).is_file() {
        sig
    } else if Path::new(&dbfile).is_file() {
        dbfile
    } else {
        egtbfile
    };
    let file = match File::open(&path) {
        Err(cant) => return Err(format!("can't open {} ({})", path, cant)),
        Ok(f) => f,
    };
    let mut rdr = BufReader::with_capacity(8 * 1024 * 1024, file);

    let mut wkinds = vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    let mut bkinds = vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    let c0 = CPos { bits: 0 };
    let mut wexamples = vec![c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0];
    let mut bexamples = vec![c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0];
    let mut last = c0;
    let mut total = 0usize;
    let mut sorted = true;
    let mut duplic = false;
    loop {
        match CPos::read_seq(&mut rdr) {
            Err(x) if x.kind() == UnexpectedEof => break,
            Err(x) => return Err(format!("read error ({}) in {}", x, path)),
            Ok(c) => {
                let inx = c.state(WHITE) as usize;
                wkinds[inx] += 1;
                wexamples[inx] = c;
                let inx = c.state(BLACK) as usize;
                bkinds[inx] += 1;
                bexamples[inx] = c;

                sorted = sorted && last <= c;
                duplic = duplic || last == c;
                last = c;
                total += 1;
            }
        }
    }
    for i in 1..15 {
        let s = CPosState::from(i as u64);
        if wkinds[i] > 0 {
            eprintln!(
                "{:>12} white positions with status {:<20} for example {}",
                formatted_sz(wkinds[i]),
                format!("{:?}", s),
                encodeFEN(&wexamples[i].uncompressed(WHITE))
            );
        }
        if bkinds[i] > 0 {
            eprintln!(
                "{:>12} black positions with status {:<20} for example {}",
                formatted_sz(bkinds[i]),
                format!("{:?}", s),
                encodeFEN(&bexamples[i].uncompressed(BLACK))
            );
        }
    }
    eprintln!("{:>12} positions total", formatted_sz(total));
    if wkinds[UNKNOWN as usize] + bkinds[UNKNOWN as usize] > 0 {
        eprintln!("Warning: the table contains positions with UNKNOWN state.");
    }
    if !sorted {
        eprintln!("Warning: the table is not sorted.");
    }
    if duplic {
        eprintln!("Warning: the table contains duplicates.");
    }
    if wkinds[UNKNOWN as usize] > 0 || bkinds[UNKNOWN as usize] > 0 || !sorted || duplic {
        eprintln!("Warning: This is an invalid or yet incomplete end game table.");
    }

    Ok(())
}

/// Allocate the memory needed for in-memory processing but not more than twice the vector length,
/// since we need at most 2 hash entries per position.
pub fn alloc_working_memory(sig: &str, vec: &Vec<CPos>, hash: &mut PosHash) -> Result<usize, String> {
    let hsize = compute_cache_entries(vec.len()).min(2 * vec.len());
    hash.try_reserve(hsize).map_err(|ioe| {
        format!(
            "Cannot reserve space for {} hasmap entries ({}).",
            formatted_sz(hsize),
            ioe
        )
    })?;
    eprintln!("{} reserved space for {} hash entries.", sig, formatted_sz(hsize));
    Ok(hsize)
}

/// Generate an endgame table base.
///
/// We have 3 modi here:
/// 1. A small EGTB is computed in memory and written to disk
/// 2. the positions of a large EGTB are written to disk file, unsorted.
/// 3. A sorted file of compressed files is found and subsequently processed in either memory or on disk.
///
/// In case 1, if the processing is interrupted with Ctrl+C, a sorted file of positions processed so far
/// is written and will be found and continued to get processed the next time.
///
/// An EGTB basically counts as small if we can allocate a vector of the required size.
/// However, for smooth processing we also want to have a hash.
///
pub fn gen(sig: String) -> Result<(), String> {
    let ppsu = decodeSignature(&sig)?;
    let ppssig = P::Signature::fromVec(&ppsu);
    let pps = if ppssig.isCanonic() {
        ppsu
    } else {
        // let's switch the colours
        ppsu.iter().copied().map(|(p, x)| (p.opponent(), x)).collect::<Vec<_>>()
    };
    let signature = ppssig.mkCanonic();
    let sig = signature.display();

    let egtbPath = format!("{}/{}.egtb", env::var("EGTB").unwrap_or(String::from("egtb")), sig);
    let rawPath = format!("{}/{}.unsorted", env::var("EGTB").unwrap_or(String::from("egtb")), sig);
    let sortPath = format!("{}/{}.sorted", env::var("EGTB").unwrap_or(String::from("egtb")), sig);

    if Path::new(&egtbPath).is_file() {
        eprintln!(
            "{} seems to exist already, please remove manually to re-create",
            egtbPath
        );
        return Ok(());
    };
    let restart = Path::new(&sortPath).is_file();
    // Places to use for the white king.
    // If there are no pawns, it is enough to compute the positions where
    // the king is in the lower left quarter. The remaining positions
    // can be obtained by reflecting vertically, horizontally or
    // horizontally and vertically.
    // If there are pawns, we can still restrict ourselves to positions
    // where the king is in the left half, since a single vertical
    // reflection is in order.

    let mut positions = Vec::with_capacity(0);

    // Pass1 - create all positions
    eprint!(
        "{} Pass 1 - {} ",
        sig,
        if restart { "restore previous positions" } else { "create all positions" }
    );

    let inMemory = if restart {
        let mut file =
            File::open(&sortPath).map_err(|ioe| format!("Can't read source file {} ({})", &sortPath, ioe))?;
        let bytes = file
            .seek(SeekFrom::End(0))
            .map_err(|ioe| format!("error seeking checkpoint file {} ({})", &sortPath, ioe))?;
        file.seek(SeekFrom::Start(0))
            .map_err(|ioe| format!("error seeking checkpoint file {} ({})", &sortPath, ioe))?;
        let vecmax = (bytes as usize) / SIZE_CPOS;
        let mut brdr = BufReader::new(file);
        match positions.try_reserve_exact(vecmax) {
            Ok(_) => {
                loop {
                    match CPos::read_seq_with_eof(&mut brdr) {
                        Ok(None) => break,
                        Ok(Some(cp)) => positions.push(cp),
                        Err(ioe) => return Err(format!("error reading checkpoint file {} ({})", &sortPath, ioe)),
                    }
                }
                eprintln!("{} positions found.", formatted_sz(vecmax));
                true
            }
            Err(_) => {
                eprintln!(
                    "restart failed, {} too big ({} positions).",
                    sortPath,
                    formatted_sz(vecmax)
                );
                false
            }
        }
    } else {
        let vecmax = expected_positions(signature);
        eprint!("({} are expected) ", formatted_sz(vecmax));
        let wKbits = if signature.whitePawns() > 0 || signature.blackPawns() > 0 {
            P::leftHalf
        } else {
            P::lowerLeftQuarter
        };
        let mut sink = match positions.try_reserve_exact(vecmax) {
            Ok(_) => {
                eprint!("in memory ... ");
                Sink::V(&mut positions)
            }
            Err(_) => {
                eprint!("in {} ... ", rawPath);
                let f = File::create(&rawPath).map_err(|e| format!("Can't create {} ({})", &rawPath, e))?;
                Sink::W(BufWriter::new(f))
            }
        };
        for wk in wKbits {
            let pos1 = POSITION_NULL.place(WHITE, KING, P::bit(wk));
            // place the black king
            for bk in BitSet::all() {
                if pos1.isEmpty(bk) {
                    let pos2 = pos1.place(BLACK, KING, P::bit(bk));
                    if pos2.valid() {
                        complete(&pos2, 0, &pps, &mut sink);
                    }
                }
            }
        }
        sink.flush();

        if let Sink::V(_) = sink {
            eprintln!("done: found {} possible positions.", formatted_sz(positions.len()));
            if positions.len() > vecmax as usize {
                eprintln!("WARNING: vecmax was calculated too low!");
            } else if positions.len() < vecmax as usize {
                positions.shrink_to_fit();
            }
            true
        } else {
            eprintln!("done");
            false
        }
    };

    // for now
    if !inMemory {
        return Err(String::from("the rest of the processing is not implemented yet."));
    }

    eprintln!("    Generating EGTB for {} in {}", sig, egtbPath);

    let mut pass = 2usize;

    // Pass 2 - sorting, only needed if no restart
    if !restart {
        eprint!("{} Pass {} (sorting) ... ", sig, pass);
        positions.sort_unstable();
        eprintln!("done.");
        pass += 1;
    }

    let mut analyzed = vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    let mut mateonly = true;
    let mut openFiles: P::EgtbMap = HashMap::new();
    let npositions = positions.len();
    let mut posHash: PosHash = HashMap::with_capacity(0);
    let maxHashCap = alloc_working_memory(&sig, &positions, &mut posHash)?;

    let sigint_received = Arc::new(atomic::AtomicBool::new(false));
    let handler_ref = Arc::clone(&sigint_received);

    ctrlc::set_handler(move || {
        handler_ref.store(true, atomic::Ordering::SeqCst);
    })
    .map_err(|e| format!("Cannot set CTRL-C handler ({})", e))?;

    'pass: while !sigint_received.load(atomic::Ordering::SeqCst) {
        let mut cacheHits = 0usize;
        let mut cacheLookups = 0usize;

        eprint!(
            "{} Pass {} - analyzing {} positions ...    0% ",
            sig,
            pass,
            if mateonly { "mate" } else { "draw" }
        );
        pass += 1;

        for i in 0..npositions {
            if (i % 100) == 0 && sigint_received.load(atomic::Ordering::SeqCst) {
                eprintln!(" canceled.");
                continue 'pass;
            }
            if i % 500_000 == 0 || i + 1 == npositions {
                eprint!("\x08\x08\x08\x08\x08\x08 {:3}% ", (i + 1) * 100 / positions.len());
            }

            // do the following for BLACK, then for WHITE
            // a bit clumsy, as we can't implement trait Step for Player right now.
            let black = 0;
            let white = 1;
            for colour in black..=white {
                let player = Player::from(colour != black);
                let other = player.opponent();
                let mut c = positions[i];

                if c.state(player) == UNKNOWN {
                    let p = c.uncompressed(player);
                    let key = 2 * i + player as usize;
                    let hashLen = posHash.len();
                    cacheLookups += 1;
                    cacheHits += 1;
                    let reached = posHash.entry(key).or_insert_with(|| {
                        cacheHits -= 1;
                        p.moves()
                            .iter()
                            .copied()
                            .map(|m| (m, p.apply(m).compressed()))
                            .collect::<Vec<(Move, CPos)>>()
                    });

                    let mut all_can_mate = true;
                    let mut all_can_draw = true;
                    let mut all_unknown = true;
                    if reached.len() == 0 {
                        let s = if p.inCheck(player) { MATE } else { STALEMATE };
                        analyzed[s as usize] += 1;
                        c = c.with_state_for(player, s);
                    } else {
                        'children: for u in 0..reached.len() {
                            let child = reached[u].1;
                            let chsig = child.signature();
                            let canon = child.canonical(chsig);
                            let csig = canon.signature();
                            let alien = csig != signature;
                            let rp = if child.state(other) == UNKNOWN {
                                if !alien {
                                    // look in positions only
                                    match positions.binary_search(&canon) {
                                        Ok(u) => Ok(positions[u]),
                                        Err(_) => Err(format!("cannot happen alien={}", alien)),
                                    }
                                } else {
                                    // must be alien
                                    canon.find_canonic(csig, &mut openFiles).map_err(|s| {
                                        format!(
                                        "could not find reached position because:\n{}\nfen: {}  canonical: {}  hex: \
                                    0x{:016x}",
                                        s,
                                        encodeFEN(&reached[u].1.uncompressed(other)),
                                        encodeFEN(&canon.uncompressed(other)),
                                        canon.bits
                                    )
                                    })
                                }
                            } else {
                                Ok(reached[u].1)
                            }?;
                            let rpstate = rp.state(other);
                            if reached[u].1.state(other) != rpstate {
                                reached[u].1 = rp;
                            }
                            all_unknown = all_unknown && rpstate == UNKNOWN;
                            all_can_mate = all_can_mate && (rpstate == CAN_MATE_K || rpstate == CAN_MATE_P);
                            all_can_draw = all_can_draw && (rpstate == CAN_DRAW_K || rpstate == CAN_DRAW_P);
                            match rpstate {
                                MATE | CANNOT_AVOID_MATE => {
                                    if mateonly {
                                        let mv = reached[u].0;
                                        let can_mate = if mv.piece() == KING { CAN_MATE_K } else { CAN_MATE_P };
                                        let idx = c.piece_index_by_player_mv(player, mv);
                                        analyzed[can_mate as usize] += 1;
                                        c = c.with_state_for(player, can_mate).with_piece_index_for(player, idx);

                                        all_can_mate = false;
                                        all_can_draw = false;
                                        break 'children;
                                    }
                                }
                                STALEMATE | CANNOT_AVOID_DRAW => {
                                    if !mateonly {
                                        let mv = reached[u].0;
                                        let can_draw = if mv.piece() == KING { CAN_DRAW_K } else { CAN_DRAW_P };
                                        let idx = c.piece_index_by_player_mv(player, mv);
                                        analyzed[can_draw as usize] += 1;
                                        c = c.with_state_for(player, can_draw).with_piece_index_for(player, idx);

                                        all_can_mate = false;
                                        all_can_draw = false;
                                        break 'children;
                                    }
                                }
                                _ => (),
                            }
                        }
                        if all_can_mate && mateonly {
                            analyzed[CANNOT_AVOID_MATE as usize] += 1;
                            c = c.with_state_for(player, CANNOT_AVOID_MATE);
                        } else if !all_can_mate && all_can_draw && !mateonly {
                            analyzed[CANNOT_AVOID_DRAW as usize] += 1;
                            c = c.with_state_for(player, CANNOT_AVOID_DRAW);
                        }
                    }

                    if c.state(player) != UNKNOWN
                        || (all_unknown && maxHashCap < 2 * npositions)
                        || hashLen >= maxHashCap
                    {
                        posHash.remove(&key);
                    }

                    if c.state(player) != UNKNOWN {
                        positions[i] = c;
                    }
                } // unknown state
            } // black/white
        } // loop over positions
        eprintln!(
            "done. Cache hit rate {}%, new hash size {}",
            if cacheLookups > 0 { cacheHits * 100 / cacheLookups } else { 100 },
            formatted_sz(posHash.len())
        );

        // are we done yet?
        if !mateonly && analyzed.iter().fold(0, |acc, x| acc + x) == 0 {
            eprintln!("    Construction of end game table completed.");
            break;
        }

        mateonly = analyzed[MATE as usize] > 0
            || analyzed[CAN_MATE_P as usize] > 0
            || analyzed[CAN_MATE_K as usize] > 0
            || analyzed[CANNOT_AVOID_MATE as usize] > 0;
        for i in 0..analyzed.len() {
            if analyzed[i] != 0 {
                eprintln!(
                    "    Found {} new {:?} positions.",
                    formatted_sz(analyzed[i]),
                    CPosState::from(i as u64)
                );
                analyzed[i] = 0;
            }
        }
    }

    let interrupted = sigint_received.load(atomic::Ordering::SeqCst);
    let fkind = if interrupted { "checkpoint" } else { "EGBT" };
    let writePath = if interrupted { sortPath.clone() } else { egtbPath.clone() };
    eprint!("{} Pass {} - writing {} ...    0% ", sig, pass + 1, fkind);
    let file =
        File::create(&writePath).map_err(|ioe| format!("could not create {} file {} ({})", fkind, writePath, ioe))?;
    let mut bufWriter = BufWriter::new(file);
    let mut npos = 0usize;
    for i in 0..positions.len() {
        let mut cpos = positions[i];
        if i % 1_000_000 == 0 || i + 1 == positions.len() {
            eprint!("\x08\x08\x08\x08\x08\x08 {:3}% ", (i + 1) * 100 / positions.len());
        }
        // make states sane
        if !interrupted && cpos.state(WHITE) == UNKNOWN && cpos.state(BLACK) != UNKNOWN {
            cpos = cpos.with_state_for(WHITE, CANNOT_AVOID_DRAW);
        } else if !interrupted && cpos.state(WHITE) != UNKNOWN && cpos.state(BLACK) == UNKNOWN {
            cpos = cpos.with_state_for(BLACK, CANNOT_AVOID_DRAW);
        }
        // filter superfluous
        if interrupted || cpos.state(WHITE) != UNKNOWN || cpos.state(BLACK) != UNKNOWN {
            match cpos.write_seq(&mut bufWriter) {
                Err(ioe) => {
                    return Err(format!(
                        "error writing {}th position to {} file {} ({})",
                        i + 1,
                        fkind,
                        writePath,
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
    eprintln!(
        "done, {} positions written to file {}.",
        formatted_sz(npos),
        writePath,
        // formattedSZ(excl)
    );
    if restart && !interrupted {
        remove_file(&sortPath).map_err(|ioe| format!("Can't remove {} ({})", &sortPath, ioe))?;
    }
    if interrupted {
        Err(String::from("terminated by SIGINT"))
    } else {
        Ok(())
    }
}

enum Sink<'a> {
    W(BufWriter<File>),
    V(&'a mut Vec<CPos>),
}

impl Sink<'_> {
    pub fn push(&mut self, cpos: CPos) {
        match self {
            Sink::W(bf) => {
                cpos.write_seq(bf).unwrap();
            }
            Sink::V(vec) => {
                vec.push(cpos);
            }
        }
    }
    pub fn flush(&mut self) {
        match self {
            Sink::W(bf) => bf.flush().unwrap(),
            Sink::V(_) => (),
        }
    }
}

fn complete(pos: &Position, index: usize, pps: &Vec<PlayerPiece>, positions: &mut Sink) {
    if index >= pps.len() {
        let c = pos.compressed();
        if c.state(WHITE) != INVALID_POS || c.state(BLACK) != INVALID_POS {
            positions.push(c);
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
                let c = p.compressed();
                if c.state(WHITE) != INVALID_POS || c.state(BLACK) != INVALID_POS {
                    positions.push(c);
                }
                // for all pawns that could have moved a double step we need to make a
                // position where the player whose turn it is could
                // capture this pawn en passant.
                for pf in p.pawns() {
                    // which player's pawn?
                    let pc = if p.whites.member(pf) { WHITE } else { BLACK };
                    // If this is a white pawn, use the board where it is black's move and
                    // vice versa.
                    let ep = p;

                    if pc == WHITE && pf.rank() == 4 {
                        // compute the skipped field and proceed when it is empty
                        // because when it is occupied the pawn couldn't have moved a double
                        // step
                        let sf = Field::from(pf as u8 - 8);
                        if ep.isEmpty(sf) {
                            let c = ep.setFlag(sf).compressed();
                            if c.state(WHITE) != INVALID_POS || c.state(BLACK) != INVALID_POS {
                                positions.push(c);
                            }
                        }
                    }
                    if pc == BLACK && pf.rank() == 5 {
                        let sf = Field::from(pf as u8 + 8);
                        if ep.isEmpty(sf) {
                            let c = ep.setFlag(sf).compressed();
                            if c.state(WHITE) != INVALID_POS || c.state(BLACK) != INVALID_POS {
                                positions.push(c);
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

/// Estimate the number of positions we will find.
fn expected_positions(signature: P::Signature) -> usize {
    // positions of the two kings, less than 32*64 or 16*64
    let vecbase = if signature.whitePawns() > 0 || signature.blackPawns() > 0 {
        1806
    } else {
        903
    };
    // compute estimated number of moves, taking kind and number into account as well as already
    // hypothetically
    // for example, assuming 2 queens would yield a factor of 64*64 would be a great overestimation
    // The computed number is in most cases a slight over-estimation because positions where both
    // kings are not considered in further computations.
    let vecmax = vecbase
        * over(56, signature.whitePawns() as u64)
        * over(56 - signature.whitePawns() as u64, signature.blackPawns() as u64)
        * over(
            62 - signature.whitePawns() as u64 - signature.blackPawns() as u64,
            signature.whiteKnights() as u64,
        )
        * over(
            62 - signature.whitePawns() as u64 - signature.blackPawns() as u64 - signature.whiteKnights() as u64,
            signature.blackKnights() as u64,
        )
        * over(
            62 - signature.whitePawns() as u64
                - signature.blackPawns() as u64
                - signature.whiteKnights() as u64
                - signature.blackKnights() as u64,
            signature.whiteBishops() as u64,
        )
        * over(
            62 - signature.whitePawns() as u64
                - signature.blackPawns() as u64
                - signature.whiteKnights() as u64
                - signature.blackKnights() as u64
                - signature.whiteBishops() as u64,
            signature.blackBishops() as u64,
        )
        * over(
            62 - signature.whitePawns() as u64
                - signature.blackPawns() as u64
                - signature.whiteKnights() as u64
                - signature.blackKnights() as u64
                - signature.whiteBishops() as u64
                - signature.blackBishops() as u64,
            signature.whiteRooks() as u64,
        )
        * over(
            62 - signature.whitePawns() as u64
                - signature.blackPawns() as u64
                - signature.whiteKnights() as u64
                - signature.blackKnights() as u64
                - signature.whiteBishops() as u64
                - signature.blackBishops() as u64
                - signature.whiteRooks() as u64,
            signature.blackRooks() as u64,
        )
        * over(
            62 - signature.whitePawns() as u64
                - signature.blackPawns() as u64
                - signature.whiteKnights() as u64
                - signature.blackKnights() as u64
                - signature.whiteBishops() as u64
                - signature.blackBishops() as u64
                - signature.whiteRooks() as u64
                - signature.blackRooks() as u64,
            signature.whiteQueens() as u64,
        )
        * over(
            62 - signature.whitePawns() as u64
                - signature.blackPawns() as u64
                - signature.whiteKnights() as u64
                - signature.blackKnights() as u64
                - signature.whiteBishops() as u64
                - signature.blackBishops() as u64
                - signature.whiteRooks() as u64
                - signature.blackRooks() as u64
                - signature.whiteQueens() as u64,
            signature.blackQueens() as u64,
        );
    vecmax as usize
}
