#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(unused_attributes)]
#![allow(uncommon_codepoints)]

//! # The end game tablebase
//!
//! The EGTB is a collection of files in a dedicated directory.
//! Each filename is composed of a `Signature` (like KQB-KQ) and an extension.
//!
//! The `Signature` describes the end game unanimously. For example, in KQB-KQ, one player
//! has a queen and a bishop besides the king, the opponent has only a king and a queen.
//!
//! The extension describes what is in the file (we continue to use KQB-KQ as an example signature):
//! - KQB-KQ.`egtb` is a sorted array of compressed end game positions. Each contained position has a status
//! for white and black. The status tells what the position is worth for white and black.
//! - KQB-KQ.`moves` is a sorted array of compressed end game positions. It contains only positions that have
//! status CAN_MATE for a specific player. Instead of status codes, it contains an indication whether it is meant
//! for black or for white and a move to reach mate.
//!
//! Not all possible moves for a position have to be in the `egtb` file. It is assumed that such positions
//! are a draw if no player makes an error. For example, the `K-K.egtb` file is entirely empty.
//!
//! The following extensions are used during the process of making end game tables:
//! - KQB-KQ.`sorted` can be the result of manual interruption (e.g. Ctrl+C) of the in-memory analysis.
//! The next attempt to generate KQB-KQ will pick up this file and will continue where it left off.
//! It can also be result of an explicit sort, see next paragraph.
//! - KQB-KQ.`unsorted` will be created, when the available RAM is unable to hold all the possible positions.
//! This is especially likely with the 6-piece tablebases.
//! It is possible to sort such files and obtain KQB-KQ.`sorted`.
//! - KQB-KQ.`um` (for *unsorted moves*) will be written during analysis. When analysis is complete,
//! this file will be sorted and renamed to KQB-KQ.`moves`.
//! - KQB-KQ.`chunk`*N* temporary files created during sorting. Should normally get removed once sorting completes.
//! They can be forced to a certain directory by setting environment variable `EGTBTEMP`.
//! Absent this, they go to the same directory as the other files.
//! - KQB-KQ.`mc` (for *mates complete*) is a variant of KQB-KQ.`sorted`
//! that is the result of an analysis pass that finds no new
//! mate-related positions. The next analysis pass will do draw analysis, and this will either result in a new
//! KQB-KQ.`mc` file or, if the draw analysis is also complete, in the KQB-KQ.`egtb` file.
//! - KQB-KQ.`pass` is the already finished part of an analysis pass and is left on
//! disk when the pass was manually interrupted. If the request to generate KQB-KQ finds
//! KQB-KQ.`sorted` or KQB-KQ.`mc` and the
//! positions do not fit into memory, the analysis will pick up at the point of interruption. When a pass through
//! the sorted file is complete, this file becomes the new sorted file.
//!
//! ## Command line arguments for EGTB creation
//!
//! All commands expect the table base directory in the path given by environment variable EGTB.
//! If not set, the local directory (or symbolic link) `./egtb` will be used.
//!
//! ### rasch make KQB-KQ
//!
//! The `make` verb expects a signature and has many functions,
//! depending on the current state of the EGTB directory.
//!
//! The ultimate goal of `make` is to produce the `egtb` and `moves` files for the given signature.
//!
//! 1. If there is already an `egtb` file and a `moves` file, nothing is done.
//! You must remove these files manually if you want to generate them anew (but why would you?).
//! This is so that you don't - for example - get a `make` command from
//! the shell history by accident and destroy the work of days and weeks.
//!
//! 2. If there is an `egtb` file and an `um` file, the latter will be sorted and thus a `moves` file created.
//! You should then run a `stats` command for the signature in question to make sure everything is fine.
//! This is a state of affairs that will normally not happen. Did a power out or shutdown occur, maybe?
//! Chances are that the existing files are corrupt or incomplete.
//!
//! 3. It is an error, if there is either one of `egtb` or `moves`/`um` present, but the other is missing.
//! It is for now not possible to recreate the `moves` file from the `egtb` file, and it will never be
//! possible the other way around.
//!
//! 4. If there is a `sorted` file this is the signal to restart the mate analysis. The `um` file will be continued.
//! Whether the analysis is done in-memory or on disk depends on the available RAM. If it turns out to be disk based,
//! a `pass` file will be honored.
//!
//! 5. If there is a `mc` file, the behavior is the same as with `sorted`, just that draw analysis will be done.
//!
//! 6. If there is an `unsorted` file, it will get sorted and all `pass` and `um` files for the given signature
//! removed. It then proceeds like in paragraph 4.
//!
//! 7. If none of the above is the case, it will try a memory based analysis. If this isn't possible, it
//! writes all possible moves to the `unsorted` file, and proceeds like in the previous paragraph.
//!
//! Note that generation for a given signature assumes the existence of the `egtb` files for signatures that
//! will be needed for lookup when a capturing move or a pawn promotion happens. The generation will stop with
//! a message that tells which EGTB is missing. The recipe is to generate first the K-K end game, then the end games
//! with one extra piece, and so forth. Also, KP-K must be done only after KN-K, KB-K, KR-K and KQ-K are done,
//! end games with more pawns only after the end games with one pawn less, and so forth.
//!
//! Analysis can be interruptet by entering Ctrl+C.
//! In memory based runs, this will create a `sorted` file for restart.
//! In disk based ones, the positions completed so far are written to the `pass` file.
//!
//! ### rasch sort KQB-KQ
//!
//! Separate sorting of an `unsorted` file to produce a `sorted` one.
//! The `unsorted` one and all corresponding `pass` and `um` files are removed.
//!
//! ### rasch check KQB-KQ
//!
//! Checks a `moves` file. Detects when the file is not sorted properly or contains duplicates.
//! It also checks for each position if the corresponding one in the `egtb` file has the CAN_MATE status.
//!
//! ### rasch stats KQB-KQ
//!
//! Reads a complete `egtb` and checks for various conditions that would indicate corruption or incompleteness.
//! It also checks that the number of positions in the `moves` file
//! equals the number of CAN_MATE positions found.
//!

use super::{
    basic::{CPosState, Move, Player},
    cpos::{CPos, DtmMap, EgtbMap, MPos, Mirrorable, MoveImpact, MovesMap, Signature},
    cposio::{
        byte_anon_map, byte_ro_map, byte_rw_map, cpos_anon_map, cpos_append_writer, cpos_create_writer,
        cpos_file_size, cpos_open_reader, cpos_ro_map, cpos_rw_map, mk_egtb_path, mk_mpos_dtm, mk_temp_path,
        short_anon_map, short_ro_map, short_rw_map, CPosReader, MPosDTM, WSeq,
    },
    fen::{decodeFEN, encodeFEN},
    fieldset::Field,
    position::{showMovesSAN, Position},
    sortegtb::sort_from_to,
    util::*,
};

// use super::fieldset::*;
// use super::position as P;

use CPosState::*;
use Field::*;
use MoveImpact::*;
use Player::*;

use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fs::{remove_file, rename, File},
    io::{BufReader, BufWriter, ErrorKind::*, Seek, SeekFrom, Write},
    path::Path,
    sync::{atomic, Arc},
};

pub type Cache = HashMap<usize, Vec<(Move, CPos)>>;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
enum MakeState {
    Done,
    EgtbPresent,
    WorkPresent,
    StartUp,
    WriteEgtb,
    ScanForMate(CPos),
    ScanForAliens(CPos),
    ScanUM(usize),
    OptDTM,
}

use MakeState::*;

static sigint_received: atomic::AtomicBool = atomic::AtomicBool::new(false);
static handler_installed: atomic::AtomicBool = atomic::AtomicBool::new(false);

/// Sort the um file, if present.
///
/// results in `Done`
fn make_moves(
    signature: Signature,
    um_path: &str,
    moves_path: &str,
    dtm_path: &str,
) -> Result<MakeState, String> {
    let um_exists = Path::new(um_path).is_file();
    let moves_exists = Path::new(moves_path).is_file();
    if um_exists {
        if moves_exists {
            Err(format!(
                "Both {0} and {1} exist.\n\
                Please remove {0} manually if {1} is ok. To check this use 'rasch check {2}'",
                um_path, moves_path, signature
            ))
        } else {
            sort_from_to(signature, um_path, moves_path, dtm_path).map(|_| Done)
        }
    } else {
        if moves_exists {
            Ok(Done)
        } else {
            Err(format!(
                "Neither {0} nor {1} exist.\n    \
                Remove {2} and recreate EGTB for {3}, e.g. 'rasch gen {3}'",
                um_path,
                moves_path,
                mk_egtb_path(signature, "egtb"),
                signature
            ))
        }
    }
}

/// Make positions on disk
///
/// Result is: WorkPresent
fn make_work(signature: Signature, pass: usize, work_path: &str) -> Result<MakeState, String> {
    let last = signature.last();
    let max_items = last.canonic_addr().0;
    let mut n_items = 0;
    eprint!("{}  Pass {:2} - generate positions ", signature, pass,);
    progress(0, 0);

    let map_size = last.canonic_addr().bytes();
    let (_map, array) = byte_anon_map(map_size)?;

    // set all flags to INVP
    array.fill(0xff);

    for cpos in signature.first() {
        n_items += 1;
        if n_items % (1024 * 1024) == 0 || cpos == last {
            progress(cpos.canonic_addr().0, max_items);
        }
        let ws = if cpos.valid(WHITE) { DRAW } else { INVP };
        let bs = if cpos.valid(BLACK) { DRAW } else { INVP };
        cpos.with_state(ws, bs).set_canonic(array);
    }
    eprintln!("done, found {}.", formatted_h(max_items, '1'));
    let mut writer = cpos_create_writer(work_path)?;
    eprint!("    writing buffer to {} ", work_path);

    writer
        .write_all(array)
        .map_err(|e| format!("write error on {} ({})", work_path, e))?;
    eprintln!("done.");

    Ok(WorkPresent)
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

/// feed in an end game position and get back an appropriate move, provided the EGTB files exist
pub fn findEndgameMove(pos: &Position) -> Result<Move, String> {
    let cpos = if pos.validEndgame() {
        Ok(pos.compressed())
    } else {
        Err(String::from("position is no valid end game."))
    }?;
    let mut ehash: EgtbMap = HashMap::new();
    let mut mhash: MovesMap = HashMap::new();
    let mut dhash: DtmMap = HashMap::new();
    let sig = cpos.signature();
    // println!("# {} is canonic {}", sig, sig.is_canonic());
    let rpos = cpos.find(&mut ehash)?;
    if rpos != cpos {
        println!(
            "# {} looked for    0x{:016x}  {:?}  fen: {}",
            sig,
            cpos.bits,
            cpos,
            encodeFEN(&cpos.uncompressed(pos.turn()))
        );
    }
    println!(
        "# {} found canonic 0x{:016x}  {:?}  fen: {}",
        rpos.signature(),
        rpos.bits,
        rpos,
        encodeFEN(&rpos.uncompressed(pos.turn()))
    );

    let canonic_player = if rpos.canonic_has_bw_switched() { pos.turn().opponent() } else { pos.turn() };
    let s = rpos.state(canonic_player);
    let other = pos.turn().opponent();

    match s {
        INVP => Err(format!("illegal {:?} state for this position", s)),
        WINS => {
            let mpos = rpos.find_canonic_mpos(rpos.signature(), canonic_player, &mut mhash)?;
            println!(
                "# {} found move    0x{:016x}  {}  fen: {}",
                mpos.signature(),
                mpos.bits,
                mpos.mpos_debug(),
                encodeFEN(&mpos.uncompressed(canonic_player))
            );
            let mv = rpos.mv_from_mpos(mpos);
            println!(
                "# {:?} to move plays {:5} to enforce mate",
                mv.player(),
                mv.showSAN(*pos)
            );
            Ok(mv)
        }
        LOST => {
            if rpos.successors(canonic_player).next().is_none() {
                println!("# this looks like mate");
                Err("MATE".to_string())
            } else {
                let (dtm, rmv) = rpos
                    .successors(canonic_player)
                    .map(|(succ, mv)| {
                        (
                            succ.find_dtm(canonic_player.opponent(), &mut ehash, &mut mhash, &mut dhash)
                                .or::<String>(Ok(0u16))
                                .unwrap(),
                            mv,
                        )
                    })
                    .reduce(|(dtm1, mv1), (dtm2, mv2)| if dtm1 > dtm2 { (dtm1, mv1) } else { (dtm2, mv2) })
                    .unwrap();
                let cmv = if rpos.canonic_has_bw_switched() {
                    Move::new(
                        rmv.player().opponent(),
                        rmv.piece(),
                        rmv.promote(),
                        rmv.from(),
                        rmv.to(),
                    )
                } else {
                    rmv
                };
                let dmv = if rpos.canonic_was_mirrored_d() { cmv.mirror_d() } else { cmv };
                let hmv = if rpos.canonic_was_mirrored_h() { dmv.mirror_h() } else { dmv };
                let vmv = if rpos.canonic_was_mirrored_v() { hmv.mirror_v() } else { hmv };
                println!(
                    "# {:?} to move plays {:5} to avoid mate with DTM {}",
                    pos.turn(),
                    vmv.showSAN(*pos),
                    dtm,
                );
                Ok(vmv)
            }
        }
        DRAW => {
            let moves = pos.moves();
            if moves.is_empty() {
                let what = "stalemate";
                println!("# this looks like {}", what);
                Err(what.to_uppercase())
            } else {
                let mv = match moves.iter().copied().find(|&mv| {
                    pos.apply(mv) // erreichte Position
                    .compressed() // komprimiert
                    .find(&mut ehash) // Ok(rp) oder Err()
                    .ok() // Some(rp) oder None
                    .map(|r| if r.canonic_has_bw_switched() { r.flipped_flags() } else { r })
                    .map(|r| r.state(other) != WINS)
                    .unwrap_or(false)
                }) {
                    None => moves[0],
                    Some(mv) => mv,
                };
                println!(
                    "# {:?} to move cannot avoid {} so {} {}",
                    pos.turn(),
                    if s == LOST { "mate" } else { "draw" },
                    if mv == moves[0] && moves.len() > 1 { "arbitrarily" } else { "intentionally" },
                    mv.showSAN(*pos)
                );
                Ok(mv)
            }
        }
    }
}

/// ### Check sanity of move positions for a Signature
/// - the moves file must exist
/// - the moves file must be sorted
/// - it must not contain duplicates
/// - for each move position, there must be an entry in the associated EGTB file and the status for the player must be [WINS]
/// - for each move position, a valid [Position] must be obtainable
/// - for each move position, the reconstructed `Move` must occur in the moves list of that [Position]
pub fn check_moves(arg: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(arg)?;
    let path = mk_egtb_path(signature, "moves");
    let egtb = mk_egtb_path(signature, "egtb");
    let epositions = byte_ro_map(&egtb)?;
    let mut e_hash: EgtbMap = HashMap::new();
    let mut last = CPos { bits: 0 };
    let mut sorted = true;
    let mut dupl = 0usize;
    let mut unrelated = false;
    let mut invalid = false;
    let mut badmoves = 0usize;
    let mut badmpos = 0usize;
    let mut nowin = 0usize;
    let rdr = CPosReader::new(&path)?;
    let n_pos = cpos_file_size(&path)?;
    let mut curr_pos = 0;
    eprint!(
        "{} checking {} move positions from {} ",
        signature.display(),
        formatted_sz(n_pos),
        path
    );
    progress(0, 0);
    for mpos in rdr {
        curr_pos += 1;
        if n_pos > 0 && (curr_pos % 1_000_000 == 1 || curr_pos >= n_pos) {
            progress(curr_pos, n_pos);
        }
        sorted = sorted && last <= mpos;

        if last == mpos {
            if dupl == 0 {
                eprintln!(
                    "\n{}:{}  duplicate mpos {} ({:#016x})",
                    signature,
                    curr_pos,
                    mpos.mpos_debug(),
                    mpos.bits
                );
            }
            dupl += 1;
        }
        last = mpos;

        if mpos.signature() != signature {
            if badmpos == 0 {
                eprintln!(
                    "\n{}:{}  signature mismatch  {}  {}",
                    signature,
                    curr_pos,
                    mpos.signature(),
                    mpos.mpos_debug()
                );
            }
            badmpos += 1;
            continue;
        }

        let canonic = mpos.mpos_to_cpos();
        if canonic != canonic.canonical(signature) {
            if badmpos == 0 {
                eprintln!("\n{}:{}  move position is not canonic.", signature, curr_pos);
                eprintln!("    move pos  {}", mpos.mpos_debug());
                eprintln!("    canonic   {:?}", canonic.canonical(signature));
            }
            badmpos += 1;
            continue;
        }

        let cpos: CPos;
        match canonic.lookup_canonic(epositions.1) {
            Ok(x) => cpos = x,
            Err(s) => {
                if badmpos == 0 {
                    eprintln!(
                        "\n{}:{}  move position cannot be found in {}",
                        signature, curr_pos, egtb
                    );
                    eprintln!("    {}", s);
                    eprintln!("    {} (Signature {})", mpos.mpos_debug(), mpos.signature());
                }
                badmpos += 1;
                continue;
            }
        };

        let player = mpos.mpos_player();

        if !unrelated && cpos.state(player) != WINS {
            eprintln!(
                "\n{}:{}  position not marked as WINS for {:?}",
                signature, curr_pos, player
            );
            eprintln!("    mpos:  {} ({:#016x})", mpos.mpos_debug(), mpos.bits);
            eprintln!("    cpos:  {:?} ({:#016x})", cpos, cpos.bits);
        }
        unrelated = unrelated || cpos.state(player) != WINS;

        let pos = cpos.uncompressed(player);
        invalid = invalid || !pos.valid();
        if pos.valid() {
            let mvs = pos.moves();
            let xmv = cpos.mv_from_mpos(mpos);
            badmoves = match mvs.iter().copied().find(|x| *x == xmv) {
                Some(_mv) => {
                    let loos = cpos.apply(xmv);
                    let lcan = loos.clear_trans().find(&mut e_hash)?;
                    let looser = if lcan.canonic_has_bw_switched() { player } else { player.opponent() };
                    if lcan.state(looser) != LOST {
                        if nowin == 0 {
                            eprintln!("\n{}:{}  move {} will not win", signature, curr_pos, xmv,);
                            eprintln!("cpos    0x{:016x} {:?}", cpos.bits, cpos);
                            eprintln!("mpos    0x{:016x} {}", mpos.bits, mpos.mpos_debug());
                            eprintln!("succ    0x{:016x} {:?}", loos.bits, loos);
                            eprintln!("c-succ  0x{:016x} {:?}", lcan.bits, lcan);
                        }
                        nowin += 1;
                    }
                    badmoves
                }
                None => {
                    if badmoves == 0 {
                        eprintln!(
                            "\n{}:{}  move {} impossible in position {}",
                            signature,
                            curr_pos,
                            xmv,
                            encodeFEN(&pos)
                        );
                        eprintln!("cpos 0x{:016x} {:?}", cpos.bits, cpos);
                        eprintln!("mpos 0x{:016x} {}", mpos.bits, mpos.mpos_debug());
                    }
                    badmoves + 1
                }
            };
        }
    }
    eprintln!(" done.");
    if !sorted {
        eprintln!("{} moves file {} is not sorted.", signature, path);
    }
    if dupl > 0 {
        eprintln!(
            "{} moves file {} contains {} duplicate entries.",
            signature, path, dupl
        );
    }
    if unrelated {
        eprintln!("{} moves file {} has unrelated entries.", signature, path);
    }
    if invalid {
        eprintln!(
            "{} moves file {} has entries that yield invalid positions.",
            signature, path
        );
    }
    if badmpos > 0 {
        eprintln!("{} moves file has {} illegal positions.", signature, badmpos);
    }
    if badmoves > 0 {
        eprintln!(
            "{} moves file {} has {} bad/unknown moves.",
            signature, path, badmoves
        );
    }
    if nowin > 0 {
        eprintln!("{} moves file suggests {} moves that wont win", signature, nowin);
    }

    std::mem::drop(epositions.0);

    if sorted && dupl == 0 && !unrelated && !invalid && badmoves == 0 && badmpos == 0 && nowin == 0 {
        Ok(())
    } else {
        Err(format!("moves file {} is corrupt", path))
    }
}

/// Provide statistics for an endgame tablebase and perform basic checks.
pub fn check_egtb(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let e_path = mk_egtb_path(signature, "egtb");
    let m_path = mk_egtb_path(signature, "moves");
    let d_path = mk_egtb_path(signature, "dtm");
    let (_map1, edb) = byte_ro_map(&e_path)?;
    let (_map2, mdb) = cpos_ro_map(&m_path)?;

    let mut wkinds = vec![0, 0, 0, 0, 0, 0, 0, 0];
    let mut bkinds = vec![0, 0, 0, 0, 0, 0, 0, 0];
    let c0 = CPos { bits: 0 };
    let mut wexamples = vec![c0, c0, c0, c0, c0, c0, c0, c0];
    let mut bexamples = vec![c0, c0, c0, c0, c0, c0, c0, c0];
    let mut total = 0usize;
    let mut moves_missing = 0usize;
    let mut poss_missing = 0usize;
    let mut winners_missing = 0usize;
    let mut cad_both = 0usize;
    let mut cad_example = c0;
    let mut mates_white = 0usize;
    let mut mates_black = 0usize;
    let mut mate_wexample = c0;
    let mut mate_bexample = c0;
    let last = signature.last();
    eprint!(
        "{}  checking {} positions in {} ",
        signature,
        formatted_sz(2 * edb.len()),
        e_path
    );
    progress(0, 0);
    for current in signature.first() {
        total += 1;
        if total % (1024 * 1024) == 1 || current == last {
            progress(current.canonic_addr().0, last.canonic_addr().0);
            if sigint_received.load(atomic::Ordering::SeqCst) {
                eprintln!("\x08\x08 canceled.");
                sigint_received.store(false, atomic::Ordering::SeqCst);
                return Err("interrupted".to_string());
            }
        }

        let c = match current.lookup_canonic(edb) {
            Ok(x) => x,
            Err(s) if poss_missing == 0 => {
                eprintln!(
                    "\n{}:{}  position cannot be found in {}",
                    signature, total, e_path
                );
                eprintln!("    {}", s);
                poss_missing += 1;
                continue;
            }
            Err(_) => {
                poss_missing += 1;
                continue;
            }
        };

        let inx = c.state(WHITE) as usize;
        wkinds[inx] += 1;
        wexamples[inx] = c;
        let inx = c.state(BLACK) as usize;
        bkinds[inx] += 1;
        bexamples[inx] = c;

        if c.state(BLACK) == DRAW && c.state(WHITE) == DRAW {
            cad_both += 1;
            cad_example = c;
        }

        if c.state(BLACK) == LOST && c.state(WHITE) == INVP && c.successors(BLACK).next().is_none() {
            if mates_black == 0 {
                mate_bexample = c;
            }
            mates_black += 1;
        }

        if c.state(WHITE) == LOST && c.state(BLACK) == INVP && c.successors(WHITE).next().is_none() {
            if mates_white == 0 {
                mate_wexample = c;
            }
            mates_white += 1;
        }

        for p in [BLACK, WHITE] {
            if c.state(p) == WINS {
                let m = c.cpos_to_mpos(p);
                match mdb.binary_search(&m) {
                    Ok(_) => {}
                    Err(s) if moves_missing == 0 => {
                        eprintln!(
                            "\n{}:{}  no move for {:?} WINS position ({})",
                            signature, total, p, s
                        );
                        eprintln!("    cpos  {:?} ({:#016x})", c, c.bits);
                        moves_missing += 1;
                    }
                    Err(_) => moves_missing += 1,
                }
            }
            if c.state(p) == LOST {
                match c
                    .predecessors(p)
                    .map(|(c, mv)| (c.light_canonical().lookup_canonic(edb).unwrap(), mv))
                    .find(|(c, _)| c.state(p.opponent()) != WINS)
                {
                    Some((w, mv)) if winners_missing == 0 => {
                        eprintln!();
                        eprintln!("{}:{}    {:?}  {:?}", signature, total, p, c);
                        eprintln!(
                            "{}:{}    {:?}  {:?}  predecessor by {}",
                            signature,
                            total,
                            p.opponent(),
                            w,
                            mv
                        );
                        eprintln!("{}:{}    state should be {:?}", signature, total, WINS);
                        winners_missing += 1;
                    }
                    Some(_) => winners_missing += 1,
                    None => {}
                }
                match c
                    .successors(p)
                    .filter(|(_, mv)| c.mv_impact(*mv) != SIGNATURE)
                    .map(|(w, mv)| {
                        let w1 = match c.mv_impact(mv) {
                            HARMLESS => w,
                            CANONICALITY => w.light_canonical(),
                            SIGNATURE => w,
                        };
                        (w1.lookup_canonic(edb).unwrap(), mv)
                    })
                    .find(|(c, _mv)| c.state(p.opponent()) != WINS)
                {
                    Some((w, mv)) if winners_missing == 0 => {
                        eprintln!();
                        eprintln!("{}:{}    {:?}  {:?}", signature, total, p, c);
                        eprintln!(
                            "{}:{}    {:?}  {:?}  successor by {}",
                            signature,
                            total,
                            p.opponent(),
                            w,
                            mv
                        );
                        eprintln!("{}:{}    state should be {:?}", signature, total, WINS);
                        winners_missing += 1;
                    }
                    Some(_) => winners_missing += 1,
                    None => {}
                }
            }
        }
    }
    eprintln!(" done.");

    for i in 0..wkinds.len() {
        let s = CPosState::from(i as u64);
        if wkinds[i] > 0 {
            eprintln!(
                "{:>12} white positions with status {:<6} for example {}",
                formatted_sz(wkinds[i]),
                format!("{:?}", s),
                encodeFEN(&wexamples[i].uncompressed(WHITE))
            );
        }
        if bkinds[i] > 0 {
            eprintln!(
                "{:>12} black positions with status {:<6} for example {}",
                formatted_sz(bkinds[i]),
                format!("{:?}", s),
                encodeFEN(&bexamples[i].uncompressed(BLACK))
            );
        }
    }
    if cad_both > 0 {
        eprintln!(
            "{:>12} both  positions with status {:<6} for example {}",
            formatted_sz(cad_both),
            format!("{:?}", DRAW),
            encodeFEN(&cad_example.uncompressed(BLACK))
        );
    }
    if mates_white > 0 {
        eprintln!(
            "{:>12} white positions with status MATE   for example {}",
            formatted_sz(mates_white),
            encodeFEN(&mate_wexample.uncompressed(WHITE))
        );
    }
    if mates_black > 0 {
        eprintln!(
            "{:>12} black positions with status MATE   for example {}",
            formatted_sz(mates_black),
            encodeFEN(&mate_bexample.uncompressed(BLACK))
        );
    }
    eprintln!("{:>12} positions total", formatted_sz(total));
    if moves_missing > 0 {
        eprintln!("{:>12} moves are missing", formatted_sz(moves_missing));
    }
    if poss_missing > 0 {
        eprintln!("{:>12} positions are missing", formatted_sz(poss_missing));
    }
    if winners_missing > 0 {
        eprintln!("{:>12} winners are missing", formatted_sz(winners_missing));
    }

    if moves_missing > 0 || poss_missing > 0 || winners_missing > 0 {
        eprintln!("Warning: This is an invalid or yet incomplete end game table.");
        Err("corrupt egtb".to_string())
    } else {
        // print DTM table if d_path exists
        match short_ro_map(&d_path) {
            Ok((_map, ddb)) => {
                let mut vx: Vec<usize> = (0..ddb.len()).collect();
                vx.sort_unstable_by(|a, b| ddb[*b].cmp(&ddb[*a]));
                println!("Top 20 positions with the greatest distance to mate:");
                for top in 0..20.min(vx.len()) {
                    let index = vx[top];
                    let mpos = mdb[index];
                    println!(
                        "DTM {:>4}  {}",
                        ddb[index],
                        encodeFEN(&mpos.mpos_to_cpos().uncompressed(mpos.mpos_player()))
                    );
                }
                println!(
                    "{} of {} dtm values are unknown.",
                    (0..ddb.len()).filter(|u| ddb[*u] == 0xffff).count(),
                    ddb.len()
                );
            }
            Err(s) => eprintln!("Warning: {}", s),
        }

        Ok(())
    }
}

pub fn make(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let rels = signature.predecessors();
    for x in rels.keys() {
        let s = x.display();
        make(&s)?;
    }
    gen(sig)
}

/// mmap backed generation
pub fn gen(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let egtb_path = mk_egtb_path(signature, "egtb");
    let moves_path = mk_egtb_path(signature, "moves");
    let dtm_path = mk_egtb_path(signature, "dtm");
    let work_path = mk_egtb_path(signature, "work");
    let um_path = mk_temp_path(signature, "um");
    let restart_path = mk_egtb_path(signature, "restart");

    let mut pass = 0;
    let mut um_writer = cpos_append_writer("/dev/null")?;
    let (mut e_map, mut edb) = byte_anon_map(1)?;
    let (mut _m_map, mut _mdb) = cpos_anon_map(1)?;
    let (mut _d_map, mut _ddb) = short_anon_map(1)?;

    let mut e_hash: EgtbMap = HashMap::new();
    let mut m_hash: MovesMap = HashMap::new();
    let mut d_hash: DtmMap = HashMap::new();

    // initial state depends on the files in the EGTB directory
    let mut state = if Path::new(&egtb_path).is_file() {
        if Path::new(&moves_path).is_file() {
            if Path::new(&dtm_path).is_file() {
                Done
            } else {
                Done // InitDTM
            }
        } else {
            EgtbPresent
        }
    } else if Path::new(&work_path).is_file() {
        WorkPresent
    } else {
        StartUp
    };

    let handler_ref = Arc::new(&sigint_received);
    if !handler_installed.load(atomic::Ordering::SeqCst) {
        ctrlc::set_handler(move || {
            handler_ref.store(true, atomic::Ordering::SeqCst);
        })
        .map_err(|e| format!("Cannot set CTRL-C handler ({})", e))?;
        handler_installed.store(true, atomic::Ordering::SeqCst);
    }

    while state != Done {
        pass += 1;
        // eprintln!("{}  Pass {:2} {:?}", signature, pass, state);
        match state {
            EgtbPresent => {
                eprintln!("{}  Pass {:2} - sorting moves", signature, pass);
                std::mem::drop(um_writer);
                um_writer = cpos_append_writer("/dev/null")?;
                make_moves(signature, &um_path, &moves_path, &dtm_path)?;

                state = OptDTM;
            }

            OptDTM => {
                eprint!(
                    "{}  Pass {:2} - optimizing winning moves (not yet!)  ",
                    signature, pass
                );
                progress(0, 0);

                eprintln!();
                /* if signature.to_vec().len() < 4 {
                    check_egtb(&signature.display())?;
                    check_moves(&signature.display())?;
                } */
                state = Done;
            }

            StartUp => {
                remove_file(&moves_path).unwrap_or_default();
                state = make_work(signature, pass, &work_path)?;
            }

            WriteEgtb => {
                eprintln!("{}  Pass {:2} - finishing {}", signature, pass, egtb_path);
                std::mem::drop(e_map);
                let (xmap, xdb) = byte_anon_map(1)?;
                e_map = xmap;
                edb = xdb;
                rename(Path::new(&work_path), Path::new(&egtb_path))
                    .map_err(|e| format!("mv {} {} failed ({})", work_path, egtb_path, e))?;
                state = EgtbPresent;
            }

            WorkPresent => {
                eprintln!("{}  Pass {:2} - continuing with {}", signature, pass, work_path);
                remove_file(&moves_path).unwrap_or_default();
                std::mem::drop(e_map);
                // from this point on, the map is connected to the working file
                // and um_writer writes to the um file
                // until EgtbPresent
                {
                    let (xmap, xdb) = byte_rw_map(&work_path)?;
                    e_map = xmap;
                    edb = xdb;

                    um_writer = if Path::new(&um_path).is_file() {
                        cpos_append_writer(&um_path)
                    } else {
                        cpos_create_writer(&um_path)
                    }?;
                }

                // check for restart file
                let restart = Path::new(&restart_path).is_file();

                state = if restart {
                    let mut rdr = cpos_open_reader(&restart_path)?;
                    let pat = CPos::read_seq(&mut rdr)
                        .map_err(|e| format!("error reading {} ({})", restart_path, e))?;
                    let xpos = CPos::read_seq(&mut rdr)
                        .map_err(|e| format!("error reading {} ({})", restart_path, e))?;
                    eprintln!(
                        "    Restarting {} at {}",
                        if pat.bits as usize == RESTART_MATES {
                            "mate analysis"
                        } else if pat.bits as usize == RESTART_UM {
                            "retrograde analysis"
                        } else {
                            "nothing"
                        },
                        xpos.bits
                    );
                    if pat.bits as usize == RESTART_MATES {
                        ScanForMate(xpos)
                    } else if pat.bits as usize == RESTART_UM {
                        ScanUM(xpos.bits as usize)
                    } else if pat.bits as usize == RESTART_ALIENS {
                        ScanForAliens(xpos)
                    } else {
                        return Err("there is garbage in the restart file".to_string());
                    }
                } else {
                    ScanForMate(signature.first())
                };
            }

            ScanForMate(at) => {
                eprint!(
                    "{}  Pass {:2} - find mates, stalemates & direct winners    0‰ {} ",
                    signature,
                    pass,
                    fmt_human(edb.len(), 0)
                );
                let result = scan_mates_aliens(
                    signature,
                    at,
                    &mut um_writer,
                    edb,
                    &mut e_hash,
                    &mut m_hash,
                    &mut d_hash,
                    true,
                )?;
                um_writer
                    .flush()
                    .map_err(|e| format!("error while flushing {} ({})", um_path, e))?;
                eprint!("    setting checkpoint ");
                match result {
                    None => {
                        set_checkpoint(&restart_path, RESTART_ALIENS, 0)?;
                        state = ScanForAliens(signature.first());
                    }
                    Some(wpos) => {
                        eprint!(" for restart ");
                        set_checkpoint(&restart_path, RESTART_MATES, wpos.bits as usize)?;
                        return Err("terminated via Ctrl-C".to_string());
                    }
                }
            }

            ScanForAliens(at) => {
                eprint!(
                    "{}  Pass {:2} - find alien loosers    0‰ {} ",
                    signature,
                    pass,
                    fmt_human(edb.len(), 0)
                );
                let result = scan_mates_aliens(
                    signature,
                    at,
                    &mut um_writer,
                    edb,
                    &mut e_hash,
                    &mut m_hash,
                    &mut d_hash,
                    false,
                )?;
                um_writer
                    .flush()
                    .map_err(|e| format!("error while flushing {} ({})", um_path, e))?;
                eprint!("    setting checkpoint ");
                match result {
                    None => {
                        set_checkpoint(&restart_path, RESTART_UM, 0)?;
                        state = ScanUM(0);
                    }
                    Some(wpos) => {
                        eprint!(" for restart ");
                        set_checkpoint(&restart_path, RESTART_ALIENS, wpos.bits as usize)?;
                        return Err("terminated via Ctrl-C".to_string());
                    }
                }
            }

            ScanUM(at) => {
                eprint!("{}  Pass {:2} - retrograde analysis    0‰    0 ", signature, pass);
                m_hash.clear(); // reduce the virtual memory load
                let mut um_reader = cpos_open_reader(&um_path)?;
                um_reader
                    .seek(SeekFrom::Start(at as u64 * 10))
                    .map_err(|e| format!("Can't seek {} to index {} ({})", um_path, at, e))?;
                let r = scan_um(signature, &mut um_reader, &mut um_writer, edb, &mut e_hash)?;
                eprint!("    Flushing {} and {} ... ", work_path, um_path);
                e_map
                    .flush()
                    .and_then(|_| um_writer.flush())
                    .map_err(|e| format!("Can't sync {} and {} ({})", work_path, um_path, e))?;
                eprintln!("done.");
                state = match r {
                    Some(u) => {
                        eprint!("    setting checkpoint for restart ");
                        set_checkpoint(&restart_path, RESTART_UM, u)?;
                        return Err("terminated via Ctrl-C".to_string());
                    }
                    None => WriteEgtb,
                }
            }

            Done => {} // required for completeness
        }
    }

    std::mem::drop(um_writer);
    for p in [um_path, work_path, restart_path] {
        remove_file(p).unwrap_or_default();
    }
    eprintln!("{}  complete", signature);
    Ok(())
}

const RESTART_MATES: usize = 0xBADDEED;
const RESTART_UM: usize = 0xDEADBEEF;
const RESTART_ALIENS: usize = 0xCAFEBABE;

/// Remember the reading position in long words of the um-file
fn set_checkpoint(path: &str, pattern: usize, at: usize) -> Result<(), String> {
    let mut bufw = cpos_create_writer(path)?;
    CPos { bits: pattern as u64 }
        .write_seq(&mut bufw)
        .and_then(|_| CPos { bits: at as u64 }.write_seq(&mut bufw))
        .and_then(|_| bufw.flush())
        .map_err(|e| format!("cannot write {} ({})", path, e))?;
    eprintln!("at {:x} {}", pattern, at);
    Ok(())
}

const DEBUG_POSITION: CPos = CPos { bits: 0xd_4006_c980_6000 };

/// assert_eq!(signature, can.signature())
fn register_winner(
    signature: Signature,
    can: CPos,
    umv: Move,
    dtm: u16,
    writer: &mut BufWriter<File>,
    db: &mut [u8],
) -> Result<usize, String> {
    assert!(dtm & 1 == 1); // winner DTMs must be odd
    let canonic = can.clear_trans().canonical(signature);
    if canonic == DEBUG_POSITION {
        eprint!(" \x08");
    }
    let c = canonic.lookup_canonic(db)?;
    if c.state(umv.player()) == DRAW {
        c.with_db_state_for(umv.player(), WINS, db);
        let vmv = if canonic.canonic_was_mirrored_v() { umv.mirror_v() } else { umv };
        let hmv = if canonic.canonic_was_mirrored_h() { vmv.mirror_h() } else { vmv };
        let mv = if canonic.canonic_was_mirrored_d() { hmv.mirror_d() } else { hmv };
        let mpos = c.mpos_from_mv(mv);
        mk_mpos_dtm(mpos, dtm).write_seq(writer).map_err(|ioe| {
            format!(
                "unexpected error ({}) while writing to {}",
                ioe,
                mk_temp_path(signature, "um")
            )
        })?;
        /*
        let lost = c.apply(mv);
        if !signature.has_pawns() && lost.signature() == signature {
            let lost = lost.canonical(signature);
            let looser = mv.player().opponent();
            let wk = lost.white_king();
            if A1D1D4_TRIANGLE.member(wk) && A1H8_DIAGONAL.member(wk) && A1H8_DIAGONAL(lost.black_king()) {

            }
        }
        */
        Ok(1)
    } else {
        Ok(0)
    }
}

fn scan_mates_aliens(
    signature: Signature,
    start: CPos,
    writer: &mut BufWriter<File>,
    edb: &mut [u8],
    e_hash: &mut EgtbMap,
    m_hash: &mut MovesMap,
    d_hash: &mut DtmMap,
    mate_only: bool,
) -> Result<Option<CPos>, String> {
    let mut wins = 0usize;
    let mut loos = 0usize;
    let mut mates = 0usize;
    let mut stalemates = 0usize;
    let n_items = edb.len();
    let mut n = 0;
    let last = signature.last();

    for current in start {
        n += 1;
        if n_items > 0 && (n % (128 * 1024) == 0 || current == last) {
            if sigint_received.load(atomic::Ordering::SeqCst) {
                eprintln!("\x08\x08 canceled.");
                sigint_received.store(false, atomic::Ordering::SeqCst);
                return Ok(Some(current));
            }
            progress(current.canonic_addr().0, 2 * n_items);
        }

        'next_player: for player in [BLACK, WHITE] {
            let cp = current.lookup_canonic(edb)?;
            if cp == DEBUG_POSITION {
                eprint!(" \x08");
            }
            if cp.state(player) == DRAW {
                let mut all_successors_can_mate = true;
                let mut no_moves = true;
                let mut max_dtm = 0;
                if !mate_only && cp.alien_successors(player).next().is_none() {
                    continue 'next_player;
                }
                for (succ, mv) in cp.successors(player) {
                    no_moves = false;
                    if mate_only {
                        continue 'next_player;
                    }
                    // is this one of our positions?
                    // if so, let this position remain DRAW
                    // and let the work be done by retrograde analysis
                    if succ.signature().mk_canonic() == signature {
                        all_successors_can_mate = false;
                        continue /* next move */;
                    }
                    // it's an alien
                    let succ = succ.find(e_hash)?;
                    let opponent = if succ.canonic_has_bw_switched() { player } else { player.opponent() };
                    match succ.state(opponent) {
                        LOST => {
                            let dtm = succ.find_dtm(opponent, e_hash, m_hash, d_hash)?;
                            let newwin = register_winner(signature, cp, mv, dtm + 1, writer, edb)?;
                            wins += newwin;
                            continue 'next_player;
                        }
                        WINS => {
                            let dtm = succ.find_dtm(opponent, e_hash, m_hash, d_hash)?;
                            max_dtm = max_dtm.max(dtm);
                        }
                        DRAW | INVP => {
                            all_successors_can_mate = false;
                        }
                    }
                }
                if no_moves && cp.state(player.opponent()) != INVP {
                    // it's STALEMATE
                    stalemates += 1;
                    // it was already DRAW
                    // cp.with_state_for(player, DRAW).update_canonic(db);
                } else if all_successors_can_mate {
                    // it's not STALEMATE, but either MATE or all successors are alien and can mate!
                    if no_moves && cp.state(player.opponent()) == INVP {
                        mates += 1;
                        max_dtm = 1;
                    } else {
                        loos += 1;
                        max_dtm += 2;
                    }
                    cp.with_db_state_for(player, LOST, edb);
                    // find all moves from same signature (no captures, no promotions) that come here
                    // they all CAN_MATE
                    for (pred, mv) in cp.predecessors(player) {
                        let found = register_winner(signature, pred, mv, max_dtm, writer, edb)?;
                        wins += found;
                    }
                }
            }
        }
    }

    eprintln!("done.");
    if mates > 0 {
        eprintln!("    Found {} MATE positions.", formatted_sz(mates),);
    }
    if stalemates > 0 {
        eprintln!("    Found {} STALEMATE positions.", formatted_sz(stalemates),);
    }
    if wins > 0 {
        eprintln!("    Found {} new WINS positions.", formatted_sz(wins));
    }
    if loos > 0 {
        eprintln!("    Found {} new LOST positions.", formatted_sz(loos));
    }
    Ok(None)
}

/// Read the UM file and write new moves to the end until reading hits EOF when no new moves can be found.
///
/// Returns Ok(Some(position)) if interrupted, otherwise Ok(None)
fn scan_um(
    signature: Signature,
    reader: &mut BufReader<File>,
    writer: &mut BufWriter<File>,
    edb: &mut [u8],
    e_hash: &mut EgtbMap,
) -> Result<Option<usize>, String> {
    // let no_pawns = !signature.has_pawns();
    let um_path = mk_temp_path(signature, "um");
    let mut r_pos = reader
        .stream_position()
        .map_err(|e| format!("seek reader {} ({})", um_path, e))? as usize
        / 10;

    let mut n_winner = 0usize;
    let mut n_looser = 0usize;
    let mut w_pos = writer
        .flush()
        .and_then(|_| writer.stream_position())
        .map_err(|e| format!("flush/seek writer {} ({})", um_path, e))? as usize
        / 10;
    let w_pos0 = w_pos;

    loop {
        // make sure the reader can "see" the changes that happened
        if r_pos + 1 >= w_pos {
            w_pos = writer
                .flush()
                .and_then(|_| writer.stream_position())
                .map_err(|e| format!("flush/seek writer {} ({})", um_path, e))? as usize
                / 10;
            progress(r_pos, w_pos0 + n_winner);
        }

        match MPosDTM::read_seq(reader) {
            Err(some) if some.kind() == UnexpectedEof => break,
            Err(other) => Err(format!("read error on {} ({})", um_path, other))?,
            Ok(item) => {
                r_pos += 1;
                if w_pos > 0 && (r_pos % (32 * 1024) == 0 || r_pos + 1 == w_pos) {
                    if sigint_received.load(atomic::Ordering::SeqCst) {
                        eprintln!("\x08\x08 canceled.");
                        sigint_received.store(false, atomic::Ordering::SeqCst);
                        return Ok(Some(r_pos));
                    }
                    progress(r_pos, w_pos0 + n_winner);
                }
                let cpos = item.mpos.mpos_to_cpos();
                if cpos == DEBUG_POSITION {
                    eprint!(" \x08");
                }
                let player = item.mpos.mpos_player();
                // loop over potential loosers
                for (lpos, _lmv) in cpos.predecessors(player) {
                    let can_looser = lpos.light_canonical();
                    if can_looser == DEBUG_POSITION {
                        eprint!(" \x08");
                    }
                    let dbu = can_looser.lookup_canonic(edb)?;
                    // is this position unknown yet?
                    if dbu.state(player.opponent()) == DRAW {
                        // make it an CANNOT_AVOID_MATE if all moves reach CAN_MATE
                        let mut all_children_can_mate = true;
                        for (wpos, wmv) in dbu.successors(player.opponent()) {
                            let win_state = match dbu.mv_impact(wmv) {
                                SIGNATURE => wpos.find_state(player, e_hash)?,
                                HARMLESS => wpos.lookup_canonic(edb)?.state(player),
                                CANONICALITY => wpos.lookup_light(edb)?.state(player),
                            };
                            all_children_can_mate = all_children_can_mate && win_state == WINS;
                            if !all_children_can_mate {
                                break;
                            } // first non can-mate is enough
                        }
                        if all_children_can_mate {
                            dbu.with_db_state_for(player.opponent(), LOST, edb);
                            n_looser += 1;
                            // this may also give rise to new can mates
                            for (canm, umv) in dbu.predecessors(player.opponent()) {
                                let w = register_winner(signature, canm, umv, item.dtm + 2, writer, edb)?;
                                n_winner += w;
                            }
                            if dbu == DEBUG_POSITION {
                                eprintln!(" \x08");
                            }
                        }
                    }
                }
            }
        }
    }
    eprintln!("done.");
    eprintln!("    Found {:>12} new WINS positions.", formatted_sz(n_winner));
    eprintln!("    Found {:>12} new LOST positions.", formatted_sz(n_looser),);
    Ok(None)
}

/// command line: rasch debug mv 12345 cpos 67543 mpos  99928763
/// (as the vscode debugger displays only raw decimal values)
pub fn debug(args: &[String]) -> Result<(), String> {
    let mut w = 0;
    let mut ehash = HashMap::new();
    let mut m_hash = HashMap::new();
    let mut d_hash = HashMap::new();
    let e_hash = &mut ehash;

    let decode_pos = |s: &str| {
        if s.starts_with("0x") {
            CPos { bits: u64::from_str_radix(&s[2..], 16).unwrap_or_default() }
        } else if s.contains('/') {
            match decodeFEN(s) {
                Ok(pos) => pos.compressed(),
                Err(s) => {
                    eprintln!("Warning: {}", s);
                    CPos { bits: 0 }
                }
            }
        } else {
            CPos { bits: u64::from_str_radix(s, 10).unwrap_or_default() }
        }
    };

    while w < args.len() {
        match args[w].as_str() {
            "mv" | "move" => {
                let u: u32 = u32::from_str_radix(args[w + 1].as_str(), 10).unwrap_or_default();
                let mv = Move { mv: u };
                eprintln!(
                    "move {:?} {:?} {:?} {}{}",
                    mv.player(),
                    mv.piece(),
                    mv.promote(),
                    mv.from(),
                    mv.to()
                );
                w += 2;
            }
            "cpos" => {
                let cpos = decode_pos(&args[w + 1]);
                eprintln!("cpos  {}  0x{:016x}  {:?} ", cpos.signature(), cpos.bits, cpos);
                w += 2;
            }
            "mpos" => {
                let mpos = decode_pos(&args[w + 1]);
                eprintln!(
                    "mpos  {}  0x{:016x}  {}",
                    mpos.signature(),
                    mpos.bits,
                    mpos.mpos_debug()
                );
                let cpos = mpos.mpos_to_cpos();
                let succ = cpos.apply(cpos.mv_from_mpos(mpos)).find(e_hash).unwrap();
                eprintln!("succ  {}  0x{:016x}  {:?} ", succ.signature(), succ.bits, succ);
                w += 2;
            }
            "dtm" => {
                let cpos = decode_pos(&args[w + 1]);
                match cpos.find_state(WHITE, e_hash) {
                    Err(s) => eprintln!("{}", s),
                    Ok(ws) => match cpos.find_state(BLACK, e_hash) {
                        Err(s) => eprintln!("{}", s),
                        Ok(bs) => {
                            let xpos = cpos.with_state(ws, bs);
                            let wdtm = xpos
                                .find_dtm(WHITE, e_hash, &mut m_hash, &mut d_hash)
                                .map_err(|_| "NO DTM".to_string());
                            let bdtm = xpos
                                .find_dtm(BLACK, e_hash, &mut m_hash, &mut d_hash)
                                .map_err(|_| "NO DTM".to_string());
                            eprintln!(
                                "cpos  {}  0x{:016x}  {:?}  DTM(w) {:?} DTM(b) {:?}",
                                xpos.signature(),
                                xpos.bits,
                                xpos,
                                wdtm,
                                bdtm
                            );
                        }
                    },
                }

                w += 2;
            }
            "succ" => {
                let cpos = decode_pos(&args[w + 1]);
                eprint!("cpos  {}  0x{:016x}  {:?}  ", cpos.signature(), cpos.bits, cpos);
                match cpos.find(e_hash) {
                    Err(s) => eprintln!("not found in EGTB {}", s),
                    Ok(epos) => {
                        if epos == cpos {
                            eprintln!("is canonic");
                        } else {
                            eprintln!("is not canonic");
                            eprint!("epos  {}  0x{:016x}  {:?}  ", cpos.signature(), cpos.bits, cpos);
                        }
                        for p in [BLACK, WHITE] {
                            if epos.state(p) != INVP {
                                for (succ, mv) in epos.mpos_to_cpos().successors(p) {
                                    eprintln!(
                                        "    {:?}  by {}  {}  0x{:016x}  {:?}",
                                        p,
                                        mv,
                                        succ.signature(),
                                        succ.bits,
                                        succ
                                    );
                                    match succ.find(e_hash) {
                                        Err(s) => eprintln!("    error {}", s),
                                        Ok(esuc) => eprintln!(
                                            "    canonic         {}  0x{:016x}  {:?}",
                                            esuc.signature(),
                                            esuc.bits,
                                            esuc,
                                        ),
                                    }
                                }
                            }
                        }
                    }
                }
                w += 2;
            }
            "looser" => {
                let player = if args[w + 1].starts_with("w") { WHITE } else { BLACK };
                let wpos = decode_pos(&args[w + 2]).mpos_to_cpos().find(e_hash)?;
                w += 3;
                let signature = wpos.signature();
                for (pred, _mv) in wpos.predecessors(player) {
                    let wins = pred.find(e_hash)?;
                    let dtm = pred.find_dtm(player.opponent(), e_hash, &mut m_hash, &mut d_hash)?;
                    let mpos = wins.find_canonic_mpos(wins.signature(), player.opponent(), &mut m_hash)?;
                    eprintln!(
                        "pred    0x{:013x}  {}  {}  DTM {:5}",
                        wins.mpos_to_cpos().bits,
                        mpos.mpos_debug(),
                        wins.signature(),
                        dtm
                    );
                }
                eprint!(
                    "looser  0x{:013x}  {:?}  {}",
                    wpos.mpos_to_cpos().bits,
                    wpos,
                    signature
                );
                if wpos.state(player) == LOST {
                    let dtm = wpos.find_dtm(player, e_hash, &mut m_hash, &mut d_hash)?;
                    eprintln!("  DTM {:5}  {}", dtm, encodeFEN(&wpos.uncompressed(player)));
                } else {
                    eprintln!("  not a looser");
                }

                for (succ, mv) in wpos.successors(player)
                // .filter(|(c, m)| c.find_state(player.opponent(), e_hash) == Ok(LOST))
                {
                    // if succ.find_state(player.opponent(), e_hash) == Ok(LOST) {
                    let wins = { succ.find(e_hash) }?;
                    let winner = if wins.canonic_has_bw_switched() { player } else { player.opponent() };
                    let dtm = succ.find_dtm(player.opponent(), e_hash, &mut m_hash, &mut d_hash)?;
                    eprintln!(
                        "by {:5} 0x{:013x}  {:?}  {}  DTM {:5}  {}",
                        mv,
                        wins.mpos_to_cpos().bits,
                        wins,
                        wins.signature(),
                        dtm,
                        encodeFEN(&wins.uncompressed(winner))
                    );
                    // }
                }
            }
            "winner" => {
                let player = if args[w + 1].starts_with("w") { WHITE } else { BLACK };
                let wpos = decode_pos(&args[w + 2]).mpos_to_cpos().find(e_hash)?;
                w += 3;
                let signature = wpos.signature();
                for (pred, _mv) in wpos.predecessors(player) {
                    if pred.find_state(player.opponent(), e_hash) == Ok(LOST) {
                        let loos = pred.find(e_hash)?;
                        let dtm = pred.find_dtm(player.opponent(), e_hash, &mut m_hash, &mut d_hash)?;
                        // let mpos = wins.find_canonic_mpos(wins.signature(), player.opponent(), &mut m_hash)?;
                        eprintln!(
                            "pred    0x{:013x}  {:?}  {}  DTM {:5}",
                            loos.mpos_to_cpos().bits,
                            loos,
                            loos.signature(),
                            dtm
                        );
                    }
                }
                eprint!(
                    "winner  0x{:013x}  {:?}  {}",
                    wpos.mpos_to_cpos().bits,
                    wpos,
                    signature
                );
                if wpos.state(player) == WINS {
                    let dtm = wpos.find_dtm(player, e_hash, &mut m_hash, &mut d_hash)?;
                    eprintln!("  DTM {:5}  {}", dtm, encodeFEN(&wpos.uncompressed(player)));
                } else {
                    eprintln!("  not a winner");
                }
                let mpos = wpos.find_canonic_mpos(signature, player, &mut m_hash)?;
                eprintln!(
                    "mpos    0x{:013x}  {}  {}",
                    mpos.mpos_to_cpos().bits,
                    mpos.mpos_debug(),
                    signature
                );
                for (succ, mv) in wpos.successors(player)
                // .filter(|(c, m)| c.find_state(player.opponent(), e_hash) == Ok(LOST))
                {
                    if succ.find_state(player.opponent(), e_hash) == Ok(LOST) {
                        let lost = { succ.find(e_hash) }?;
                        let looser = if lost.canonic_has_bw_switched() { player } else { player.opponent() };
                        let dtm = succ.find_dtm(player.opponent(), e_hash, &mut m_hash, &mut d_hash)?;
                        eprintln!(
                            "by {:5} 0x{:013x}  {:?}  {}  DTM {:5}  {}",
                            mv,
                            lost.mpos_to_cpos().bits,
                            lost,
                            lost.signature(),
                            dtm,
                            encodeFEN(&lost.uncompressed(looser))
                        );
                    }
                }
            }
            "list" => {
                let signature = Signature::new_from_str_canonic(&args[w + 1])?;
                let start = usize::from_str_radix(&args[w + 2], 10).map_err(|e| format!("{}", e))?;
                let upto = usize::from_str_radix(&args[w + 3], 10).map_err(|e| format!("{}", e))?;
                w += 4;
                let mut n_item = 0;
                for cp in signature.first() {
                    if n_item >= upto {
                        break;
                    }
                    let cc = cp.find(e_hash)?;
                    assert_eq!(cc, cp);
                    if n_item >= start {
                        println!("{:6}  {}  0x{:016x}  {:?} ", n_item, cc.signature(), cc.bits, cc);
                    }
                    n_item += 1;
                }
            }
            bad => {
                eprintln!("`{}` not supported", bad);
                w += 1;
            }
        }
    }
    Ok(())
}

#[derive(Clone)]
struct Variant(CPos, Vec<Move>);

/// command line: rasch win FEN
pub fn check_win(fen: &str) -> Result<(), String> {
    let pos = decodeFEN(fen)?;
    let fenfields = fen.split(' ').collect::<Vec<_>>();
    let halfmoves = usize::from_str_radix(fenfields[5], 10).map_err(|e| format!("{}", e))?;
    let winner = pos.turn();
    let looser = winner.opponent();
    if pos.valid() { Ok(()) } else { Err("invalid position".to_string()) }?;
    if pos.validEndgame() { Ok(()) } else { Err("no endgame".to_string()) }?;
    let mut e_hash = HashMap::new();
    let mut m_hash = HashMap::new();
    let cpos = pos.compressed();
    let mv = cpos.find_move(winner, &mut e_hash, &mut m_hash)?;

    let mut variants = Vec::new();
    variants.push(Variant(cpos.apply(mv), vec![mv]));

    while !variants.is_empty() {
        match variants.pop() {
            None => {}
            Some(Variant(lost, mvs)) => {
                let st = lost.find_state(looser, &mut e_hash)?;
                if st == LOST { Ok(()) } else { Err("NOT LOST".to_string()) }?;
                let mut found = 0;
                if halfmoves < 2 || mvs.len() < halfmoves {
                    for mv in lost.move_iterator(looser) {
                        let wins = lost.apply(mv);
                        if wins.valid(winner) {
                            found += 1;
                            let mut mvs2 = mvs.clone();
                            mvs2.push(mv);
                            let wmv = wins.find_move(winner, &mut e_hash, &mut m_hash)?;
                            mvs2.push(wmv);
                            variants.push(Variant(wins.apply(wmv), mvs2));
                        }
                    }
                    if found == 0 {
                        println!("{} mate", showMovesSAN(&mvs, pos));
                    }
                } else {
                    println!("{}", showMovesSAN(&mvs, pos),);
                }
            }
        }
    }
    Ok(())
}

pub fn check_sane_via_moves(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let mut e_hash = HashMap::new();
    let mut m_hash = HashMap::new();
    let mut d_hash = HashMap::new();
    let moves_path = mk_egtb_path(signature, "moves");
    let dtm_path = mk_egtb_path(signature, "dtm");
    let (_map1, mdb) = cpos_rw_map(&moves_path)?;
    let (_map2, ddb) = short_rw_map(&dtm_path)?;

    dtm_opt(signature, ddb, mdb, &mut e_hash, &mut m_hash, &mut d_hash).map(|_| ())
}

pub fn dtm_command(sig: &str, fwd: bool) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let dtm_path = mk_egtb_path(signature, "dtm");
    let moves_path = mk_egtb_path(signature, "moves");
    let path_dtm = Path::new(&dtm_path);
    let remove_on_error = !path_dtm.is_file();
    match dtm_run(signature, fwd, (&dtm_path).to_string(), moves_path) {
        Ok(()) => Ok(()),
        Err(s) => {
            if remove_on_error {
                remove_file(path_dtm).unwrap_or_default();
            };
            Err(s)
        }
    }
}

pub fn dtm_run(signature: Signature, fwd: bool, dtm_path: String, moves_path: String) -> Result<(), String> {
    let mut m_hash = HashMap::new();
    let mut d_hash = HashMap::new();
    let mut e_hash = HashMap::new();

    let (_map2, mdb) = cpos_rw_map(&moves_path)?;

    if !Path::new(&dtm_path).is_file() {
        dtm_init(&dtm_path, mdb)?;
    }

    let (_map1, ddb) = short_rw_map(&dtm_path)?;

    if fwd {
        loop {
            eprint!("{}  compute DTM ", signature);
            progress(0, 0);
            dtm_mate_aliens(signature, ddb, mdb, &mut d_hash, &mut m_hash, &mut e_hash)?;
            eprint!("{}  optimize moves  ", signature);
            progress(0, 0);
            match dtm_opt(signature, ddb, mdb, &mut e_hash, &mut m_hash, &mut d_hash) {
                Ok(0) => break,
                Ok(_) => continue,
                Err(s) => return Err(s),
            }
        }
        Ok(())
    } else {
        eprint!("{}  compute DTM and optimize moves backwards ", signature);
        progress(0, 0);
        dtm_opt_backwards(signature, ddb, mdb, &mut e_hash, &mut m_hash, &mut d_hash)
        // .and_then(|_| dtm_opt(signature, ddb, mdb, &mut e_hash, &mut m_hash, &mut d_hash).map(|_| ()))
    }
}

pub fn dtm_init(dtm_path: &str, mdb: &[CPos]) -> Result<(), String> {
    let (_map, ddb) = byte_anon_map(mdb.len() * 2)?;
    ddb.fill(0xff);
    let mut writer = cpos_create_writer(dtm_path)?;
    writer
        .write_all(ddb)
        .map_err(|e| format!("write error on {} ({})", dtm_path, e))
}

/// compute DTM of won position
pub fn compute_wins_dtm(
    signature: Signature,
    wins: CPos,
    winner: Player,
    ddb: &mut [u16],
    mdb: &mut [CPos],
    d_hash: &mut DtmMap,
    m_hash: &mut MovesMap,
    e_hash: &mut EgtbMap,
) -> Result<u16, String> {
    let looser = winner.opponent();
    let mv = if wins.signature() == signature {
        let canonic = wins.clear_trans().find(e_hash)?;
        let player = if canonic.canonic_has_bw_switched() { looser } else { winner };
        let mpos = CPos {
            bits: (canonic.bits & CPos::COMP_BITS) | if player == WHITE { CPos::WHITE_BIT } else { 0 },
        };
        let u = mdb
            .binary_search(&mpos)
            .map_err(|_| "WINNER NOT FOUND LOCALLY".to_string())?;
        canonic.mv_from_mpos(mdb[u])
    } else {
        wins.find_move(winner, e_hash, m_hash)?
    };
    let lost = wins.apply(mv).find(e_hash)?;
    compute_lost_dtm(signature, lost, looser, ddb, mdb, d_hash, m_hash, e_hash).map(|u| u + 1)
}

/// compute the DTM of a lost position
pub fn compute_lost_dtm(
    signature: Signature,
    lost: CPos,
    looser: Player,
    ddb: &mut [u16],
    mdb: &mut [CPos],
    d_hash: &mut DtmMap,
    m_hash: &mut MovesMap,
    e_hash: &mut EgtbMap,
) -> Result<u16, String> {
    let mut lost_dtm = 0;
    let winner = looser.opponent();

    for (wins, _) in lost.successors(looser) {
        let w_dtm = if wins.signature() == signature {
            let wins = wins.find(e_hash)?;
            let wmov = CPos {
                bits: if winner == WHITE { CPos::WHITE_BIT } else { 0 } | (wins.bits & CPos::COMP_BITS),
            };
            let u = mdb
                .binary_search(&wmov)
                .map_err(|_| "NOT FOUND LOCALLY".to_string())?;
            if ddb[u] == 0xffff {
                ddb[u] = compute_wins_dtm(signature, wins, winner, ddb, mdb, d_hash, m_hash, e_hash)?;
            }
            ddb[u]
        } else {
            wins.find_dtm(winner, e_hash, m_hash, d_hash)?
        };

        if w_dtm > lost_dtm {
            lost_dtm = w_dtm;
        }
    }
    Ok(if lost_dtm > 0 { lost_dtm + 1 } else { lost_dtm })
}

pub fn dtm_mate_aliens(
    signature: Signature,
    ddb: &mut [u16],
    mdb: &mut [CPos],
    d_hash: &mut DtmMap,
    m_hash: &mut MovesMap,
    e_hash: &mut EgtbMap,
) -> Result<(), String> {
    let mut n_done = 0usize;
    let mut max_dtm = 0u16;

    ddb.fill(0xffff);

    for item in 0..mdb.len() {
        if item & 0x0f_ffff == 0 {
            progress(n_done, mdb.len());
        }
        let mpos = mdb[item];
        let cpos = mpos.mpos_to_cpos().find_canonic(signature, e_hash)?;
        let winner = mpos.mpos_player();
        let mv = cpos.mv_from_mpos(mpos);
        let lost = cpos.apply(mv);
        let looser_stat = lost.find_state(winner.opponent(), e_hash)?;
        let winner_stat = lost.find_state(winner, e_hash)?;
        assert!(looser_stat == LOST); // because it was a winning move
        if lost.signature() != signature {
            ddb[item] = 1 + lost.find_dtm(winner.opponent(), e_hash, m_hash, d_hash)?;
            n_done += 1;
            max_dtm = max_dtm.max(ddb[item]);
        } else if winner_stat == INVP && lost.successors(winner.opponent()).next().is_none() {
            ddb[item] = 1;
            n_done += 1;
            max_dtm = max_dtm.max(1);
        }
    }
    progress(n_done, mdb.len());
    // eprintln!("  found {}", n_done);

    // eprint!("{}  find subsequent DTM  ", signature);
    // progress(0, 0);

    for item in 0..mdb.len() {
        if item & 0x0f_ffff == 0 {
            progress(item, mdb.len());
        }
        if ddb[item] < 0xffff {
            continue;
        }
        let mpos = mdb[item];
        let wins = mpos.mpos_to_cpos().find_canonic(signature, e_hash)?;
        let winner = mpos.mpos_player();
        ddb[item] = compute_wins_dtm(signature, wins, winner, ddb, mdb, d_hash, m_hash, e_hash)?;
        max_dtm = max_dtm.max(ddb[item]);
    }
    progress(999, 1000);
    // assert!(ddb.iter().find(|x| **x == 0xffff).is_none());
    eprintln!("done, max DTM {}", max_dtm);
    Ok(())
}

pub fn dtm_opt(
    signature: Signature,
    ddb: &mut [u16],
    mdb: &mut [CPos],
    e_hash: &mut EgtbMap,
    m_hash: &mut MovesMap,
    d_hash: &mut DtmMap,
) -> Result<usize, String> {
    CPos::egtb_mmap(signature, "egtb", e_hash)?;
    let edb = e_hash.get(&signature).unwrap().1;

    let mut n_insane = 0usize;
    let mut w_king = H8;

    for n_item in 0..mdb.len() {
        if n_item & 0x1_ffff == 0 {
            progress(n_item, mdb.len());
            if sigint_received.load(atomic::Ordering::SeqCst) {
                eprintln!("\x08\x08 canceled.");
                sigint_received.store(false, atomic::Ordering::SeqCst);
                return Err("canceled".to_uppercase());
            }
        }
        let mpos = mdb[n_item];
        let winner = mpos.mpos_player();
        let looser = winner.opponent();
        let cpos = mpos.mpos_to_cpos().lookup_canonic(edb)?;
        let cpos_dtm = ddb[n_item];
        let cpos_mv = cpos.mv_from_mpos(mpos);
        let mut better_mv = cpos_mv;
        let mut better_dtm = cpos_dtm;
        for (succ, mv) in cpos.successors(winner) {
            let impact = cpos.mv_impact(mv);
            let lost = match impact {
                HARMLESS => succ.lookup_canonic(edb)?,
                CANONICALITY => succ.canonical(signature).lookup_canonic(edb)?,
                SIGNATURE => succ.clear_trans().find(e_hash)?,
            };
            if lost.state(if lost.canonic_has_bw_switched() { winner } else { looser }) != LOST {
                continue;
            }
            let dtm = if impact == SIGNATURE {
                succ.find_dtm(looser, e_hash, m_hash, d_hash)?
            } else {
                let mut lost_dtm = 0;

                // lost is canonic
                for (succ, mv) in lost.successors(looser) {
                    let impact = lost.mv_impact(mv);
                    let wins = match impact {
                        HARMLESS => succ,
                        CANONICALITY => succ.canonical(signature),
                        SIGNATURE => succ,
                    };
                    let w_dtm = if impact != SIGNATURE {
                        // wins is canonical
                        let wmov = CPos {
                            bits: if winner == WHITE { CPos::WHITE_BIT } else { 0 }
                                | (wins.bits & CPos::COMP_BITS),
                        };
                        let u = mdb
                            .binary_search(&wmov)
                            .map_err(|_| "NOT FOUND LOCALLY".to_string())?;
                        if ddb[u] == 0xffff {
                            ddb[u] =
                                compute_wins_dtm(signature, wins, winner, ddb, mdb, d_hash, m_hash, e_hash)?;
                        }
                        ddb[u]
                    } else {
                        wins.find_dtm(winner, e_hash, m_hash, d_hash)?
                    };

                    if w_dtm > lost_dtm {
                        lost_dtm = w_dtm;
                    }
                    if lost_dtm + 2 >= better_dtm {
                        break;
                    }
                }
                if lost_dtm > 0 {
                    lost_dtm + 1
                } else {
                    lost_dtm
                }
            };
            if dtm + 1 < better_dtm {
                better_mv = mv;
                better_dtm = dtm + 1;
            }
        }
        if better_dtm < cpos_dtm {
            if mpos.white_king() != w_king {
                eprintln!();
                eprint!(
                    "    {}  DTM {:2} by {} → DTM {:2} by {}            ",
                    mpos.mpos_debug(),
                    cpos_dtm,
                    cpos_mv,
                    better_dtm,
                    better_mv
                );
                progress(n_item, mdb.len());
                w_king = mpos.white_king();
            }
            n_insane += 1;
            mdb[n_item] = cpos.mpos_from_mv(better_mv);
            ddb[n_item] = better_dtm;
        }
    }
    progress(999, 1000);
    eprintln!(" {} optimized.", n_insane);
    Ok(n_insane)
}

pub fn dtm_opt_backwards(
    signature: Signature,
    ddb: &mut [u16],
    mdb: &mut [CPos],
    e_hash: &mut EgtbMap,
    m_hash: &mut MovesMap,
    d_hash: &mut DtmMap,
) -> Result<(), String> {
    CPos::egtb_mmap(signature, "egtb", e_hash)?;
    let edb = e_hash.get(&signature).unwrap().1;
    let mut stack1: LooserSet = LooserSet::new();

    let _shutup: HashSet<MPos> = HashSet::new();

    ddb.fill(0xffff);
    unsafe {
        n_winners = 0;
        if mdb.len() > 0 {
            progress(n_winners, mdb.len());
        }
    }

    // find alien only winners (there is no LOST successor in this signature) and initialize their dtm

    for alien_u in 0..mdb.len() {
        let wins = mdb[alien_u].mpos_to_cpos();
        let mv = wins.mv_from_mpos(mdb[alien_u]);
        let winner = mv.player();
        let looser = winner.opponent();
        let impact = wins.mv_impact(mv);
        if impact == SIGNATURE {
            // does it have domestic looser successors?
            if wins
                .successors(winner)
                .filter(|(succ, _)| succ.signature() == signature)
                .map(|(succ, _)| succ.light_canonical().lookup_canonic(edb))
                .find(|r| match r {
                    Ok(found) => found.state(looser) == LOST,
                    Err(_) => false,
                })
                .is_none()
            {
                ddb[alien_u] = 1 + wins.apply(mv).find_dtm(looser, e_hash, m_hash, d_hash)?;
                unsafe {
                    n_winners += 1;
                }
                /*
                stack1.extend(
                    wins.canonic_predecessors(mv.player())
                        .map(|(c, _mv)| c.lookup_canonic(edb).unwrap())
                        .filter(|c| c.state(mv.player().opponent()) == LOST)
                        .map(|c| c.cpos_to_mpos(mv.player().opponent())),
                );
                */
            }
        }
    }

    for it in signature.first() {
        let mate_pos = it.lookup_canonic(edb)?;
        for looser in [BLACK, WHITE] {
            let winner = looser.opponent();
            if mate_pos.state(looser) == LOST
                && mate_pos.state(winner) == INVP
                && mate_pos.successors(looser).next().is_none()
            {
                stack1.insert(mate_pos.cpos_to_mpos(looser));
            }
        }
    }
    // main loop
    //let mut curr_dtm = 0;
    while !stack1.is_empty() {
        let mut stack2: LooserSet = LooserSet::new();
        for mpos in stack1.iter() {
            inspect_loss_backwd(mpos, &mut stack2, edb, ddb, mdb, e_hash, m_hash, d_hash)?;
        }
        stack1.clear();
        stack1 = stack2;
    }

    unsafe {
        if stack1.is_empty() && mdb.len() > 0 && n_winners > 0 {
            progress(n_winners - 1, mdb.len());
        }
    }

    let mut max_dtm = 0;

    for u in 0..ddb.len() {
        if ddb[u] == 0xffff {
            /*
            let mpos = mdb[u];
            eprintln!(
                "mpos    0x{:013x}  {}  {}  DTM {:5}",
                mpos.mpos_to_cpos().bits,
                mpos.mpos_debug(),
                signature,
                ddb[u]
            );
            */
            let mut inuse = HashSet::new();
            make_wins_dtm(u, &mut inuse, edb, mdb, ddb, e_hash, m_hash, d_hash)?;
            /*
            eprintln!(
                "mpos    0x{:013x}  {}  {}  DTM {:5}",
                mpos.mpos_to_cpos().bits,
                mpos.mpos_debug(),
                signature,
                ddb[u]
            );
            */
        }
        if ddb[u] == 0xffff {
            eprintln!();
            eprintln!(
                "{}  move  {}  ({:#013x}) has DTM 0xffff",
                signature,
                mdb[u].mpos_debug(),
                mdb[u].mpos_to_cpos().bits
            );
            return Err("assumption violated".to_uppercase());
        }
        max_dtm = max_dtm.max(ddb[u]);
    }
    eprintln!("done, max DTM {}", max_dtm);
    Ok(())
}

fn make_wins_dtm(
    u: usize,
    inuse: &mut HashSet<MPos>,
    edb: &[u8],
    mdb: &mut [CPos],
    ddb: &mut [u16],
    e_hash: &mut EgtbMap,
    m_hash: &mut MovesMap,
    d_hash: &mut DtmMap,
) -> Result<u16, String> {
    let mpos = mdb[u];
    let cpos = mpos.mpos_to_cpos();
    let winner = mpos.mpos_player();
    let mut unresolved_loosers = Vec::new();

    for (succ, mv) in cpos.successors(winner) {
        let impact = cpos.mv_impact(mv);
        let lost = match impact {
            HARMLESS => succ.lookup_canonic(edb),
            CANONICALITY => succ.light_canonical().lookup_canonic(edb),
            SIGNATURE => succ.find(e_hash),
        }?;
        let looser = if lost.canonic_has_bw_switched() { winner } else { winner.opponent() };
        if lost.state(looser) == LOST {
            let dtm = 1 + if impact == SIGNATURE {
                succ.find_dtm(winner.opponent(), e_hash, m_hash, d_hash)
            } else {
                current_looser_dtm(&lost.cpos_to_mpos(looser), ddb, mdb, e_hash, m_hash, d_hash)
            }?;
            if dtm == 0xffff {
                unresolved_loosers.push((lost.cpos_to_mpos(looser), mv));
            }
            if dtm < ddb[u] {
                // more noise
                if ddb[u] == 0xffff {
                    unsafe {
                        n_winners += 1;
                        if (n_winners & 0x3ffff) == 0x3ffff || n_winners + 1 == mdb.len() {
                            progress(n_winners, mdb.len());
                        }
                    }
                }
                mdb[u] = cpos.mpos_from_mv(mv);
                ddb[u] = dtm;
            }
        }
    }
    if ddb[u] == 0xffff {
        if unresolved_loosers.iter().all(|(m, _)| inuse.contains(m)) {
            eprint!(
                "\nwinner  0x{:013x}  {:?}  {} successors in progress ",
                cpos.bits,
                cpos,
                unresolved_loosers.len()
            );
            return Err("RECURSION".to_string());
        }
        for (mpos, mv) in unresolved_loosers.iter().copied() {
            if !inuse.contains(&mpos) {
                let dtm = match make_looser_dtm(&mpos, inuse, edb, ddb, mdb, e_hash, m_hash, d_hash) {
                    Err(s) if s == "RECURSION" => continue,
                    Err(s) => return Err(s),
                    Ok(u) => 1 + u,
                };
                if dtm < ddb[u] {
                    // more noise
                    if ddb[u] == 0xffff {
                        unsafe {
                            n_winners += 1;
                            if (n_winners & 0x3ffff) == 0x3ffff || n_winners + 1 == mdb.len() {
                                progress(n_winners, mdb.len());
                            }
                        }
                    }
                    mdb[u] = cpos.mpos_from_mv(mv);
                    ddb[u] = dtm;
                    break;
                }
            }
        }
        // still nothing
        if ddb[u] == 0xffff {
            return Err("RECURSION".to_string());
        }
    }
    Ok(ddb[u])
}

fn make_looser_dtm(
    lost: &MPos,
    inuse: &mut HashSet<MPos>,
    edb: &[u8],
    ddb: &mut [u16],
    mdb: &mut [MPos],
    e_hash: &mut EgtbMap,
    m_hash: &mut MovesMap,
    d_hash: &mut DtmMap,
) -> Result<u16, String> {
    let looser = lost.mpos_player();
    let winner = looser.opponent();
    let mut dtm = 0u16;

    inuse.insert(*lost);

    for (succ, mv) in lost.mpos_to_cpos().successors(looser) {
        dtm = dtm.max(1); // because we do have successors
        let impact = lost.mv_impact(mv);
        let wins = match impact {
            HARMLESS => succ,
            CANONICALITY => succ.light_canonical(),
            SIGNATURE => succ,
        };
        let w_dtm = if impact != SIGNATURE {
            let wmov = wins.cpos_to_mpos(winner);
            let u = mdb
                .binary_search(&wmov)
                .map_err(|_| "LOOSER SUCC NOT FOUND".to_string())?;
            if ddb[u] == 0xffff {
                make_wins_dtm(u, inuse, edb, mdb, ddb, e_hash, m_hash, d_hash)
            } else {
                Ok(ddb[u])
            }
        } else {
            wins.find_dtm(winner, e_hash, m_hash, d_hash)
        };
        match w_dtm {
            Err(s) if s == "RECURSION" => continue,
            Err(s) => {
                inuse.remove(lost);
                return Err(s);
            }
            Ok(odd) => {
                assert!(odd & 1 == 1 && odd != 0xffff);
                dtm = dtm.max(odd + 1);
            }
        }
    }

    inuse.remove(lost);
    if dtm == 1 {
        Err("RECURSION".to_string())
    } else {
        Ok(dtm)
    }
}

type LooserSet = BTreeSet<MPos>;

fn current_looser_dtm(
    lost: &MPos,
    ddb: &mut [u16],
    mdb: &mut [MPos],
    e_hash: &mut EgtbMap,
    m_hash: &mut MovesMap,
    d_hash: &mut DtmMap,
) -> Result<u16, String> {
    let looser = lost.mpos_player();
    let winner = looser.opponent();
    let mut dtm = 0u16;

    for (succ, mv) in lost.mpos_to_cpos().successors(looser) {
        let impact = lost.mv_impact(mv);
        let wins = match impact {
            HARMLESS => succ,
            CANONICALITY => succ.light_canonical(),
            SIGNATURE => succ,
        };
        dtm = dtm.max(if impact != SIGNATURE {
            let wmov = wins.cpos_to_mpos(winner);
            let u = mdb
                .binary_search(&wmov)
                .map_err(|_| "LOOSER SUCC NOT FOUND".to_string())?;
            ddb[u]
        } else {
            wins.find_dtm(winner, e_hash, m_hash, d_hash)?
        });
        if dtm == 0xffff {
            return Ok(dtm - 1);
        }
    }
    Ok(if dtm == 0 { 0 } else { 1 + dtm })
}

fn inspect_loss_backwd(
    lost: &MPos,
    stack: &mut LooserSet,
    edb: &[u8],
    ddb: &mut [u16],
    mdb: &mut [MPos],
    e_hash: &mut EgtbMap,
    m_hash: &mut MovesMap,
    d_hash: &mut DtmMap,
) -> Result<(), String> {
    let looser = lost.mpos_player();
    let winner = looser.opponent();

    // are we ready to do this yet
    let dtm = current_looser_dtm(lost, ddb, mdb, e_hash, m_hash, d_hash)?;

    if dtm >= 0xfffe {
        // stack.insert(*lost);
        return Ok(());
    }

    let preds: Vec<(CPos, Move)> = lost.predecessors(looser).collect();

    // we must first mark all winners and then explore them further
    for (wins, mv) in preds {
        assert_eq!(mv.player(), winner);
        let cwin = wins.light_canonical().lookup_canonic(edb)?;
        if optimal_winner(cwin, mv, dtm + 1, ddb, mdb, e_hash, m_hash, d_hash)? {
            stack.extend(
                cwin.predecessors(winner)
                    .map(|(c, _mv)| c.light_canonical().lookup_canonic(edb).unwrap())
                    .filter(|c| c.state(looser) == LOST)
                    .map(|c| c.cpos_to_mpos(looser)),
            );
        }
    }

    Ok(())
}

static mut n_winners: usize = 0;

fn optimal_winner(
    wins: CPos,
    mv: Move,
    dtm: u16,
    ddb: &mut [u16],
    mdb: &mut [CPos],
    e_hash: &mut EgtbMap,
    m_hash: &mut MovesMap,
    d_hash: &mut DtmMap,
) -> Result<bool, String> {
    let mpos = wins.mpos_from_mv(mv);
    if wins == DEBUG_POSITION {
        eprintln!(" \x08");
    }
    let u = mdb.binary_search(&mpos).map_err(|_| {
        format!(
            "LOOSER PRED {:#016x} BY {} NOT FOUND {}",
            wins.bits,
            mv,
            mpos.mpos_debug()
        )
    })?;
    // this will get updated now, let's take the opportunity to make some noise
    if ddb[u] == 0xffff {
        unsafe {
            n_winners += 1;
            if (n_winners & 0x3ffff) == 0x3ffff || n_winners + 1 == mdb.len() {
                progress(n_winners, mdb.len());
            }
        }
    }
    let other_mv = wins.mv_from_mpos(mdb[u]);
    if other_mv != mv && wins.mv_impact(other_mv) == SIGNATURE {
        // alien move
        let alien_dtm =
            1 + wins
                .apply(other_mv)
                .find_dtm(other_mv.player().opponent(), e_hash, m_hash, d_hash)?;
        if alien_dtm < dtm {
            ddb[u] = alien_dtm;
            return Ok(true);
        }
    }
    if dtm < ddb[u] {
        mdb[u] = mpos;
        ddb[u] = dtm;
        Ok(true)
    } else {
        Ok(false)
    }
}
