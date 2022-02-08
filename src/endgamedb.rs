#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

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
    basic::{CPosState, Move, Piece, Player},
    cpos::{CPos, EgtbMap, Mirrorable, MovesMap, Signature},
    cposio::{
        byte_anon_map, byte_ro_map, byte_rw_map, cpos_append_writer, cpos_create_writer, cpos_file_size,
        cpos_open_reader, cpos_ro_map, mk_egtb_path, mk_temp_path, CPosReader,
    },
    fen::{decodeFEN, encodeFEN},
    position::{showMovesSAN, Position},
    sortegtb::sort_from_to,
    util::*,
};

// use super::fieldset::*;
// use super::position as P;

use CPosState::*;
use Piece::*;
use Player::*;

use std::{
    collections::HashMap,
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
}

use MakeState::*;

static sigint_received: atomic::AtomicBool = atomic::AtomicBool::new(false);
static handler_installed: atomic::AtomicBool = atomic::AtomicBool::new(false);

/// Sort the um file, if present.
///
/// results in `Done`
fn make_moves(signature: Signature, um_path: &str, moves_path: &str) -> Result<MakeState, String> {
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
            sort_from_to(signature, um_path, moves_path).map(|_| Done)
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
    let (_map, array) = byte_anon_map(&map_size)?;

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
    let sig = cpos.signature();
    println!("# {} is canonic {}", sig, sig.is_canonic());
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
                "# {:?} to move will enforce mate with {}",
                mv.player(),
                mv.showSAN(*pos)
            );
            Ok(mv)
        }
        DRAW | LOST => {
            let moves = pos.moves();
            if moves.is_empty() {
                let what = if s == LOST { "mate" } else { "stalemate" };
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
    let mut last = CPos { bits: 0 };
    let mut sorted = true;
    let mut dupl = 0usize;
    let mut unrelated = false;
    let mut invalid = false;
    let mut badmoves = 0usize;
    let mut badmpos = 0usize;
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
                Some(_mv) => badmoves,
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

    std::mem::drop(epositions.0);

    if sorted && dupl == 0 && !unrelated && !invalid && badmoves == 0 && badmpos == 0 {
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
    let mut cad_both = 0usize;
    let mut cad_example = c0;
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
        if total % (1024 * 1024 * 4) == 1 || current == last {
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

        for p in [BLACK, WHITE] {
            if c.state(p) == WINS {
                let m =
                    CPos { bits: if p == WHITE { CPos::WHITE_BIT } else { 0 } | (c.bits & CPos::COMP_BITS) };
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
    eprintln!("{:>12} positions total", formatted_sz(total));
    if moves_missing > 0 {
        eprintln!("{:>12} moves are missing", formatted_sz(moves_missing));
    }
    if poss_missing > 0 {
        eprintln!("{:>12} positions are missing", formatted_sz(poss_missing));
    }

    if moves_missing > 0 || poss_missing > 0 {
        eprintln!("Warning: This is an invalid or yet incomplete end game table.");
        Err("corrupt egtb".to_string())
    } else {
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
    let work_path = mk_egtb_path(signature, "work");
    let um_path = mk_temp_path(signature, "um");
    let restart_path = mk_egtb_path(signature, "restart");

    let mut pass = 0;
    let mut um_writer = cpos_append_writer("/dev/null")?;
    let (mut map, mut db) = byte_anon_map(&1)?;
    let mut ehash: EgtbMap = HashMap::new();

    // initial state depends on the files in the EGTB directory
    let mut state = if Path::new(&egtb_path).is_file() {
        if Path::new(&moves_path).is_file() {
            Done
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
                state = make_moves(signature, &um_path, &moves_path)?;
                check_egtb(&signature.display())?;
                check_moves(&signature.display())?;
            }

            StartUp => {
                remove_file(&moves_path).unwrap_or_default();
                state = make_work(signature, pass, &work_path)?;
            }

            WriteEgtb => {
                eprintln!("{}  Pass {:2} - finishing {}", signature, pass, egtb_path);
                std::mem::drop(map);
                let (xmap, xdb) = byte_anon_map(&1)?;
                map = xmap;
                db = xdb;
                rename(Path::new(&work_path), Path::new(&egtb_path))
                    .map_err(|e| format!("mv {} {} failed ({})", work_path, egtb_path, e))?;
                state = EgtbPresent;
            }

            WorkPresent => {
                eprintln!("{}  Pass {:2} - continuing with {}", signature, pass, work_path);
                remove_file(&moves_path).unwrap_or_default();
                std::mem::drop(map);
                // from this point on, the map is connected to the working file
                // and um_writer writes to the um file
                {
                    let (xmap, xdb) = byte_rw_map(&work_path)?;
                    map = xmap;
                    db = xdb;

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
                    fmt_human(db.len(), 0)
                );
                let result = scan_mates_aliens(signature, at, &mut um_writer, db, &mut ehash, true)?;
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
                    fmt_human(db.len(), 0)
                );
                let result = scan_mates_aliens(signature, at, &mut um_writer, db, &mut ehash, false)?;
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
                let mut um_reader = cpos_open_reader(&um_path)?;
                um_reader
                    .seek(SeekFrom::Start(at as u64 * 8))
                    .map_err(|e| format!("Can't seek {} to index {} ({})", um_path, at, e))?;
                let r = scan_um(signature, &mut um_reader, &mut um_writer, db, &mut ehash)?;
                eprint!("    Flushing {} and {} ... ", work_path, um_path);
                map.flush()
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

const DEBUG_POSITION: CPos = CPos { bits: 0xdaa0002103040 };

/// assert_eq!(signature, can.signature())
fn register_winner(
    signature: Signature,
    can: CPos,
    umv: Move,
    writer: &mut BufWriter<File>,
    db: &mut [u8],
) -> Result<usize, String> {
    let canonic = can.clear_trans().canonical(signature);
    if canonic == DEBUG_POSITION {
        eprint!(" \x08");
    }
    let c = canonic.lookup_canonic(db)?;
    let hmv = if canonic.canonic_was_mirrored_h() { umv.mirror_h() } else { umv };
    let mv = if canonic.canonic_was_mirrored_v() { hmv.mirror_v() } else { hmv };
    let mpos = c.mpos_from_mv(mv);
    if c.state(mv.player()) == DRAW {
        c.with_db_state_for(mv.player(), WINS, db);
        mpos.write_seq(writer).map_err(|ioe| {
            format!(
                "unexpected error ({}) while writing to {}",
                ioe,
                mk_temp_path(signature, "um")
            )
        })?;
        Ok(1)
    } else {
        Ok(0)
    }
}

fn scan_mates_aliens(
    signature: Signature,
    at: CPos,
    um_writer: &mut BufWriter<File>,
    db: &mut [u8],
    hash: &mut EgtbMap,
    mate_only: bool,
) -> Result<Option<CPos>, String> {
    let mut wins = 0usize;
    let mut loos = 0usize;
    let mut mates = 0usize;
    let mut stalemates = 0usize;
    let n_items = db.len();
    let mut n = 0;
    let start = at;
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
            let cp = current.lookup_canonic(db)?;
            if cp == DEBUG_POSITION {
                eprint!(" \x08");
            }
            if cp.state(player) == DRAW {
                let mut all_successors_can_mate = true;
                let mut no_moves = true;
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
                    let succ = succ.find(hash).map(|x| {
                        if x.canonic_has_bw_switched() {
                            x.flipped_flags()
                        } else {
                            x
                        }
                    })?;
                    match succ.state(player.opponent()) {
                        LOST => {
                            let newwin = register_winner(signature, cp, mv, um_writer, db)?;
                            wins += newwin;
                            continue 'next_player;
                        }
                        WINS => continue, /* with next move */
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
                    // || (no_moves && cp.state(player.opponent()) == INVALID_POS)
                    // it's not STALEMATE, but either MATE or all successors are alien and can mate!
                    if no_moves && cp.state(player.opponent()) == INVP {
                        mates += 1;
                    } else {
                        loos += 1;
                    }
                    cp.with_db_state_for(player, LOST, db);
                    // find all moves from same signature (no captures, no promotions) that come here
                    // they all CAN_MATE
                    for (pred, mv) in cp.predecessors(player) {
                        let found = register_winner(signature, pred, mv, um_writer, db)?;
                        wins += found;
                    }
                }
            }
        }
    }

    eprintln!("done.");
    if wins + mates + loos + stalemates > 0 {
        eprintln!("    Found {} MATE positions.", formatted_sz(mates),);
        eprintln!("    Found {} STALEMATE positions.", formatted_sz(stalemates),);
        eprintln!("    Found {} new WINS positions.", formatted_sz(wins));
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
    db: &mut [u8],
    e_hash: &mut EgtbMap,
) -> Result<Option<usize>, String> {
    let um_path = mk_temp_path(signature, "um");
    let mut r_pos = reader
        .stream_position()
        .map_err(|e| format!("seek reader {} ({})", um_path, e))? as usize
        / 8;

    let mut n_winner = 0usize;
    let mut n_looser = 0usize;

    loop {
        // make sure the reader can "see" the changes that happened
        let w_pos = writer
            .flush()
            .and_then(|_| writer.stream_position())
            .map_err(|e| format!("flush/seek writer {} ({})", um_path, e))? as usize
            / 8;

        match CPos::read_seq(reader) {
            Err(some) if some.kind() == UnexpectedEof => break,
            Err(other) => Err(format!("read error on {} ({})", um_path, other))?,
            Ok(mpos) => {
                r_pos += 1;
                if w_pos > 0 && (r_pos % (32 * 1024) == 0 || r_pos + 1 == w_pos) {
                    if sigint_received.load(atomic::Ordering::SeqCst) {
                        eprintln!("\x08\x08 canceled.");
                        sigint_received.store(false, atomic::Ordering::SeqCst);
                        return Ok(Some(r_pos));
                    }
                    progress(r_pos, w_pos);
                }
                let cpos = mpos.mpos_to_cpos();
                if cpos == DEBUG_POSITION {
                    eprint!(" \x08");
                }
                let player = mpos.mpos_player();
                // loop over potential loosers
                for (lpos, _lmv) in cpos.predecessors(player) {
                    let can_looser = lpos.clear_trans().canonical(signature);
                    if can_looser == DEBUG_POSITION {
                        eprint!(" \x08");
                    }
                    let dbu = can_looser.lookup_canonic(db)?;
                    // is this position unknown yet?
                    if dbu.state(player.opponent()) == DRAW {
                        // make it an CANNOT_AVOID_MATE if all moves reach CAN_MATE
                        let mut all_children_can_mate = true;
                        for (wpos, _wmv) in dbu
                            .move_iterator(player.opponent())
                            .map(|mv| (dbu.apply(mv), mv))
                            .filter(|(c, _mv)| c.valid(player))
                        {
                            // this could be an alien move
                            let wsig = wpos.signature();
                            let canon = wpos.canonical(wsig);
                            let chsig = canon.signature();
                            let rp = if wsig != signature {
                                // alien
                                canon.find_canonic(chsig, e_hash).map(|r| {
                                    if canon.canonic_has_bw_switched() {
                                        r.flipped_flags()
                                    } else {
                                        r
                                    }
                                })?
                            } else {
                                let dbv = canon.lookup_canonic(db)?;
                                if canon.canonic_has_bw_switched() {
                                    dbv.flipped_flags()
                                } else {
                                    dbv
                                }
                            };
                            all_children_can_mate = all_children_can_mate && rp.state(player) == WINS;
                            if !all_children_can_mate {
                                break;
                            } // first non can-mate is enough
                        }
                        if all_children_can_mate {
                            dbu.with_db_state_for(player.opponent(), LOST, db);
                            n_looser += 1;
                            // this may also give rise to new can mates
                            for (canm, umv) in dbu.predecessors(player.opponent()) {
                                let w = register_winner(signature, canm, umv, writer, db)?;
                                n_winner += w;
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
pub fn debug(args: &[String]) {
    let mut w = 0;

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
                let cpos = CPos { bits: u64::from_str_radix(args[w + 1].as_str(), 10).unwrap_or_default() };
                eprintln!("cpos  {}  0x{:016x}  {:?}", cpos.signature(), cpos.bits, cpos);
                w += 2;
            }
            "mpos" => {
                let mpos = CPos { bits: u64::from_str_radix(args[w + 1].as_str(), 10).unwrap_or_default() };
                eprintln!(
                    "cpos  {}  0x{:016x}  {}",
                    mpos.signature(),
                    mpos.bits,
                    mpos.mpos_debug()
                );
                w += 2;
            }
            bad => {
                eprintln!("`{}` not supported", bad);
                w += 1;
            }
        }
    }
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

pub fn check_sane(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let last_addr = signature.last().canonic_addr();

    eprint!("{} looking for insane mate moves  ", signature);
    progress(0, 0);
    let mut n_item = 0;
    let mut n_insane = 0;

    let mut e_hash = HashMap::new();
    let mut m_hash = HashMap::new();

    let is_mate_pos = |cp: CPos, player| {
        cp.state(player) == LOST
            && cp.state(player.opponent()) == INVP
            && cp.successors(player).next().is_none()
    };

    for current in signature.first() {
        n_item += 1;
        if n_item & 0xff_ffff == 1 {
            progress(current.canonic_addr().0, last_addr.0);
        }
        for player in [BLACK, WHITE] {
            let cp = current.find_canonic(signature, &mut e_hash)?;
            if is_mate_pos(cp, player) {
                for (pred, mv) in cp.predecessors(player) {
                    match pred.find_move(player.opponent(), &mut e_hash, &mut m_hash) {
                        Ok(back) if back == mv => {}
                        Ok(back) => {
                            let other = pred.apply(back).find(&mut e_hash).unwrap_or(pred.apply(back));
                            let alsomate = is_mate_pos(other, player);

                            if n_insane == 0 && !alsomate {
                                println!();
                                println!("    {:?} mate  {:?}", player, cp);
                                println!(
                                    "    predecessor {:?}  by {}",
                                    pred.find(&mut e_hash).unwrap_or(pred),
                                    mv
                                );
                                println!(
                                    "    leading to  {:?}  by {}  ({})",
                                    other,
                                    back,
                                    if alsomate { "mate" } else { "NOT mate" }
                                );
                            }
                            n_insane += if alsomate { 0 } else { 1 };
                        }
                        Err(s) => {
                            return Err(s);
                        }
                    }
                }
            }
        }
    }
    progress(999, 1000);
    eprintln!(" done.");
    if n_insane > 0 {
        Err(format!("{} insane mates", n_insane))
    } else {
        Ok(())
    }
}
