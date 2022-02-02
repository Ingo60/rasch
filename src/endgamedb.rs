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
    basic::{CPosState, Move, Piece, Player, PlayerPiece},
    cpos::{CPos, EgtbMMap, EgtbMap, Mirrorable, Signature},
    cposio::{
        cpos_anon_map, cpos_append_writer, cpos_create_writer, cpos_file_size, cpos_open_reader, cpos_ro_map,
        cpos_rw_map, mk_egtb_path, mk_temp_path, read_a_chunk, CPosReader,
    },
    fen::{decodeFEN, encodeFEN},
    sortegtb::sort_from_to,
};

use super::cposio as CIO;
use super::fieldset::*;
use super::position as P;

use CPosState::*;
use Piece::*;
use Player::*;

use super::position::Position;
use super::util::*;
// use crate::position::Mirrorable;

use std::env;
use std::fs::{remove_file, File};
use std::io::BufReader;
use std::io::BufWriter;
use std::io::ErrorKind::*;
use std::io::{Seek, SeekFrom, Write};
use std::{collections::HashMap, fs::rename};
// use std::iter::FromIterator;
use std::path::Path;

use std::sync::{atomic, Arc};

pub type Cache = HashMap<usize, Vec<(Move, CPos)>>;

const POSITION_NULL: Position = Position {
    hash: 0,
    flags: P::WHITE_TO_MOVE,
    whites: BitSet::empty(),
    bishopSet: BitSet::empty(),
    pawnSet: BitSet::empty(),
    rookSet: BitSet::empty(),
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
enum MakeState {
    Done,
    EgtbPresent,
    SortedPresent,
    McPresent,
    UnsortedPresent,
    StartUp,
    InterruptedMemory,
    UnsortedVec,
    PrepareAnalyzeMemory,
    AnalyzeMemory(bool),
    WriteEgtb,
    RestoreItems,
    PrepareDiskAnalysis(bool),
    ReadNextBuffer(bool),
    AnalyzeBuffer(bool),
    BufferDone(bool),
    InterruptedBuffer(usize),
    EgtbFromSorted,
    ScanForMate(usize),
    ScanUM(usize),
}

use MakeState::*;

/// state machine for making a signature
pub fn make(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let egtb_path = mk_egtb_path(signature, "egtb");
    let sorted_path = mk_egtb_path(signature, "sorted");
    let unsorted_path = mk_egtb_path(signature, "unsorted");
    let um_path = mk_egtb_path(signature, "um");
    let mc_path = mk_egtb_path(signature, "mc");
    let pass_path = mk_egtb_path(signature, "pass");
    let moves_path = mk_egtb_path(signature, "moves");

    let mut pass = 0;
    let mut positions: Vec<CPos> = Vec::with_capacity(0);
    let mut cache: Cache = HashMap::with_capacity(0);
    let mut cache_max = 0usize;
    let mut egtb_map: EgtbMMap = HashMap::new();
    let mut restart = false;
    let mut last_disk_pass = true;
    // the following will be opened (and closed) by overwriting
    let mut um_writer = cpos_append_writer("/dev/null")?;
    let mut sorted_reader = cpos_open_reader("/dev/null")?;
    let mut pass_writer = cpos_append_writer("/dev/null")?;

    // initial state depends on the files in the EGTB directory
    let mut state = if Path::new(&egtb_path).is_file() {
        if Path::new(&moves_path).is_file() {
            Done
        } else {
            EgtbPresent
        }
    } else if Path::new(&sorted_path).is_file() {
        SortedPresent
    } else if Path::new(&unsorted_path).is_file() {
        UnsortedPresent
    } else if Path::new(&mc_path).is_file() {
        McPresent
    } else {
        StartUp
    };

    let handler_ref = Arc::new(&sigint_received);
    ctrlc::set_handler(move || {
        handler_ref.store(true, atomic::Ordering::SeqCst);
    })
    .map_err(|e| format!("Cannot set CTRL-C handler ({})", e))?;

    while state != Done {
        pass += 1;
        // eprintln!("{}  Pass {:2} {:?}", signature, pass, state);
        match state {
            EgtbPresent => {
                eprintln!("{}  Pass {:2} - sorting moves", signature, pass);
                positions.clear();
                positions.shrink_to_fit();
                cache.clear();
                cache.shrink_to_fit();
                cache_max = 0;
                um_writer = cpos_append_writer("/dev/null")?;
                state = make_moves(signature, &um_path, &moves_path)?;
            }
            StartUp => {
                positions.clear();
                positions.shrink_to_fit();
                cache.clear();
                cache.shrink_to_fit();
                cache_max = 0;
                state = make_positions(signature, pass, &unsorted_path, &mut positions)?;
            }
            UnsortedVec => {
                eprint!(
                    "{}  Pass {:2} - sorting in-memory positions ... ",
                    signature, pass
                );
                positions.sort_unstable();
                eprintln!("done.");
                state = PrepareAnalyzeMemory;
            }
            PrepareAnalyzeMemory => {
                eprintln!(
                    "{}  Pass {:2} - preparing in-memory retrograde analysis",
                    signature, pass
                );
                cache_max = alloc_working_memory(&positions, &mut cache)?;
                um_writer = if restart {
                    cpos_append_writer(&um_path)
                } else {
                    cpos_create_writer(&um_path)
                }?;
                state = AnalyzeMemory(true);
            }
            AnalyzeMemory(mateonly) => {
                eprint!(
                    "{}  Pass {:2} - analyzing {} positions    0‰ ",
                    signature,
                    pass,
                    if mateonly { "mate" } else { "draw" }
                );
                state = analyze_in_memory(
                    signature,
                    mateonly,
                    &mut positions,
                    &mut cache,
                    cache_max,
                    &mut egtb_map,
                    &mut um_writer,
                )?;
                um_writer
                    .flush()
                    .map_err(|x| format!("couldn't flush buffer for {} ({})", um_path, x))?;
            }
            WriteEgtb => {
                eprint!("{}  Pass {:2} - writing {}   0% ", signature, pass, egtb_path);
                let mut writer = cpos_create_writer(&egtb_path)?;
                let max_items = positions.len();
                state = make_egtb(&mut positions.iter().copied(), max_items, &mut writer, &egtb_path)?;
            }
            EgtbFromSorted => {
                eprint!("{}  Pass {:2} - writing {}   0% ", signature, pass, egtb_path);
                let mut writer = cpos_create_writer(&egtb_path)?;
                let max_items = cpos_file_size(&sorted_path)?;
                let mut it = CPosReader::new(&sorted_path)?;
                state = make_egtb(&mut it, max_items, &mut writer, &egtb_path)?;
            }
            InterruptedMemory => {
                eprint!(
                    "{}  Pass {:2} - saving work in {}   0% ",
                    signature, pass, sorted_path
                );
                sorted_reader = cpos_open_reader("/dev/null")?;
                sorted_reader.rewind().unwrap_or_default();
                let mut writer = cpos_create_writer(&sorted_path)?;
                let max_items = positions.len();
                make_write(
                    SortedPresent,
                    &mut positions,
                    max_items,
                    &mut writer,
                    &sorted_path,
                )?;
                return Err("terminated by user".to_string());
            }
            InterruptedBuffer(n_pos) => {
                // write already processed positions to the pass file
                eprint!(
                    "{}  Pass {:2} - saving work in {}   0% ",
                    signature, pass, sorted_path
                );
                make_write(SortedPresent, &mut positions, n_pos, &mut pass_writer, &pass_path)?;
                return Err("terminated by user".to_string());
            }
            RestoreItems => {
                eprintln!("{}  Pass {:2} - restore in-memory positions", signature, pass);
                sorted_reader = cpos_open_reader(&sorted_path)?;
                cpos_file_size(&sorted_path).and_then(|n_pos| {
                    positions
                        .try_reserve_exact(n_pos)
                        .map_err(|e| {
                            format!(
                                "cannot reserve memory for {} positions ({})",
                                formatted_sz(n_pos),
                                e
                            )
                        })
                        .and_then(|_| {
                            read_a_chunk(&mut positions, &sorted_path, n_pos + 1, &mut sorted_reader)
                        })
                })?;
                state = PrepareAnalyzeMemory;
            }
            UnsortedPresent => {
                eprintln!("{}  Pass {:2} - sorting {}", signature, pass, unsorted_path);
                um_writer = cpos_append_writer("/dev/null")?;
                sorted_reader = cpos_open_reader("/dev/null")?;
                pass_writer = cpos_append_writer("/dev/null")?;
                state = sort_from_to(signature, &unsorted_path, &sorted_path).map(|_| SortedPresent)?;
                remove_file(&um_path).unwrap_or_default();
                remove_file(&pass_path).unwrap_or_default();
            }
            SortedPresent | McPresent => {
                eprintln!("{}  Pass {:2} - continuing with {}", signature, pass, sorted_path);
                let read_path: &str = if state == SortedPresent { &sorted_path } else { &mc_path };
                let n_items = cpos_file_size(&read_path)?;
                let in_memory = n_items < compute_vector_entries(128 * 1024 * 1024);
                restart = if in_memory {
                    Path::new(&um_path).is_file()
                } else {
                    Path::new(&um_path).is_file() && Path::new(&pass_path).is_file()
                };

                state = if restart && in_memory {
                    RestoreItems
                } else {
                    PrepareDiskAnalysis(state == SortedPresent)
                };
            }
            PrepareDiskAnalysis(mate) => {
                eprintln!(
                    "{}  Pass {:2} - preparing disk based retrograde analysis",
                    signature, pass
                );
                let read_path: &str = if mate { &sorted_path } else { &mc_path };
                positions.clear();
                positions.shrink_to_fit();
                cache.clear();
                cache.shrink_to_fit();
                let n_items = compute_vector_entries(256 * 1024 * 1024).min(256 * 1024 * 1024);
                positions.try_reserve_exact(n_items).map_err(|e| {
                    format!(
                        "can't allocate memory for {} positions ({})",
                        formatted_sz(n_items),
                        e
                    )
                })?;
                cache_max = alloc_working_memory(&positions, &mut cache)?;
                um_writer = if Path::new(&um_path).is_file() {
                    cpos_append_writer(&um_path)
                } else {
                    cpos_create_writer(&um_path)
                }?;
                pass_writer = if Path::new(&pass_path).is_file() {
                    cpos_append_writer(&pass_path)
                } else {
                    cpos_create_writer(&pass_path)
                }?;
                sorted_reader = cpos_open_reader(read_path)?;

                let p_items = cpos_file_size(&pass_path)?;
                last_disk_pass = p_items > 0;

                sorted_reader
                    .seek(SeekFrom::Start((p_items * SIZE_CPOS) as u64))
                    .map_err(|e| {
                        format!(
                            "cannot seek to {} on {} ({})",
                            formatted_sz(p_items * SIZE_CPOS),
                            read_path,
                            e
                        )
                    })?;

                state = ReadNextBuffer(mate);
            }
            ReadNextBuffer(mate) => {
                eprintln!("{}  Pass {:2} - reading buffer", signature, pass);
                let n_items = positions.len();
                positions.clear();
                cache.clear();
                let eof = read_a_chunk(&mut positions, &sorted_path, n_items, &mut sorted_reader)?;
                state = if eof && positions.len() == 0 {
                    // close sorted file & pass file
                    pass_writer = cpos_append_writer("/dev/null")?;
                    sorted_reader = cpos_open_reader("/dev/null")?;
                    let from_path: &str = if mate { &sorted_path } else { &mc_path };
                    let to_path: &str = if last_disk_pass {
                        if mate {
                            &mc_path // continue with draw analysis passes
                        } else {
                            &sorted_path // completely done
                        }
                    } else {
                        if mate {
                            &sorted_path // continue with another mate analysis pass
                        } else {
                            &mc_path // continue with another draw analysis pass
                        }
                    };
                    eprintln!("    rm {} && mv {} {} ", from_path, pass_path, to_path);
                    remove_file(&from_path)
                        .and_then(|_| rename(&pass_path, to_path))
                        .map_err(|e| {
                            format!("cannot rm {} && mv {} {} ({})", from_path, pass_path, to_path, e)
                        })?;
                    if last_disk_pass {
                        if mate {
                            PrepareDiskAnalysis(false)
                        } else {
                            EgtbFromSorted
                        }
                    } else {
                        PrepareDiskAnalysis(mate) // repeat the same
                    }
                } else {
                    AnalyzeBuffer(mate)
                }
            }
            AnalyzeBuffer(mateonly) => {
                let read_path: &str = if mateonly { &sorted_path } else { &mc_path };
                let m_items = positions.len();
                let pass_items = cpos_file_size(&pass_path)?;
                let sorted_items = cpos_file_size(read_path)?;
                eprintln!(
                    "{}  Pass {:2} - analyzing {} positions, buffer {} of {}    0‰ ",
                    signature,
                    pass,
                    if mateonly { "mate" } else { "draw" },
                    formatted_sz(pass_items / m_items + 1),
                    formatted_sz((sorted_items + (m_items - 1)) / m_items)
                );

                let mut pfile =
                    File::open(&pass_path).map_err(|e| format!("cannot open {} ({})", pass_path, e))?;
                let mut sfile =
                    File::open(read_path).map_err(|e| format!("cannot open {} ({})", read_path, e))?;
                let newstate = analyze_on_disk(
                    signature,
                    mateonly,
                    pass_items,
                    m_items,
                    sorted_items,
                    &mut positions,
                    &mut cache,
                    cache_max,
                    &mut egtb_map,
                    &mut um_writer,
                    &mut pfile,
                    &mut sfile,
                )?;
                last_disk_pass = last_disk_pass && state != newstate;
                um_writer
                    .flush()
                    .map_err(|x| format!("couldn't flush buffer for {} ({})", um_path, x))?;
                state = newstate;
            }
            BufferDone(mate) => {
                eprintln!("{}  Pass {:2} - saving buffer   0% ", signature, pass,);
                let m_items = positions.len();
                state = make_write(
                    ReadNextBuffer(mate),
                    &mut positions,
                    m_items,
                    &mut pass_writer,
                    &pass_path,
                )?;
            }
            ScanForMate(_) |  ScanUM(_) | // not here
            Done => {} // required for completeness
        }
    }
    for p in [&mc_path, &um_path, &unsorted_path, &sorted_path, &pass_path] {
        remove_file(p).unwrap_or_default();
    }
    eprintln!("{}  complete", signature);
    Ok(())
}

/// Write and flush positions to EGTB file,
/// filtering the positions that are not completely unknown and replacing UNKNOWN with CANNOT_AVOID_DRAW in
/// those positions where UNKNOWN appears for one player only.
///
/// Result is `EgtbPresent`
fn make_egtb(
    items: &mut impl Iterator<Item = CPos>,
    n_items: usize,
    writer: &mut BufWriter<File>,
    path: &str,
) -> Result<MakeState, String> {
    let mut npos = 0;
    let mut i = 0;
    for mut cpos in items {
        if i % 1_000_000 == 0 || i + 1 == n_items {
            eprint!("\x08\x08\x08\x08\x08{:3}% ", (i + 1) * 100 / n_items);
        }
        i += 1;
        // make states sane
        if cpos.state(WHITE) == UNKNOWN && cpos.state(BLACK) != UNKNOWN {
            cpos = cpos.with_state_for(WHITE, CANNOT_AVOID_DRAW);
        } else if cpos.state(WHITE) != UNKNOWN && cpos.state(BLACK) == UNKNOWN {
            cpos = cpos.with_state_for(BLACK, CANNOT_AVOID_DRAW);
        }
        let superfluous = cpos.state(WHITE) == UNKNOWN && cpos.state(BLACK) == UNKNOWN
            || cpos.state(WHITE) == CANNOT_AVOID_DRAW && cpos.state(BLACK) == CANNOT_AVOID_DRAW;
        // filter superfluous
        if !superfluous {
            cpos.write_seq(writer)
                .map_err(|ioe| format!("error writing {}th position to {} ({})", i + 1, path, ioe))?;
            npos += 1;
        }
    }
    writer
        .flush()
        .map_err(|x| format!("couldn't flush buffer for {} ({})", path, x))?;
    eprintln!("done, {} positions written.", formatted_sz(npos),);
    Ok(EgtbPresent)
}

/// Write and flush mmapped positions to EGTB file,
/// filtering the positions that are not completely unknown and replacing UNKNOWN with CANNOT_AVOID_DRAW in
/// those positions where UNKNOWN appears for one player only.
///
/// Result is `EgtbPresent`
fn fast_egtb(
    items: &[CPos],
    n_items: usize,
    writer: &mut BufWriter<File>,
    path: &str,
) -> Result<MakeState, String> {
    let mut npos = 0;
    // let mut i = 0;
    for i in 0..items.len() {
        let mut cpos = items[i];
        if i % 1_000_000 == 0 || i + 1 == n_items {
            eprint!("\x08\x08\x08\x08\x08{:3}% ", (i + 1) * 100 / n_items);
        }

        // make states sane
        if cpos.state(WHITE) == UNKNOWN && cpos.state(BLACK) != UNKNOWN {
            cpos = cpos.with_state_for(WHITE, CANNOT_AVOID_DRAW);
        } else if cpos.state(WHITE) != UNKNOWN && cpos.state(BLACK) == UNKNOWN {
            cpos = cpos.with_state_for(BLACK, CANNOT_AVOID_DRAW);
        }
        let superfluous = cpos.state(WHITE) == UNKNOWN && cpos.state(BLACK) == UNKNOWN
            || cpos.state(WHITE) == CANNOT_AVOID_DRAW && cpos.state(BLACK) == CANNOT_AVOID_DRAW;
        // filter superfluous
        if !superfluous {
            cpos.write_seq(writer)
                .map_err(|ioe| format!("error writing {}th position to {} ({})", i + 1, path, ioe))?;
            npos += 1;
        }
    }
    writer
        .flush()
        .map_err(|x| format!("couldn't flush buffer for {} ({})", path, x))?;
    eprintln!("done, {} positions written.", formatted_sz(npos),);
    Ok(EgtbPresent)
}

/// Write some items and flush the buffer.
///
/// Return will be `next_state` when successful.
fn make_write(
    next_state: MakeState,
    items: &[CPos],
    n_items: usize,
    writer: &mut BufWriter<File>,
    path: &str,
) -> Result<MakeState, String> {
    let mut npos = 0;
    for i in 0..n_items {
        let cpos = items[i];

        if i % 1_000_000 == 0 || i + 1 == n_items {
            eprint!("\x08\x08\x08\x08\x08{:3}% ", (i + 1) * 100 / n_items);
        }

        cpos.write_seq(writer)
            .map_err(|ioe| format!("error writing {}th position to {} ({})", i + 1, path, ioe))?;
        npos += 1;
    }
    writer
        .flush()
        .map_err(|x| format!("couldn't flush buffer for {} ({})", path, x))?;
    eprintln!("done, {} positions written.", formatted_sz(npos),);
    Ok(next_state)
}

static sigint_received: atomic::AtomicBool = atomic::AtomicBool::new(false);

/// One pass over one disk buffer.
///
/// Result can be `BufferDone(mateonly)`, `InterruptedBuffer(n)` or `AnalyzeBuffer(mateonly)`
fn analyze_on_disk(
    signature: Signature,
    mateonly: bool,
    pass_items: usize,
    m_items: usize,
    sorted_items: usize,
    positions: &mut Vec<CPos>,
    cache: &mut Cache,
    cache_max: usize,
    egtb_map: &mut EgtbMMap,
    um_writer: &mut BufWriter<File>,
    pfile: &mut File,
    sfile: &mut File,
) -> Result<MakeState, String> {
    let mut analyzed = vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    let mut cache_hits = 0usize;
    let mut cache_lookups = 0usize;
    let mut lookup_low = 0usize;
    let mut lookup_mem = 0usize;
    let mut lookup_high = 0usize;
    let mut lookup_alien = 0usize;
    let c_min = if m_items > 0 { positions[0] } else { CPos { bits: 0 } };
    let c_max = if m_items > 0 { positions[m_items - 1] } else { CPos { bits: !0 } };

    for i in 0..m_items {
        if (i % 1024) == 0 && sigint_received.load(atomic::Ordering::SeqCst) {
            eprintln!(" canceled.");
            return Ok(InterruptedBuffer(i));
        }
        if m_items > 0 && (i % (512 * 1024) == 0 || i + 1 == m_items) {
            eprint!("\x08\x08\x08\x08\x08\x08{:4}‰ ", (i + 1) * 1000 / m_items);
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
                let cache_len = cache.len();
                cache_lookups += 1;
                cache_hits += 1;
                let reached = cache.entry(key).or_insert_with(|| {
                    cache_hits -= 1;
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
                        let rp =
                            if child.state(other) == UNKNOWN {
                                if !alien {
                                    // look in pass file, positions or sorted file
                                    if canon < c_min {
                                        lookup_low += 1;
                                        binary_file_search(canon, pfile, 0, pass_items)
                                            .map(|p| {
                                                if canon.canonic_has_bw_switched() {
                                                    p.flipped_flags()
                                                } else {
                                                    p
                                                }
                                            })
                                            .map_err(|_| {
                                                format!(
                                                    "cannot happen alien={},\n    \
                                                    child={:?}  0x{:016x}\n    canon={:?}  0x{:016x}",
                                                    alien, child, child.bits, canon, canon.bits
                                                )
                                            })
                                    } else if canon > c_max {
                                        lookup_high += 1;
                                        binary_file_search(canon, sfile, pass_items + m_items, sorted_items)
                                            .map(|p| {
                                                if canon.canonic_has_bw_switched() {
                                                    p.flipped_flags()
                                                } else {
                                                    p
                                                }
                                            })
                                            .map_err(|_| {
                                                format!(
                                                    "cannot happen alien={},\n    \
                                                    child={:?}  0x{:016x}\n    canon={:?}  0x{:016x}",
                                                    alien, child, child.bits, canon, canon.bits
                                                )
                                            })
                                    } else
                                    /* canon >= c_min && canon <= c_max */
                                    {
                                        lookup_mem += 1;
                                        // binary search in memory
                                        match positions.binary_search(&canon) {
                                            Ok(u) => {
                                                if canon.canonic_has_bw_switched() {
                                                    Ok(positions[u].flipped_flags())
                                                } else {
                                                    Ok(positions[u])
                                                }
                                            }
                                            Err(_) => Err(format!(
                                                "cannot happen alien={},\n    \
                                                child={:?}  0x{:016x}\n    canon={:?}  0x{:016x}",
                                                alien, child, child.bits, canon, canon.bits
                                            )),
                                        }
                                    }
                                } else {
                                    lookup_alien += 1;
                                    // must be alien
                                    canon
                                        .find_canonic(csig, egtb_map)
                                        .map_err(|s| {
                                            format!(
                                                "could not find reached position because:\n\
                                                {}\nfen: {}  canonical: {}  hex: 0x{:016x}",
                                                s,
                                                encodeFEN(&reached[u].1.uncompressed(other)),
                                                encodeFEN(&canon.uncompressed(other)),
                                                canon.bits
                                            )
                                        })
                                        .map(|r| {
                                            if canon.canonic_has_bw_switched() {
                                                r.flipped_flags()
                                            } else {
                                                r
                                            }
                                        })
                                }
                            } else {
                                Ok(child)
                            }?;

                        let rpstate = rp.state(other);
                        if reached[u].1.state(other) != rpstate {
                            reached[u].1 = rp;
                        }
                        all_unknown = all_unknown && rpstate == UNKNOWN;
                        all_can_mate = all_can_mate && rpstate == CAN_MATE;
                        all_can_draw = all_can_draw && rpstate == CAN_DRAW;
                        match rpstate {
                            MATE | CANNOT_AVOID_MATE => {
                                if mateonly {
                                    let mv = reached[u].0;
                                    let mpos = c.mpos_from_mv(mv);
                                    analyzed[CAN_MATE as usize] += 1;
                                    c = c.with_state_for(player, CAN_MATE);

                                    all_can_mate = false;
                                    all_can_draw = false;

                                    // write the move to the `um` file
                                    mpos.write_seq(um_writer).map_err(|ioe| {
                                        format!(
                                            "unexpected error ({}) while writing to {}",
                                            ioe,
                                            mk_egtb_path(signature, "um")
                                        )
                                    })?;
                                    break 'children;
                                }
                            }
                            STALEMATE | CANNOT_AVOID_DRAW => {
                                if !mateonly {
                                    analyzed[CAN_DRAW as usize] += 1;
                                    c = c.with_state_for(player, CAN_DRAW);

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
                    || (all_unknown && cache_max < 2 * m_items)
                    || cache_len >= cache_max
                {
                    cache.remove(&key);
                }

                if c.state(player) != UNKNOWN {
                    positions[i] = c;
                }
            } // unknown state
        }
    }
    eprintln!("done.");
    eprintln!(
        "    Cache hit rate {}%, new hash size {}",
        if cache_lookups > 0 { cache_hits * 100 / cache_lookups } else { 100 },
        formatted_sz(cache.len())
    );
    let lookup_total = lookup_low + lookup_high + lookup_mem + lookup_alien;
    if lookup_total > 0 {
        eprintln!(
            "    Lookups low {}%  mem {}%  high {}%  alien {}%",
            lookup_low * 100 / lookup_total,
            lookup_mem * 100 / lookup_total,
            lookup_high * 100 / lookup_total,
            lookup_alien * 100 / lookup_total
        );
    }

    // are we done with this buffer?
    if analyzed.iter().fold(0, |acc, x| acc + x) == 0 {
        Ok(BufferDone(mateonly))
    } else {
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
        // need to re-iterate
        Ok(AnalyzeBuffer(mateonly))
    }
}

fn binary_file_search(this: CPos, file: &mut File, low: usize, high: usize) -> Result<CPos, String> {
    CIO::binary_file_search(this, file, low, high).map(|r| r.0)
}

/// One pass of in-memory analysis.
/// This is just a special case of `analyze_on_disk` with a single buffer.
///
/// Result can be `WriteEgtb`, `InterruptedMemory`, `AnalyzeMemory(true/false)`
fn analyze_in_memory(
    signature: Signature,
    mateonly: bool,
    positions: &mut Vec<CPos>,
    cache: &mut Cache,
    cache_max: usize,
    egtb_map: &mut EgtbMMap,
    um_writer: &mut BufWriter<File>,
) -> Result<MakeState, String> {
    let mut pfile = File::open("/dev/null").map_err(|e| format!("cannot read /dev/null ({})", e))?;
    let mut sfile = File::open("/dev/null").map_err(|e| format!("cannot read /dev/null ({})", e))?;
    analyze_on_disk(
        signature,
        mateonly,
        0,
        positions.len(),
        0,
        positions,
        cache,
        cache_max,
        egtb_map,
        um_writer,
        &mut pfile,
        &mut sfile,
    )
    .map(|s| match s {
        BufferDone(true) => AnalyzeMemory(false),
        BufferDone(false) => WriteEgtb,
        AnalyzeBuffer(m) => AnalyzeMemory(m),
        InterruptedBuffer(_) => InterruptedMemory,
        _other => {
            panic!("analyze in memory: cannot map state {:?}", s);
        }
    })
}

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

/// Generate positions, either to memory or unsorted file.
///
/// Result is `UnsortedVec` or `UnsortedPresent`
fn make_positions(
    signature: Signature,
    pass: usize,
    unsorted_path: &str,
    positions: &mut Vec<CPos>,
) -> Result<MakeState, String> {
    let vecmax = expected_positions(signature);
    eprint!(
        "{}  Pass {:2} - generate positions, up to {} are expected ",
        signature,
        pass,
        formatted_sz(vecmax)
    );

    let mut sink = match positions.try_reserve_exact(vecmax) {
        Ok(_) => {
            eprint!("in memory ... ");
            Sink::V(positions)
        }
        Err(_) => {
            eprint!("in {} ... ", unsorted_path);
            let bw = cpos_create_writer(unsorted_path)?;
            Sink::W(bw)
        }
    };

    let wKbits = if signature.white_pawns() > 0 || signature.black_pawns() > 0 {
        P::LEFT_HALF
    } else {
        P::LOWER_LEFT_QUARTER
    };
    let pps = signature.to_vec();
    for wk in wKbits {
        let pos1 = POSITION_NULL.place(WHITE, KING, P::bit(wk));
        // place the black king
        for bk in BitSet::all() {
            /*
            if signature.is_symmetric() && !signature.has_pawns() && bk < wk {
                continue;
            }
            */
            if pos1.isEmpty(bk) {
                let pos2 = pos1.place(BLACK, KING, P::bit(bk));
                if pos2.valid() {
                    complete(&pos2, 0, &pps, &mut sink)?;
                }
            }
        }
    }
    sink.flush()?;

    if let Sink::V(_) = sink {
        eprintln!("found {}", formatted_sz(positions.len()));
        if positions.len() > vecmax as usize {
            eprintln!("    WARNING: vecmax was calculated too low!");
        } else if positions.len() < vecmax as usize {
            positions.shrink_to_fit();
        }
        Ok(UnsortedVec)
    } else {
        let npos = cpos_file_size(unsorted_path)?;
        eprintln!("{} written to {}", formatted_sz(npos), unsorted_path);
        Ok(UnsortedPresent)
    }
}

/// Make positions on disk
///
/// Result is: UnsortedPresent
fn make_unsorted(signature: Signature, pass: usize, unsorted_path: &str) -> Result<MakeState, String> {
    let vecmax = expected_positions(signature);
    eprint!(
        "{}  Pass {:2} - generate positions, up to {} are expected ",
        signature,
        pass,
        formatted_sz(vecmax)
    );

    let mut sink = {
        eprint!("in {} ... ", unsorted_path);
        let bw = cpos_create_writer(unsorted_path)?;
        Sink::W(bw)
    };

    let wKbits = if signature.white_pawns() > 0 || signature.black_pawns() > 0 {
        P::LEFT_HALF
    } else {
        P::LOWER_LEFT_QUARTER
    };
    let pps = signature.to_vec();
    for wk in wKbits {
        let pos1 = POSITION_NULL.place(WHITE, KING, P::bit(wk));
        // place the black king
        for bk in BitSet::all() {
            /*
            if signature.is_symmetric() && !signature.has_pawns() && bk < wk {
                continue;
            }
            */
            if pos1.isEmpty(bk) {
                let pos2 = pos1.place(BLACK, KING, P::bit(bk));
                if pos2.valid() {
                    complete(&pos2, 0, &pps, &mut sink)?;
                }
            }
        }
    }
    sink.flush()?;
    std::mem::drop(sink);

    {
        let npos = cpos_file_size(unsorted_path)?;
        eprintln!("{} written.", formatted_sz(npos));
        Ok(UnsortedPresent)
    }
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

/// feed in an end game position and get back an appropriate move, provided the EGTB exists
pub fn findEndgameMove(pos: &Position) -> Result<Move, String> {
    let cpos = if pos.validEndgame() {
        Ok(pos.compressed())
    } else {
        Err(String::from("position is no valid end game."))
    }?;
    let mut ehash: EgtbMMap = HashMap::new();
    let mut mhash: EgtbMap = HashMap::new();
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
        UNKNOWN | INVALID_POS => Err(format!("illegal {:?} state for this position", s)),
        MATE => {
            eprintln!("# {:?} to play finds mate", pos.turn());
            Err(String::from("MATE"))
        }
        STALEMATE => {
            eprintln!("# {:?} to play finds stale mate", pos.turn());
            Err(String::from("STALEMATE"))
        }
        CAN_MATE => {
            let mpos = rpos.find_canonic_move(rpos.signature(), canonic_player, &mut mhash)?;
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
        CAN_DRAW => {
            match pos.moves().iter().copied().find(|&mv| {
                pos.apply(mv) // erreichte Position
                    .compressed() // komprimiert
                    .find(&mut ehash) // Ok(rp) oder Err()
                    .ok() // Some(rp) oder None
                    .map(|r| if r.canonic_has_bw_switched() { r.flipped_flags() } else { r })
                    .map(|r| match r.state(other) {
                        STALEMATE | CANNOT_AVOID_DRAW => true,
                        _other => false,
                    })
                    .unwrap_or(false)
            }) {
                Some(mv) => {
                    println!(
                        "# {:?} to move will enforce {} with {}",
                        pos.turn(),
                        if s == CAN_MATE { "mate" } else { "draw" },
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
                    .find(&mut ehash) // Ok(rp) oder Err()
                    .ok() // Some(rp) oder None
                    .map(|r| if r.canonic_has_bw_switched() { r.flipped_flags() } else { r })
                    .map(|r| match r.state(other) {
                        CAN_MATE => false,
                        STALEMATE => false,
                        _other => true,
                    })
                    .unwrap_or(false)
            }) {
                None => moves[0],
                Some(mv) => mv,
            };
            println!(
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

/// ### Check sanity of move positions for a Signature
/// - the moves file must exist
/// - the moves file must be sorted
/// - it must not contain duplicates
/// - for each move position, there must be an entry in the associated EGTB file and the status for the player must be CAN_MATE
/// - for each move position, the entry from the EGTB file must uncompress to a valid `Position`
/// - for each move position, the reconstructed `Move` must occur in the moves list of that `Position`
pub fn check_mmap(arg: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(arg)?;
    let path = mk_egtb_path(signature, "moves");
    let egtb = mk_egtb_path(signature, "egtb");
    // let f = cpos_rw_file(&egtb)?;
    // let mmap = unsafe { Mmap::map(&f) }.map_err(|e| format!("cannot mmap {} ({})", egtb, e))?;
    // let start = &mmap[0] as *const u8;
    let epositions = cpos_ro_map(&egtb)?;
    let mut last = CPos { bits: 0 };
    let mut sorted = true;
    let mut dupl = false;
    let mut unrelated = false;
    let mut invalid = false;
    let mut badmoves = 0usize;
    let rdr = CPosReader::new(&path)?;
    let n_pos = cpos_file_size(&path)?;
    let mut curr_pos = 0;
    eprint!(
        "{} checking {} move positions from {} ...   0% ",
        signature.display(),
        formatted_sz(n_pos),
        path
    );
    for mpos in rdr {
        curr_pos += 1;
        if curr_pos % 1_000_000 == 0 && n_pos > 0 {
            eprint!("\x08\x08\x08\x08\x08\x08{:>4}% ", curr_pos * 100 / n_pos);
        }
        sorted = sorted && last <= mpos;
        dupl = dupl || last == mpos;
        last = mpos;
        let canonic = CPos { bits: mpos.bits & CPos::COMP_BITS & !CPos::WHITE_BIT };
        let u = epositions
            .1
            .binary_search(&canonic)
            .map_err(|_| format!("cpos not found: {:?}", canonic))?;
        let cpos = epositions.1[u];
        let player = if (mpos.bits & CPos::WHITE_BIT) != 0 { WHITE } else { BLACK };
        unrelated = unrelated || cpos.state(player) != CAN_MATE;
        let pos = cpos.uncompressed(player);
        invalid = invalid || !pos.valid();
        if pos.valid() {
            let mvs = pos.moves();
            let xmv = cpos.mv_from_mpos(mpos);
            badmoves = match mvs.iter().copied().find(|x| *x == xmv) {
                Some(_mv) => badmoves,
                None => {
                    if badmoves == 0 {
                        eprintln!("\nmove {}  impossible in position {}", xmv, encodeFEN(&pos));
                        eprintln!("cpos 0x{:016x} {:?}", cpos.bits, cpos);
                        eprintln!("mpos 0x{:016x} {}", mpos.bits, mpos.mpos_debug());
                    }
                    badmoves + 1
                }
            };
        }
    }
    eprintln!("\x08\x08\x08\x08\x08\x08 done.");
    if !sorted {
        eprintln!("{} moves file {} is not sorted.", signature.display(), path);
    }
    if dupl {
        eprintln!(
            "{} moves file {} contains duplicate entries.",
            signature.display(),
            path
        );
    }
    if unrelated {
        eprintln!(
            "{} moves file {} has unrelated entries.",
            signature.display(),
            path
        );
    }
    if invalid {
        eprintln!(
            "{} moves file {} has entries that yield invalid positions.",
            signature.display(),
            path
        );
    }
    if badmoves > 0 {
        eprintln!(
            "{} moves file {} has {} bad/unknown moves.",
            signature.display(),
            path,
            badmoves
        );
    }

    std::mem::drop(epositions.0);

    if sorted && !dupl && !unrelated && !invalid && badmoves == 0 {
        Ok(())
    } else {
        Err(format!("moves file {} is corrupt", path))
    }
}

/// ### Check sanity of move positions for a Signature
/// - the moves file must exist
/// - the moves file must be sorted
/// - it must not contain duplicates
/// - for each move position, there must be an entry in the associated EGTB file and the status for the player must be CAN_MATE
/// - for each move position, the entry from the EGTB file must uncompress to a valid `Position`
/// - for each move position, the reconstructed `Move` must occur in the moves list of that `Position`
pub fn check_moves(arg: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(arg)?;
    let path = mk_egtb_path(signature, "moves");
    let mut hash: EgtbMMap = HashMap::new();
    let mut last = CPos { bits: 0 };
    let mut sorted = true;
    let mut dupl = false;
    let mut unrelated = false;
    let mut invalid = false;
    let mut badmoves = false;
    let rdr = CPosReader::new(&path)?;
    let n_pos = cpos_file_size(&path)?;
    let mut curr_pos = 0;
    eprint!(
        "{} checking {} move positions from {} ...   0% ",
        signature.display(),
        formatted_sz(n_pos),
        path
    );
    for mpos in rdr {
        curr_pos += 1;
        if curr_pos % 1_000_000 == 0 && n_pos > 0 {
            eprint!("\x08\x08\x08\x08\x08\x08{:>4}% ", curr_pos * 100 / n_pos);
        }
        sorted = sorted && last <= mpos;
        dupl = dupl || last == mpos;
        last = mpos;
        let cpos = CPos { bits: mpos.bits & CPos::COMP_BITS & !CPos::WHITE_BIT }
            .find_canonic(signature, &mut hash)?;
        let player = if (mpos.bits & CPos::WHITE_BIT) != 0 { WHITE } else { BLACK };
        unrelated = unrelated || cpos.state(player) != CAN_MATE;
        let pos = cpos.uncompressed(player);
        invalid = invalid || !pos.valid();
        if pos.valid() {
            let mvs = pos.moves();
            let xmv = cpos.mv_from_mpos(mpos);
            badmoves = badmoves
                || match mvs.iter().copied().find(|x| *x == xmv) {
                    Some(_mv) => false,
                    None => true,
                };
        }
    }
    eprintln!("\x08\x08\x08\x08\x08\x08 done.");
    if !sorted {
        eprintln!("{} moves file {} is not sorted.", signature.display(), path);
    }
    if dupl {
        eprintln!(
            "{} moves file {} contains duplicate entries.",
            signature.display(),
            path
        );
    }
    if unrelated {
        eprintln!(
            "{} moves file {} has unrelated entries.",
            signature.display(),
            path
        );
    }
    if invalid {
        eprintln!(
            "{} moves file {} has entries that yield invalid positions.",
            signature.display(),
            path
        );
    }
    if badmoves {
        eprintln!(
            "{} moves file {} has invalid/unknown moves.",
            signature.display(),
            path
        );
    }

    if sorted && !dupl && !unrelated && !invalid && !badmoves {
        Ok(())
    } else {
        Err(format!("moves file {} is corrupt", path))
    }
}

/// Provide statistics for an endgame tablebase and perform basic checks.
pub fn stats(sig: String) -> Result<(), String> {
    let dbfile = format!("{}/{}", env::var("EGTB").unwrap_or(String::from("./egtb")), sig);

    let egtbfile = format!(
        "{}/{}.egtb",
        env::var("EGTB").unwrap_or(String::from("egtb")),
        sig
    );
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
    let mv_path = path.replace(".egtb", ".moves");
    let nmoves = if mv_path == path {
        eprintln!("can't derive moves file");
        Ok(0)
    } else {
        cpos_file_size(&mv_path)
    }?;

    let mut wkinds = vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    let mut bkinds = vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    let c0 = CPos { bits: 0 };
    let mut wexamples = vec![
        c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0,
    ];
    let mut bexamples = vec![
        c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0, c0,
    ];
    let mut last = c0;
    let mut total = 0usize;
    let mut sorted = true;
    let mut duplic = false;
    let mut moves_wrong = false;
    let mut cad_both = 0usize;
    let mut cad_example = c0;
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

                if c.state(BLACK) == CANNOT_AVOID_DRAW && c.state(WHITE) == CANNOT_AVOID_DRAW {
                    cad_both += 1;
                    cad_example = c;
                }
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
    if cad_both > 0 {
        eprintln!(
            "{:>12} both  positions with status {:<20} for example {}",
            formatted_sz(cad_both),
            format!("{:?}", CANNOT_AVOID_DRAW),
            encodeFEN(&cad_example.uncompressed(BLACK))
        );
    }
    eprintln!("{:>12} positions total", formatted_sz(total));
    if wkinds[CAN_MATE as usize] + bkinds[CAN_MATE as usize] != nmoves as usize {
        eprintln!(
            "Warning: we have {} CAN_MATE positions but {} moves.",
            formatted_sz(wkinds[CAN_MATE as usize] + bkinds[CAN_MATE as usize]),
            formatted_sz(nmoves)
        );
        moves_wrong = true;
    }
    if wkinds[UNKNOWN as usize] + bkinds[UNKNOWN as usize] > 0 {
        eprintln!("Warning: the table contains positions with UNKNOWN state.");
    }
    if !sorted {
        eprintln!("Warning: the table is not sorted.");
    }
    if duplic {
        eprintln!("Warning: the table contains duplicates.");
    }
    if wkinds[UNKNOWN as usize] > 0 || bkinds[UNKNOWN as usize] > 0 || !sorted || duplic || moves_wrong {
        eprintln!("Warning: This is an invalid or yet incomplete end game table.");
    }

    Ok(())
}

/// Allocate the memory needed for in-memory processing but not more than twice the vector length,
/// since we need at most 2 hash entries per position.
pub fn alloc_working_memory(vec: &Vec<CPos>, hash: &mut Cache) -> Result<usize, String> {
    let hsize = compute_cache_entries(vec.len()).min(2 * vec.len());
    hash.try_reserve(hsize).map_err(|ioe| {
        format!(
            "cannot reserve space for {} hash entries ({}).",
            formatted_sz(hsize),
            ioe
        )
    })?;
    eprintln!("    reserved space for {} hash entries.", formatted_sz(hsize));
    Ok(hsize)
}

const DEBUG_POSITION: CPos = CPos { bits: 0x0048_d274_a630 };

/// mmap backed generation
pub fn gen(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let egtb_path = mk_egtb_path(signature, "egtb");
    let sorted_path = mk_egtb_path(signature, "sorted");
    let unsorted_path = mk_egtb_path(signature, "unsorted");
    let um_path = mk_temp_path(signature, "um");
    let restart_path = mk_egtb_path(signature, "restart");
    let moves_path = mk_egtb_path(signature, "moves");

    let mut pass = 0;
    let mut um_writer = cpos_append_writer("/dev/null")?;
    let (mut map, mut db) = cpos_anon_map(&1)?;
    let mut ehash: EgtbMMap = HashMap::new();

    // initial state depends on the files in the EGTB directory
    let mut state = if Path::new(&egtb_path).is_file() {
        if Path::new(&moves_path).is_file() {
            Done
        } else {
            EgtbPresent
        }
    } else if Path::new(&sorted_path).is_file() {
        SortedPresent
    } else if Path::new(&unsorted_path).is_file() {
        UnsortedPresent
    } else {
        StartUp
    };

    let handler_ref = Arc::new(&sigint_received);
    ctrlc::set_handler(move || {
        handler_ref.store(true, atomic::Ordering::SeqCst);
    })
    .map_err(|e| format!("Cannot set CTRL-C handler ({})", e))?;

    while state != Done && state != WriteEgtb {
        pass += 1;
        // eprintln!("{}  Pass {:2} {:?}", signature, pass, state);
        match state {
            EgtbPresent => {
                eprintln!("{}  Pass {:2} - sorting moves", signature, pass);
                std::mem::drop(um_writer);
                um_writer = cpos_append_writer("/dev/null")?;
                state = make_moves(signature, &um_path, &moves_path)?;
            }

            StartUp => {
                remove_file(&moves_path).unwrap_or_default();
                state = make_unsorted(signature, pass, &unsorted_path)?;
            }

            WriteEgtb => {
                eprint!("{}  Pass {:2} - writing {}   0% ", signature, pass, egtb_path);
                let mut writer = cpos_create_writer(&egtb_path)?;
                let max_items = db.len();
                state = fast_egtb(db, max_items, &mut writer, &egtb_path)?;
                std::mem::drop(map);
                let (xmap, xdb) = cpos_anon_map(&1)?;
                map = xmap;
                db = xdb;
            }

            UnsortedPresent => {
                eprintln!("{}  Pass {:2} - sorting {}", signature, pass, unsorted_path);
                remove_file(&moves_path).unwrap_or_default();
                um_writer = cpos_append_writer("/dev/null")?;
                state = sort_from_to(signature, &unsorted_path, &sorted_path).map(|_| SortedPresent)?;
                remove_file(&um_path).unwrap_or_default();
            }

            SortedPresent => {
                eprintln!("{}  Pass {:2} - continuing with {}", signature, pass, sorted_path);
                remove_file(&moves_path).unwrap_or_default();
                std::mem::drop(map);
                // from this point on, the map is connected to the sorted file
                // and um_writer writes to the um file
                {
                    let (xmap, xdb) = cpos_rw_map(&sorted_path)?;
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
                        ScanForMate(xpos.bits as usize)
                    } else if pat.bits as usize == RESTART_UM {
                        ScanUM(xpos.bits as usize)
                    } else {
                        return Err("there is garbage in the restart file".to_string());
                    }
                } else {
                    ScanForMate(0)
                };
            }

            ScanForMate(at) => {
                eprint!(
                    "{}  Pass {:2} - find mates, stalemates, direct winners & alien loosers    0‰ {} ",
                    signature,
                    pass,
                    fmt_human(db.len(), 0)
                );
                let result = scan_mates_aliens(signature, at, &mut um_writer, db, &mut ehash)?;
                um_writer
                    .flush()
                    .map_err(|e| format!("error while flushing {} ({})", um_path, e))?;
                eprint!("    setting checkpoint ");
                match result {
                    None => {
                        set_checkpoint(&restart_path, RESTART_UM, 0)?;
                        state = ScanUM(0);
                    }
                    Some(u) => {
                        eprint!(" for restart ");
                        set_checkpoint(&restart_path, RESTART_MATES, u)?;
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
                eprint!("    Flushing {} and {} ... ", sorted_path, um_path);
                map.flush()
                    .and_then(|_| um_writer.flush())
                    .map_err(|e| format!("Can't sync {} and {} ({})", sorted_path, um_path, e))?;
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

            AnalyzeBuffer(_)
            | McPresent
            | InterruptedBuffer(_)
            | InterruptedMemory
            | UnsortedVec
            | PrepareAnalyzeMemory
            | AnalyzeMemory(_)
            | RestoreItems
            | PrepareDiskAnalysis(_)
            | ReadNextBuffer(_)
            | BufferDone(_)
            | EgtbFromSorted => {
                Err(format!("must not happen, as {:?} is not used here", state))?;
            }
        }
    }

    std::mem::drop(um_writer);
    for p in [um_path, unsorted_path, sorted_path, restart_path] {
        remove_file(p).unwrap_or_default();
    }
    eprintln!("{}  complete", signature);
    Ok(())
}

const RESTART_MATES: usize = 0xBADDEED;
const RESTART_UM: usize = 0xDEADBEEF;

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

/// assert_eq!(signature, can.signature())
fn register_winner(
    signature: Signature,
    can: CPos,
    umv: Move,
    writer: &mut BufWriter<File>,
    db: &mut [CPos],
) -> Result<usize, String> {
    let canonic = can.canonical(signature);
    let v = db.binary_search(&canonic).map_err(|_| {
        eprintln!("\nwinning position not found");
        eprintln!("  missing pos {:?}", can);
        eprintln!("  canonic pos {:?}", canonic);
        eprintln!("  before move {}", umv);
        eprintln!("  reaching    {:?}", can.apply(umv));
        "NOT FOUND".to_string()
    })?;
    let c = db[v];
    let hmv = if canonic.canonic_was_mirrored_h() { umv.mirror_h() } else { umv };
    let mv = if canonic.canonic_was_mirrored_v() { hmv.mirror_v() } else { hmv };
    let mpos = c.mpos_from_mv(mv);
    if c.state(mv.player()) == UNKNOWN {
        if c == DEBUG_POSITION {
            eprint!("!register_new_winner!");
        }
        db[v] = c.with_state_for(mv.player(), CAN_MATE);
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
    at: usize,
    um_writer: &mut BufWriter<File>,
    db: &mut [CPos],
    hash: &mut EgtbMMap,
) -> Result<Option<usize>, String> {
    let mut wins = 0usize;
    let mut loos = 0usize;
    let mut mates = 0usize;
    let mut stalemates = 0usize;
    let n_items = db.len();

    for n in at..n_items {
        if n_items > 0 && (n % (128 * 1024) == 0 || (n + 1) == n_items) {
            if sigint_received.load(atomic::Ordering::SeqCst) {
                eprintln!("\x08\x08 canceled.");
                return Ok(Some(n));
            }
            eprint!(
                "\x08\x08\x08\x08\x08\x08\x08\x08\x08\x08\x08{:4}‰ {} ",
                (n + 1) * 1000 / n_items,
                fmt_human(n_items, n + 1)
            );
        }
        'next_player: for player in [BLACK, WHITE] {
            let cp = db[n];
            if cp == DEBUG_POSITION {
                eprint!("!scan_mates_aliens");
            }
            if cp.state(player) == UNKNOWN {
                let mut all_successors_can_mate = true;
                let mut no_moves = true;
                for (mv, succ) in cp
                    .move_iterator(player)
                    .map(|mv| (mv, cp.apply(mv)))
                    .filter(|(_, c)| c.valid(player.opponent()))
                {
                    no_moves = false;
                    // is this one of our positions?
                    // if so, let this position remain UNKNOWN
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
                        MATE | CANNOT_AVOID_MATE => {
                            let mpos = cp.mpos_from_mv(mv);
                            db[n] = cp.with_state_for(player, CAN_MATE);
                            mpos.write_seq(um_writer).map_err(|ioe| {
                                format!("unexpected error ({}) while writing to {}.um", ioe, signature)
                            })?;
                            wins += 1;
                            continue 'next_player;
                        }
                        CAN_MATE => continue, /* with next move */
                        STALEMATE | UNKNOWN | CANNOT_AVOID_DRAW | CAN_DRAW | INVALID_POS => {
                            all_successors_can_mate = false;
                        }
                    }
                }
                if no_moves && cp.state(player.opponent()) != INVALID_POS {
                    // it's STALEMATE
                    stalemates += 1;
                    db[n] = cp.with_state_for(player, STALEMATE);
                } else if all_successors_can_mate {
                    // || (no_moves && cp.state(player.opponent()) == INVALID_POS)
                    // it's not STALEMATE, but either MATE or all successors are alien and can mate!
                    if no_moves && cp.state(player.opponent()) == INVALID_POS {
                        mates += 1;
                        db[n] = cp.with_state_for(player, MATE);
                    } else {
                        loos += 1;
                        db[n] = cp.with_state_for(player, CANNOT_AVOID_MATE);
                    }
                    // find all moves from same signature (no captures, no promotions) that come here
                    // they all CAN_MATE
                    for (pred, mv) in cp
                        .reverse_move_iterator(player)
                        .filter(|mv| !mv.is_capture_by_pawn() && !mv.is_promotion())
                        .map(|mv| (cp.unapply(mv, EMPTY), mv))
                        .filter(|(c, _)| c.valid(player.opponent()) && c.signature() == signature)
                    {
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
        eprintln!("    Found {} new CAN_MATE positions.", formatted_sz(wins));
        eprintln!(
            "    Found {} new CANNOT_AVOID_MATE positions.",
            formatted_sz(loos)
        );
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
    db: &mut [CPos],
    e_hash: &mut EgtbMMap,
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
                if w_pos > 0 && (r_pos % (16 * 1024) == 0 || r_pos + 1 == w_pos) {
                    if sigint_received.load(atomic::Ordering::SeqCst) {
                        eprintln!("\x08\x08 canceled.");
                        return Ok(Some(r_pos));
                    }
                    eprint!(
                        "\x08\x08\x08\x08\x08\x08\x08\x08\x08\x08\x08{:4}‰ {} ",
                        (r_pos + 1) * 1000 / w_pos,
                        fmt_human(w_pos, r_pos + 1)
                    );
                }
                let cpos = mpos.mpos_to_cpos();
                if cpos == DEBUG_POSITION {
                    eprint!("!scan_um_winner!");
                }
                let player = mpos.mpos_player();
                // loop over potential loosers
                for (lpos, lmv) in cpos
                    .reverse_move_iterator(player)
                    .filter(|mv| !mv.is_capture_by_pawn() && !mv.is_promotion())
                    .map(|mv| (cpos.unapply(mv, EMPTY), mv))
                    .filter(|(c, _mv)| c.valid(player.opponent()) && c.signature() == signature)
                {
                    let can_looser = lpos.clear_trans().canonical(signature);
                    if can_looser == DEBUG_POSITION {
                        eprint!("!scan_um_looser!");
                    }
                    let u = db.binary_search(&can_looser).map_err(|_| {
                        eprintln!("\npotentially loosing cpos not found");
                        eprintln!("  missing pos {:?}", can_looser);
                        eprintln!("  non canonic {:?}", lpos);
                        eprintln!("  moving      {}", lmv);
                        eprintln!("  reaching    {:?}", cpos);
                        "NOT FOUND".to_string()
                    })?;
                    // is this position unknown yet?
                    if db[u].state(player.opponent()) == UNKNOWN {
                        // make it an CANNOT_AVOID_MATE if all moves reach CAN_MATE
                        let lpos = db[u];
                        let mut all_children_can_mate = true;
                        for (wpos, wmv) in lpos
                            .move_iterator(player.opponent())
                            .map(|mv| (lpos.apply(mv), mv))
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
                                let v = db.binary_search(&canon).map_err(|_| {
                                    eprintln!("\npotentially winning cpos not found");
                                    eprintln!("  missing pos {:?}", canon);
                                    eprintln!("  non canonic {:?}", wpos);
                                    eprintln!("  moving      {}", wmv);
                                    eprintln!("  reaching    {:?}", lpos);
                                    "NOT FOUND".to_string()
                                })?;
                                if canon.canonic_has_bw_switched() {
                                    db[v].flipped_flags()
                                } else {
                                    db[v]
                                }
                            };
                            all_children_can_mate = all_children_can_mate && rp.state(player) == CAN_MATE;
                            if !all_children_can_mate {
                                break;
                            } // first non can-mate is enough
                        }
                        if all_children_can_mate {
                            db[u] = lpos.with_state_for(player.opponent(), CANNOT_AVOID_MATE);
                            n_looser += 1;
                            // this may also give rise to new can mates
                            for (canm, umv) in lpos
                                .reverse_move_iterator(player.opponent())
                                .filter(|mv| !mv.is_capture_by_pawn() && !mv.is_promotion())
                                .map(|mv| (lpos.unapply(mv, EMPTY), mv))
                                .filter(|(c, _mv)| c.valid(player) && c.signature() == signature)
                            {
                                let canonic = canm.clear_trans().canonical(signature);
                                if canonic == DEBUG_POSITION {
                                    eprint!("!scan_um_new_winner!");
                                }
                                let v = db.binary_search(&canonic).map_err(|_| {
                                    eprintln!("\nwinning position not found");
                                    eprintln!("  missing pos {:?}", canonic);
                                    eprintln!("  before move {}", umv);
                                    eprintln!("  reaching    {:?}", db[u]);
                                    "NOT FOUND".to_string()
                                })?;
                                let c = db[v];
                                if c.state(umv.player()) == UNKNOWN {
                                    let hmv =
                                        if canonic.canonic_was_mirrored_h() { umv.mirror_h() } else { umv };
                                    let mv =
                                        if canonic.canonic_was_mirrored_v() { hmv.mirror_v() } else { hmv };
                                    let mpos = c.mpos_from_mv(mv);
                                    db[v] = c.with_state_for(mv.player(), CAN_MATE);
                                    mpos.write_seq(writer).map_err(|ioe| {
                                        format!("unexpected error ({}) while writing to {}", ioe, um_path)
                                    })?;
                                    n_winner += 1;
                                    // w_pos += 1;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    eprintln!("done.");
    eprintln!("    Found {:>12} new CAN_MATE positions.", formatted_sz(n_winner));
    eprintln!(
        "    Found {:>12} new CANNOT_AVOID_MATE positions.",
        formatted_sz(n_looser),
    );
    Ok(None)
}

enum Sink<'a> {
    W(BufWriter<File>),
    V(&'a mut Vec<CPos>),
}

impl Sink<'_> {
    pub fn push(&mut self, cpos: CPos) -> Result<(), String> {
        match self {
            Sink::W(bf) => cpos
                .write_seq(bf)
                .map_err(|ioe| format!("can't write sink buffer ({})", ioe)),
            Sink::V(vec) => {
                vec.push(cpos);
                Ok(())
            }
        }
    }
    pub fn flush(&mut self) -> Result<(), String> {
        match self {
            Sink::W(bf) => bf
                .flush()
                .map_err(|ioe| format!("can't flush sink buffer ({})", ioe)),
            Sink::V(_) => Ok(()),
        }
    }
}

fn complete(
    pos: &Position,
    index: usize,
    pps: &Vec<PlayerPiece>,
    positions: &mut Sink,
) -> Result<(), String> {
    if index >= pps.len() {
        let c = pos.compressed();
        if c.state(WHITE) != INVALID_POS || c.state(BLACK) != INVALID_POS {
            positions.push(c)?;
        }
        return Ok(());
    }
    let last = index + 1 == pps.len();
    let (pl, pc) = pps[index];
    let possible = if pc == PAWN { P::PAWN_FIELDS } else { BitSet::all() };

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
                    positions.push(c)?;
                }
                // for all pawns that could have moved a double step we need to make a
                // position where the player whose turn it is could
                // capture this pawn en passant.
                /*
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
                                positions.push(c)?;
                            }
                        }
                    }
                    if pc == BLACK && pf.rank() == 5 {
                        let sf = Field::from(pf as u8 + 8);
                        if ep.isEmpty(sf) {
                            let c = ep.setFlag(sf).compressed();
                            if c.state(WHITE) != INVALID_POS || c.state(BLACK) != INVALID_POS {
                                positions.push(c)?;
                            }
                        }
                    }
                } */
            } else {
                complete(&p, index + 1, pps, positions)?;
            }
        }
    }
    Ok(())
}

/// Estimate the number of positions we will find.
fn expected_positions(signature: Signature) -> usize {
    // positions of the two kings, less than 32*64 or 16*64
    let vecbase = if signature.white_pawns() > 0 || signature.black_pawns() > 0 {
        1806
    } else {
        903
    };
    // compute estimated number of moves, taking kind and number into account
    //
    // For example, assuming 2 queens would yield a factor of 64*64 would be a great overestimation
    // The computed number is in most cases a slight over-estimation because positions where both
    // kings are in check will not be considered in further computations.
    let vecmax = vecbase
        * over(48, signature.white_pawns() as u64)
        * over(
            48 - signature.white_pawns() as u64,
            signature.black_pawns() as u64,
        )
        * over(
            62 - signature.white_pawns() as u64 - signature.black_pawns() as u64,
            signature.white_knights() as u64,
        )
        * over(
            62 - signature.white_pawns() as u64
                - signature.black_pawns() as u64
                - signature.white_knights() as u64,
            signature.black_knights() as u64,
        )
        * over(
            62 - signature.white_pawns() as u64
                - signature.black_pawns() as u64
                - signature.white_knights() as u64
                - signature.black_knights() as u64,
            signature.white_bishops() as u64,
        )
        * over(
            62 - signature.white_pawns() as u64
                - signature.black_pawns() as u64
                - signature.white_knights() as u64
                - signature.black_knights() as u64
                - signature.white_bishops() as u64,
            signature.black_bishops() as u64,
        )
        * over(
            62 - signature.white_pawns() as u64
                - signature.black_pawns() as u64
                - signature.white_knights() as u64
                - signature.black_knights() as u64
                - signature.white_bishops() as u64
                - signature.black_bishops() as u64,
            signature.white_rooks() as u64,
        )
        * over(
            62 - signature.white_pawns() as u64
                - signature.black_pawns() as u64
                - signature.white_knights() as u64
                - signature.black_knights() as u64
                - signature.white_bishops() as u64
                - signature.black_bishops() as u64
                - signature.white_rooks() as u64,
            signature.black_rooks() as u64,
        )
        * over(
            62 - signature.white_pawns() as u64
                - signature.black_pawns() as u64
                - signature.white_knights() as u64
                - signature.black_knights() as u64
                - signature.white_bishops() as u64
                - signature.black_bishops() as u64
                - signature.white_rooks() as u64
                - signature.black_rooks() as u64,
            signature.white_queens() as u64,
        )
        * over(
            62 - signature.white_pawns() as u64
                - signature.black_pawns() as u64
                - signature.white_knights() as u64
                - signature.black_knights() as u64
                - signature.white_bishops() as u64
                - signature.black_bishops() as u64
                - signature.white_rooks() as u64
                - signature.black_rooks() as u64
                - signature.white_queens() as u64,
            signature.black_queens() as u64,
        );
    vecmax as usize
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

/// command line: rasch diff *sig* *path*
///
/// Makes a diff for *sig*.egtb in EGTB directory and the file given in *path*
///
pub fn egtb_diff(sig: &str, path2: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let path1 = mk_egtb_path(signature, "egtb");
    let (_map1, left) = cpos_ro_map(&path1)?;
    let (_map2, right) = cpos_ro_map(path2)?;
    let mut v = 0usize;
    let mut w = 0usize;
    let mut diffs = 0usize;
    let mut dlines = 0;
    let deplorable = |cp: CPos| {
        (cp.state(BLACK) == CANNOT_AVOID_DRAW || cp.state(BLACK) == CAN_DRAW)
            && (cp.state(WHITE) == CANNOT_AVOID_DRAW || cp.state(WHITE) == CAN_DRAW)
    };
    let fen = |c: CPos| encodeFEN(&c.uncompressed(if c.state(WHITE) == INVALID_POS { BLACK } else { WHITE }));

    println!("{} left: {}, right: {}", signature, left.len(), right.len());
    while v < left.len() && w < right.len() {
        if left[v] < right[w] && deplorable(left[v]) {
            v += 1;
        } else if left[v] > right[w] && deplorable(right[w]) {
            w += 1;
        } else if left[v] < right[w] {
            let miss0 = v;
            while v < left.len() && left[v] < right[w] {
                v += 1;
            }
            if dlines < 12 {
                println!(
                    "<  0x{:>016x}  {:?}  {}",
                    left[miss0].bits,
                    left[miss0],
                    fen(left[miss0])
                );
                dlines += 1;
                if miss0 + 1 < v - 1 {
                    println!("……… ({} further positions missing right)", (v - 1) - (miss0 + 1));
                    dlines += 1;
                }
                if miss0 + 1 < v {
                    println!(
                        "<  0x{:>016x}  {:?}  {}",
                        left[v - 1].bits,
                        left[v - 1],
                        fen(left[v - 1])
                    );
                    dlines += 1;
                }
            }
            diffs += v - miss0;
        } else if left[v] > right[w] {
            let miss0 = w;
            while w < right.len() && left[v] > right[w] {
                w += 1;
            }
            if dlines < 12 {
                println!(
                    ">  0x{:>016x}  {:?}  {}",
                    right[miss0].bits,
                    right[miss0],
                    fen(right[miss0]),
                );
                dlines += 1;
                if miss0 + 1 < w - 1 {
                    println!("……… ({} further positions missing left)", (w - 1) - (miss0 + 1));
                    dlines += 1;
                }
                if miss0 + 1 < w {
                    println!(
                        ">  0x{:>016x}  {:?}  {}",
                        right[w - 1].bits & CPos::COMP_BITS,
                        right[w - 1],
                        fen(right[w - 1])
                    );
                    dlines += 1;
                }
            }
            diffs += w - miss0;
        } else {
            let mut cp1 = left[v];
            let mut cp2 = right[w];
            for player in [BLACK, WHITE] {
                if cp1.state(player) == CAN_DRAW {
                    cp1 = cp1.with_state_for(player, CANNOT_AVOID_DRAW);
                }
                if cp2.state(player) == CAN_DRAW {
                    cp2 = cp2.with_state_for(player, CANNOT_AVOID_DRAW);
                }
            }
            if cp1.bits != cp2.bits {
                if dlines < 12 {
                    println!(
                        "≠  0x{:>016x}  {:?}  ≠≠≠  0x{:>02x}  {:?}/{:?}  {}",
                        cp1.bits,
                        cp1,
                        cp2.bits >> 56,
                        cp2.state(BLACK),
                        cp2.state(WHITE),
                        fen(cp2)
                    );
                    dlines += 1;
                }
                diffs += 1;
            }
            v += 1;
            w += 1;
        }
    }
    if v < left.len() {
        println!("{} additional positions left", left.len() - v);
        while v < left.len() {
            if !deplorable(left[v]) {
                diffs += 1;
            }
            v += 1;
        }
    }
    if w < right.len() {
        println!("{} additional positions right", right.len() - w);
        while w < right.len() {
            if !deplorable(right[w]) {
                diffs += 1;
            }
            w += 1;
        }
    }
    if diffs > 0 {
        Err(format!("{} has {} differences", signature, diffs))
    } else {
        Ok(())
    }
}

/// There is a lot of time used for binary searching.
/// The nature of the algorithm implies that (for mmap-ped arrys) many different pages might be read.
/// The idea is that we guess an index from the numerical value of the CPos that might at least hit the correct page
/// in one go.
/// (A page is 4096 bytes or 512 CPos)
/// If we can't establish that we did hit the correct page, we have at least an upper/lower bound.
/// We can then try to guess the complementary bound and fall back to binary search of a sub-slice.

/// The ratio between the difference of the numerical values of 2 CPos stored in a range and the number of actual items
/// in this range varies but is mostly something between 7000 and 13000
const ratio_low: usize = 7000;
const ratio_high: usize = 13000;

/// We fall back to binary search, hopefully with a sane range
pub fn faster_search(db: &[CPos], cp: &CPos) -> Result<usize, usize> {
    if db.len() == 0 {
        return Err(0);
    }
    let nmax = db.len();
    let usz = |cpos: CPos| (cpos.bits & CPos::COMP_BITS) as usize;
    let n = usz(*cp);
    let z = usz(db[0]);
    let r = n - z; // value relative to zero
    let g_low = nmax.min((r / ratio_high) & !0x1ff); // clearing the lowest 9 bits gives us an index at a page boundary
    let g_high = nmax.min((r / ratio_low) | 0x1ff); // the last index for some page

    let x = usz(db[g_low]);
    let y = usz(db[g_high - 1]);
    if x <= n && n < y {
        // hit
        db[g_low..g_high].binary_search(cp).map(|x| x + g_low)
    } else if y < n {
        // use g_high as a lower bound
        db[g_high..nmax].binary_search(cp).map(|x| x + g_high)
    } else if x < n {
        db[g_low..nmax].binary_search(cp).map(|x| x + g_low)
    } else {
        // use guess as upper bound
        db[0..g_low].binary_search(cp)
    }
}

pub fn test_fast(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let egtb = mk_egtb_path(signature, "egtb");
    let movs = mk_egtb_path(signature, "moves");
    let (_map1, db) = cpos_ro_map(&egtb)?;
    let (_map2, mdb) = cpos_ro_map(&movs)?;
    println!("searching {} positions", mdb.len());
    let mut r = 0;
    for n in 0..mdb.len() {
        r ^= faster_search(db, &mdb[n].mpos_to_cpos())
            .map_err(|_| format!("faster_search failed for {:#016x} {:?}", mdb[n].bits, mdb[n],))?;
    }
    println!("r={}", r);
    Ok(())
}

pub fn test_slow(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let egtb = mk_egtb_path(signature, "egtb");
    let movs = mk_egtb_path(signature, "moves");
    let (_map1, db) = cpos_ro_map(&egtb)?;
    let (_map2, mdb) = cpos_ro_map(&movs)?;
    println!("searching {} positions", mdb.len());
    let r = 0;
    for n in 0..mdb.len() {
        assert_eq!(
            db.binary_search(&mdb[n].mpos_to_cpos())
                .map_err(|_| format!("slow_search failed for {:#016x} {:?}", mdb[n].bits, mdb[n],))?,
            faster_search(db, &mdb[n].mpos_to_cpos())
                .map_err(|_| format!("slow_search failed for {:#016x} {:?}", mdb[n].bits, mdb[n],))?
        );
    }
    println!("r={}", r);
    Ok(())
}

/// command line: rasch statistics sig
pub fn statistics(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let path = mk_egtb_path(signature, "egtb");
    let (_map, db) = cpos_ro_map(&path)?;

    let items = db.len();

    if items < 2 {
        return Err("schlechter Scherz".to_string());
    }

    let pdiff = |db: &[CPos], low: usize, high: usize| {
        let items = high - low;
        let cpos0 = db[low].bits & CPos::COMP_BITS;
        let cposZ = db[high - 1].bits & CPos::COMP_BITS;
        let diff = cposZ - cpos0;
        println!(
            "{:12}-{:12} are {:12} items, from {:#013x}-{:#013x}, diff is {:12}, items/diff = {:.3}",
            low,
            high,
            items,
            cpos0,
            cposZ,
            diff,
            diff / items as u64
        );
    };

    const pagesize: usize = 1024 * 1024;
    let mut page = 0;
    while page < items {
        pdiff(db, page, items.min(page + pagesize));
        page += pagesize;
    }
    pdiff(db, 0, items);
    Ok(())
}
