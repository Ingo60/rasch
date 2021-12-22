#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

// use super::fen::{decodeFEN, encodeFEN};
// use super::fieldset::*;
// use super::position as P;
use super::position::CPos;
// use super::position::CPosState;
// use super::position::CPosState::*;
// use super::position::Move;
// use super::position::Piece;
// use super::position::Piece::*;
// use super::position::Player;
// use super::position::Player::*;
// use super::position::Position;
// use crate::position::Mirrorable;

// use std::collections::HashMap;
use std::env;
use std::fs::{remove_file, rename, File};
use std::io;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::ErrorKind::*;
use std::io::Write;
use std::path::Path;

/// We try to use 8 GB for in-place sort.
/// Why 8 GB? Because my computer has only 16GB memory and I want to do some other things while the sort runs for hours.
pub const CHUNK: usize = 1024 * 1024 * 1024;

/// Use a higher buffer size for our files
pub const BUFSZ: usize = 16 * 1024 * 1024;

/// Sort a file with compressed positions
///
/// Usage: sort("KP-KP") sorts `$EGTB/KP-KP.unsorted` into `$EGTB/KP-KP.sorted`
/// if this doesn't exist. After succesfull sort, the input file will be remove.
///
/// This is basically a merge sort, so it needs at least 3 times as much free space on the disk as the unsorted file uses.
///
/// Can be run independently with `rasch sort sig'
pub fn sort(sig: &str) -> Result<(), String> {
    let s_path = format!("{}/{}.sorted", env::var("EGTB").unwrap_or(String::from("egtb")), sig);
    let u_path = format!("{}/{}.unsorted", env::var("EGTB").unwrap_or(String::from("egtb")), sig);
    let c_path = format!("{}/{}.chunk", env::var("EGTB").unwrap_or(String::from("egtb")), sig);
    let path = Path::new(&s_path);
    if path.is_file() {
        return Err(format!("Destination file {} already exists.", &path.display()));
    }
    split(&u_path, &c_path).and_then(|v| {
        let mut x = v;
        merge(&mut x, &c_path, &s_path)
    })
    // Ok(())
}

/// Split an unsorted file into sorted chunks.
/// Returns a vector of path names of the generated chunks.
pub fn split(unsorted: &str, c_path: &str) -> Result<Vec<String>, String> {
    let file = File::open(unsorted).map_err(|ioe| format!("Can't read source file {} ({})", unsorted, ioe))?;
    let mut bufr = BufReader::with_capacity(BUFSZ, file);
    let mut positions: Vec<CPos> = Vec::with_capacity(0);
    let mut n_pos = CHUNK;
    let mut chunks: Vec<String> = Vec::new();

    // reserve some space for the actual sort
    positions
        .try_reserve_exact(n_pos)
        .or_else(|_| {
            n_pos /= 2;
            positions.try_reserve_exact(n_pos)
        })
        .or_else(|_| {
            {
                n_pos /= 2;
                positions.try_reserve_exact(n_pos)
            }
            .map_err(|e| {
                format!(
                    "Cannot even reserve sort space for {}m elements ({}). Giving up.",
                    n_pos / (1024 * 1024),
                    e
                )
            })
        })?;

    // make 1 chunk per loop
    let mut eof = false;
    while !eof {
        positions.clear();
        print!("reading from {} ...   0% ", unsorted);
        io::stdout().flush().unwrap_or_default();
        while positions.len() < n_pos {
            // be verbose
            if positions.len() % (n_pos / 100) == 0 || positions.len() + 1 == n_pos {
                print!("\x08\x08\x08\x08\x08{:3}% ", (positions.len() + 1) * 100 / n_pos);
                io::stdout().flush().unwrap_or_default();
            }
            match CPos::read_seq(&mut bufr) {
                Ok(it) => {
                    positions.push(it);
                }
                Err(some) if some.kind() == UnexpectedEof => {
                    eof = true;
                    break;
                }
                Err(other) => {
                    return Err(format!(
                        "unexpected read error after {} positions ({})",
                        positions.len(),
                        other
                    ))
                }
            }
        }
        println!("done, got {} positions.", positions.len());

        if positions.len() > 0 {
            print!("sorting ... ");
            io::stdout().flush().unwrap_or_default();
            positions.sort_unstable();
            println!("done.");
            let name = format!("{}{}", c_path, chunks.len());
            let chunk = File::create(&name).map_err(|ioe| format!("Can't create chunk file {} ({})", name, ioe))?;
            let mut bufw = BufWriter::with_capacity(BUFSZ, chunk);
            print!("writing sorted {}   0% ", &name);
            io::stdout().flush().unwrap_or_default();
            for i in 0..positions.len() {
                let cpos = positions[i];
                if i % (positions.len() / 100) == 0 || i + 1 == positions.len() {
                    print!("\x08\x08\x08\x08\x08{:3}% ", (i + 1) * 100 / positions.len());
                    io::stdout().flush().unwrap_or_default();
                }
                cpos.write_seq(&mut bufw)
                    .map_err(|ioe| format!("error writing {}th position to {} ({})", i + 1, name, ioe))?;
            }
            bufw.flush().map_err(|x| format!("couldn't flush buffer ({})", x))?;
            chunks.push(name);
            println!("done.");
        }
    }
    Ok(chunks)
}

/// Merges the sorted files whose names are in the `chunks` vector.
/// It'll name its intermediate files like `c_path`{n} where n is a number
/// that is at least the number of elements in the vector. That leaves the
/// names `c_path0` .. `c_path`{chunks.len()-1} to the creator of the vector.
///
/// It is an error if there are no chunk files named in `chunks`.
///
/// It is an error if any of the files named in `chunks` does not exist.
///
/// If there is just 1 name present, then the corresponding file is renamed to the name given in `s_path`.
/// If there are two names present, the 2 files will be merged into `s_path`.
/// And if there are even more, intermediate files will be created and merged finally into `s_path`.
pub fn merge(chunks: &mut Vec<String>, c_path: &str, s_path: &str) -> Result<(), String> {
    let mut suffix = chunks.len();
    loop {
        if chunks.len() == 0 {
            break Err(String::from("Can't merge 0 chunks, stupid!"));
        } else if chunks.len() == 1 {
            let name = chunks.remove(0);
            break rename(&name, s_path).map_err(|ioe| format!("Can't rename {} → {} ({})", name, s_path, ioe));
        } else {
            let name1 = chunks.remove(0);
            let name2 = chunks.remove(0);
            let name3 = format!("{}{}", c_path, suffix);
            suffix += 1;
            print!("merging {} and {} into {} ... - ", name1, name2, name2);
            io::stdout().flush().unwrap_or_default();
            let file1 = File::open(&name1).map_err(|ioe| format!("Can't read chunk file {} ({})", name1, ioe))?;
            let mut bufr1 = BufReader::with_capacity(BUFSZ, file1);
            let file2 = File::open(&name2).map_err(|ioe| format!("Can't read chunk file {} ({})", name2, ioe))?;
            let mut bufr2 = BufReader::with_capacity(BUFSZ, file2);
            let chunk = File::create(&name3).map_err(|ioe| format!("Can't create chunk file {} ({})", name3, ioe))?;
            let mut bufw = BufWriter::with_capacity(2 * BUFSZ, chunk);
            let mut p1 = CPos::read_seq_with_eof(&mut bufr1)
                .map_err(|ioe| format!("unexpected read error on {} ({})", name1, ioe))?;
            let mut p2 = CPos::read_seq_with_eof(&mut bufr2)
                .map_err(|ioe| format!("unexpected read error on {} ({})", name2, ioe))?;
            let mut written = 0;
            while p1.is_some() || p2.is_some() {
                written += 1;
                if written % 3_000_000 == 0 {
                    print!("\x08\x08/ ");
                    io::stdout().flush().unwrap_or_default();
                } else if written % 2_000_000 == 0 {
                    print!("\x08\x08- ");
                    io::stdout().flush().unwrap_or_default();
                } else if written % 1_000_000 == 0 {
                    print!("\x08\x08\\ ");
                    io::stdout().flush().unwrap_or_default();
                }
                match p1 {
                    Some(c1) => match p2 {
                        Some(c2) => {
                            if c1 <= c2 {
                                c1.write_seq(&mut bufw)
                                    .map_err(|ioe| format!("unexpected write error on {} ({})", name3, ioe))?;
                                p1 = CPos::read_seq_with_eof(&mut bufr1)
                                    .map_err(|ioe| format!("unexpected read error on {} ({})", name1, ioe))?;
                            } else {
                                c2.write_seq(&mut bufw)
                                    .map_err(|ioe| format!("unexpected write error on {} ({})", name3, ioe))?;
                                p2 = CPos::read_seq_with_eof(&mut bufr2)
                                    .map_err(|ioe| format!("unexpected read error on {} ({})", name2, ioe))?;
                            }
                        }
                        None => {
                            c1.write_seq(&mut bufw)
                                .map_err(|ioe| format!("unexpected write error on {} ({})", name3, ioe))?;
                            p1 = CPos::read_seq_with_eof(&mut bufr1)
                                .map_err(|ioe| format!("unexpected read error on {} ({})", name1, ioe))?;
                        }
                    },
                    None => match p2 {
                        Some(c2) => {
                            c2.write_seq(&mut bufw)
                                .map_err(|ioe| format!("unexpected write error on {} ({})", name3, ioe))?;
                            p2 = CPos::read_seq_with_eof(&mut bufr2)
                                .map_err(|ioe| format!("unexpected read error on {} ({})", name2, ioe))?;
                        }
                        None => { /* cannot happen */ }
                    },
                }
            }
            bufw.flush()
                .map_err(|x| format!("Can't flush buffer, output file may be corrupt ({})", x))?;
            chunks.push(name3);
            remove_file(&name1).map_err(|ioe| format!("Can't remove {} ({})", name1, ioe))?;
            remove_file(&name2).map_err(|ioe| format!("Can't remove {} ({})", name2, ioe))?;
            println!("\x08\x08done.");
        }
    }
}
