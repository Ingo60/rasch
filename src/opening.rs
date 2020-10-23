//! Read and setup the opening data base.
#![allow(non_snake_case)]

use super::position as P;
use super::position::{Move, Position};

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
// use std::str::SplitWhitespace;

pub type OpeningMap = HashMap<Position, Vec<Recommendation>>;

#[derive(Clone, Copy, Debug)]
pub struct Recommendation {
    pub mv:     Move,
    /// 0 means forbidden, n means n-1 exclamation marks
    pub ntimes: usize,
    /// line number
    pub lineNo: u32,
}

pub fn setupOpening() -> OpeningMap {
    let mut hash: OpeningMap = HashMap::new();
    let openResult = File::open("data/openings");
    match openResult {
        Err(x) => {
            eprintln!("couldn't open data/openings because {}", x);
            return hash;
        }
        Ok(fin) => {
            let mut lineNo = 0;
            let pos = P::initialBoard();
            for ok in BufReader::new(fin).lines() {
                match ok {
                    Err(x) => {
                        eprintln!("error {} while trying to read line {}", x, lineNo + 1);
                        return hash;
                    }
                    Ok(untrimmed) => {
                        lineNo += 1;
                        let line = untrimmed.trim();
                        if line.len() == 0 || line.starts_with('#') {
                            continue;
                        }
                        // ok, so we do have an opening line, finally
                        let mut x = line.split_whitespace();
                        process(lineNo, &mut hash, pos, &mut x);
                    }
                }
            }
        }
    }
    hash
}

fn process(lineNo: u32, hash: &mut OpeningMap, pos: Position, elements: &mut dyn Iterator<Item = &str>) {
    match elements.next() {
        None => return,
        Some(group) => {
            let rest: Vec<_> = elements.collect();
            for alg in group.split(',') {
                let mut ntimes = 0;
                let raw = if alg.ends_with('?') {
                    alg.trim_end_matches('?')
                } else if alg.ends_with('!') {
                    let r = alg.trim_end_matches('!');
                    ntimes = 1 + alg.len() - r.len();
                    r
                } else {
                    ntimes = 1;
                    alg
                };
                // now check if we have a good move
                match Move::unAlgebraic(&pos.moves(), raw) {
                    Err(_) => eprintln!("illegal move {} in line {} of openings file", alg, lineNo),
                    Ok(mv) => {
                        // see what we have, insert or update
                        match hash.get(&pos) {
                            None => {
                                let it = vec![Recommendation { mv, ntimes, lineNo }];
                                hash.insert(pos, it);
                            }
                            Some(moves) => {
                                let same: Vec<_> = moves.iter().filter(|x| x.mv == mv).collect();
                                let mut other: Vec<_> = moves.iter().filter(|x| x.mv != mv).copied().collect();
                                if same.len() != 0 && (same[0].ntimes == 0 || same[0].ntimes > ntimes) {
                                    // keep it but print error if contradiction
                                    if same.len() != 0 && same[0].ntimes == 0 && ntimes != 0 {
                                        eprintln!(
                                            "error in line {}, move {} cannot be recommended, it was forbidden \
                                             earlier (line {})",
                                            lineNo, raw, same[0].lineNo
                                        );
                                    };
                                    if same.len() != 0 && same[0].ntimes > 0 && ntimes == 0 {
                                        eprintln!(
                                            "error in line {}, forbidding move {} contradicts earlier opening \
                                             variation where it was allowed (line {})",
                                            lineNo, raw, same[0].lineNo
                                        );
                                    }
                                } else {
                                    other.push(Recommendation { lineNo, ntimes, mv });
                                    hash.insert(pos, other);
                                }
                            }
                        }
                        let pos2 = pos.apply(mv);
                        let rest = rest.clone();
                        process(lineNo, hash, pos2, &mut rest.into_iter());
                    }
                };
            }
        }
    }
}
