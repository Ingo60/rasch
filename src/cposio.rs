use std::io::ErrorKind::UnexpectedEof;
use std::{
    env,
    fs::File,
    io::{BufReader, BufWriter, Write},
};

use crate::cpos::{CPos, Signature};
use crate::util::*;

/// An iterator over elements of some type `T: Ord` that merges two of the same kind.
pub struct MergeIterator<T>
where
    T: Copy + Ord,
{
    one: Box<dyn Iterator<Item = T>>,
    two: Box<dyn Iterator<Item = T>>,
    a: Option<T>,
    b: Option<T>,
}

impl<T> MergeIterator<T>
where
    T: Copy + Ord,
{
    pub fn new(i1: Box<dyn Iterator<Item = T>>, i2: Box<dyn Iterator<Item = T>>) -> MergeIterator<T> {
        let mut m = MergeIterator { one: i1, two: i2, a: None, b: None };
        m.a = m.one.next();
        m.b = m.two.next();
        m
    }
}

impl<T> Iterator for MergeIterator<T>
where
    T: Copy + Ord,
{
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match self.a {
            None => match self.b {
                None => None,
                r @ Some(_) => {
                    self.b = self.two.next();
                    r
                }
            },
            r1 @ Some(v1) => match self.b {
                None => {
                    self.a = self.one.next();
                    r1
                }
                r2 @ Some(v2) => {
                    if v1 <= v2 {
                        self.a = self.one.next();
                        r1
                    } else {
                        self.b = self.two.next();
                        r2
                    }
                }
            },
        }
    }
}

/// Wrap a `BufReader` and make it an `Iterator<Item=CPos>`
/// Can be used to provide input for `MergeIterator<CPos>`
pub struct CPosReader {
    rdr: BufReader<File>,
}

impl CPosReader {
    pub fn new(path: &str) -> Result<CPosReader, String> {
        let file = File::open(path).map_err(|ioe| format!("Can't open file {} ({})", path, ioe))?;
        let bufr = BufReader::with_capacity(BUFSZ, file);
        Ok(CPosReader { rdr: bufr })
    }
}

impl Iterator for CPosReader {
    type Item = CPos;
    fn next(&mut self) -> Option<Self::Item> {
        match CPos::read_seq_with_eof(&mut self.rdr) {
            Ok(x) => x,
            Err(e) => {
                panic!("{} while reading from CPosReader", e)
            }
        }
    }
}

pub struct CPosVector {
    vec: Box<Vec<CPos>>,
    index: usize,
}

impl CPosVector {
    pub fn new(vec: Box<Vec<CPos>>) -> Self {
        Self { vec, index: 0 }
    }
}

impl Iterator for CPosVector {
    type Item = CPos;
    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.vec.len() {
            self.index += 1;
            Some(self.vec[self.index - 1])
        } else {
            None
        }
    }
}

pub type CPosIterator = Box<dyn Iterator<Item = CPos>>;

/// flush the contents of an `CPos` yielding Iterator to the disk
pub fn to_disk(path: &str, iter: &mut dyn Iterator<Item = CPos>) -> Result<usize, String> {
    let file = File::create(path).map_err(|ioe| format!("Can't create file {} ({})", path, ioe))?;
    let mut bufw = BufWriter::with_capacity(BUFSZ + BUFSZ / 2, file);
    eprint!("    writing to {} ... ", path);
    let mut written = 0;
    while match iter.next() {
        None => false,
        Some(cp) => {
            cp.write_seq(&mut bufw)
                .map_err(|ioe| format!("unexpected error ({}) while writing to {}", ioe, path))?;
            true
        }
    } {
        written += 1;
    }
    bufw.flush().map_err(|ioe| {
        format!(
            "unexpected error ({}) while flushing buffer for {}, file may be corrupt",
            ioe, path
        )
    })?;
    eprintln!("done, {} positions written to {}", formatted_sz(written), path);
    Ok(written)
}

/// read a number of CPos from a buffered reader
pub fn read_a_chunk(
    pos: &mut Vec<CPos>,
    from_path: &str,
    n_pos: usize,
    bufr: &mut BufReader<File>,
) -> Result<bool, String> {
    pos.clear();
    eprintln!("    reading from {} ...", from_path);
    while pos.len() < n_pos {
        match CPos::read_seq(bufr) {
            Ok(it) => {
                pos.push(it);
            }
            Err(some) if some.kind() == UnexpectedEof => {
                eprintln!(
                    "    ... reading finally done, got {} positions from {}.",
                    formatted_sz(pos.len()),
                    from_path
                );
                return Ok(true);
            }
            Err(other) => {
                return Err(format!(
                    "unexpected read error after {} positions ({})",
                    pos.len(),
                    other
                ))
            }
        }
    }
    eprintln!(
        "    ... reading done, got {} positions from {}",
        formatted_sz(pos.len()),
        from_path
    );
    Ok(false)
}

/// Make a path name from a signature and an extension.
pub fn mk_egtb_path(s: Signature, ext: &str) -> String {
    format!(
        "{}/{}.{}",
        env::var("EGTB").unwrap_or(String::from("./egtb")),
        s.display(),
        ext
    )
}
