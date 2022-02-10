use std::{
    env,
    fs::{metadata, File, OpenOptions},
    io::{BufReader, BufWriter, ErrorKind::UnexpectedEof, SeekFrom, Write},
    path::Path,
    slice,
};

use memmap2::{Mmap, MmapMut};

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
pub fn to_disk<T, F>(path: &str, iter: &mut dyn Iterator<Item = T>, to_bytes: F) -> Result<usize, String>
where
    T: Sized + Copy,
    F: Fn(T) -> [u8; 8],
{
    let file = File::create(path).map_err(|ioe| format!("Can't create file {} ({})", path, ioe))?;
    let mut bufw = BufWriter::with_capacity(BUFSZ + BUFSZ / 2, file);
    eprint!("    writing to {} ... ", path);
    let mut written = 0;
    while match iter.next() {
        None => false,
        Some(cp) => {
            let bytes = to_bytes(cp);
            // cp.write_seq(&mut bufw)
            bufw.write_all(&bytes)
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
    eprintln!("done, {} items written.", formatted_sz(written));
    Ok(written)
}

/// Read a number of CPos from a buffered reader.
///
/// Return **true** if end-of-file was reached.
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

/// Make a path name from a signature and an extension.
pub fn mk_temp_path(s: Signature, ext: &str) -> String {
    format!(
        "{}/{}.{}",
        env::var("EGTBTEMP").unwrap_or(String::from("./egtb")),
        s.display(),
        ext
    )
}

/// Get the number of positions in a CPos file
pub fn cpos_file_size(path: &str) -> Result<usize, String> {
    let m = metadata(Path::new(path)).map_err(|e| format!("can't stat {} ({})", path, e))?;
    let u = m.len() / 8;
    if m.len() & 7 != 0 {
        Err(format!(
            "length of {} not divisible by size of a CPos ({})",
            path, SIZE_CPOS
        ))
    } else {
        Ok(u as usize)
    }
}

pub fn cpos_create_writer(path: &str) -> Result<BufWriter<File>, String> {
    File::create(path)
        .and_then(|f| Ok(BufWriter::with_capacity(BUFSZ, f)))
        .map_err(|e| format!("Can't create {} ({})", path, e))
}

pub fn cpos_append_writer(path: &str) -> Result<BufWriter<File>, String> {
    OpenOptions::new()
        .append(true)
        .open(path)
        .and_then(|f| Ok(BufWriter::with_capacity(BUFSZ, f)))
        .map_err(|e| format!("Can't create {} ({})", path, e))
}

pub fn cpos_open_reader(path: &str) -> Result<BufReader<File>, String> {
    File::open(path)
        .and_then(|f| Ok(BufReader::with_capacity(BUFSZ, f)))
        .map_err(|e| format!("Can't create {} ({})", path, e))
}

pub fn cpos_rw_file(path: &str) -> Result<File, String> {
    OpenOptions::new()
        .read(true)
        .write(true)
        .open(path)
        .map_err(|e| format!("Can't read/write {} ({})", path, e))
}

/// Map the given path as read-only CPos slice into memory.
///
/// **Note**: when the returned map goes out of scope, the slice will become unusable!
pub fn cpos_ro_map<'a>(path: &str) -> Result<(Mmap, &'a [CPos]), String> {
    let file = OpenOptions::new()
        .read(true)
        .open(path)
        .map_err(|e| format!("Can't read {} ({})", path, e))?;
    let map = unsafe { Mmap::map(&file) }.map_err(|e| format!("Can't mmap {} ({})", path, e))?;
    // let start = &map[0] as *const u8;
    // let s = map.as_ptr();
    let array = unsafe { slice::from_raw_parts(map.as_ptr().cast::<CPos>(), map.len() / 8) };
    Ok((map, array))
}

/// Map the given path as read-only byte slice into memory.
///
/// **Note**: when the returned map goes out of scope, the slice will become unusable!
pub fn byte_ro_map<'a>(path: &str) -> Result<(Mmap, &'a [u8]), String> {
    let file = OpenOptions::new()
        .read(true)
        .open(path)
        .map_err(|e| format!("Can't read {} ({})", path, e))?;
    let map = unsafe { Mmap::map(&file) }.map_err(|e| format!("Can't mmap {} ({})", path, e))?;
    let array = unsafe { slice::from_raw_parts(map.as_ptr(), map.len()) };
    Ok((map, array))
}

/// Map the given path as read-only unsigned short slice into memory
pub fn short_ro_map<'a>(path: &str) -> Result<(Mmap, &'a [u16]), String> {
    let file = OpenOptions::new()
        .read(true)
        .open(path)
        .map_err(|e| format!("Can't read {} ({})", path, e))?;
    let map = unsafe { Mmap::map(&file) }.map_err(|e| format!("Can't mmap {} ({})", path, e))?;
    let array = unsafe { slice::from_raw_parts(map.as_ptr().cast::<u16>(), map.len() / 2) };
    Ok((map, array))
}

/// Map the given path as read/write CPos slice into memory.
///
/// **Note**: when the returned map goes out of scope, the slice will become unusable!
pub fn short_rw_map(path: &str) -> Result<(MmapMut, &mut [u16]), String> {
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(path)
        .map_err(|e| format!("Can't read/write {} ({})", path, e))?;
    let mut map = unsafe { MmapMut::map_mut(&file) }.map_err(|e| format!("Can't mmap {} ({})", path, e))?;
    let array = unsafe { slice::from_raw_parts_mut(map.as_mut_ptr().cast::<u16>(), map.len() / 2) };
    Ok((map, array))
}

/// Map the given path as read/write CPos slice into memory.
///
/// **Note**: when the returned map goes out of scope, the slice will become unusable!
pub fn cpos_rw_map(path: &str) -> Result<(MmapMut, &mut [CPos]), String> {
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(path)
        .map_err(|e| format!("Can't read/write {} ({})", path, e))?;
    let mut map = unsafe { MmapMut::map_mut(&file) }.map_err(|e| format!("Can't mmap {} ({})", path, e))?;
    let array = unsafe { slice::from_raw_parts_mut(map.as_mut_ptr().cast::<CPos>(), map.len() / 8) };
    Ok((map, array))
}

/// Map the given path as read/write byte slice into memory.
///
/// **Note**: when the returned map goes out of scope, the slice will become unusable!
pub fn byte_rw_map(path: &str) -> Result<(MmapMut, &mut [u8]), String> {
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(path)
        .map_err(|e| format!("Can't read/write {} ({})", path, e))?;
    let mut map = unsafe { MmapMut::map_mut(&file) }.map_err(|e| format!("Can't mmap {} ({})", path, e))?;
    let start = map.as_mut_ptr();
    let array = unsafe { slice::from_raw_parts_mut(start, map.len()) };
    Ok((map, array))
}

/// Map the given path as read/write CPos anonymous slice into memory.
///
/// **Note**: when the returned map goes out of scope, the slice will become unusable!
pub fn cpos_anon_map<'a>(n_pos: usize) -> Result<(MmapMut, &'a mut [CPos]), String> {
    let mut map = /*unsafe*/ { MmapMut::map_anon(n_pos * 8) }.map_err(|e| format!("Can't mmap anon ({})", e))?;
    let array = unsafe { slice::from_raw_parts_mut(map.as_mut_ptr().cast::<CPos>(), map.len() / 8) };
    Ok((map, array))
}

/// Map the given path as read/write anonymous [u8] slice into memory.
///
/// **Note**: when the returned map goes out of scope, the slice will become unusable!
pub fn byte_anon_map<'a>(n_bytes: usize) -> Result<(MmapMut, &'a mut [u8]), String> {
    let mut map = /*unsafe*/ { MmapMut::map_anon(n_bytes) }.map_err(|e| format!("Can't mmap anon ({})", e))?;
    let array = unsafe { slice::from_raw_parts_mut(map.as_mut_ptr(), map.len()) };
    Ok((map, array))
}

/// Map the given path as read/write anonymous [u16] slice into memory
pub fn short_anon_map<'a>(n_short: usize) -> Result<(MmapMut, &'a mut [u16]), String> {
    let mut map = /*unsafe*/ { MmapMut::map_anon(n_short * 2) }.map_err(|e| format!("Can't mmap anon ({})", e))?;
    let array = unsafe { slice::from_raw_parts_mut(map.as_mut_ptr().cast::<u16>(), map.len() / 2) };
    Ok((map, array))
}

pub fn binary_file_search(
    this: CPos,
    file: &mut File,
    low: usize,
    high: usize,
) -> Result<(CPos, usize), String> {
    let mut lower = low;
    let mut upper = high;
    while lower < upper {
        let mid = lower + (upper - lower) / 2;
        match CPos::read_at(file, SeekFrom::Start(8 * mid as u64)) {
            Err(ioe) => {
                return Err(format!(
                    "error while searching pass/sorted file at {} ({})",
                    8 * mid,
                    ioe
                ));
            }
            Ok(c) => {
                if c == this {
                    return Ok((c, mid));
                } else if c < this {
                    lower = mid + 1;
                } else
                /* c >  canon */
                {
                    upper = mid;
                }
            }
        }
    }
    Err(String::from("NOT FOUND"))
}
