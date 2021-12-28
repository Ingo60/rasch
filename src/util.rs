///! Some functions at the bottom level.
use sysinfo::SystemExt;

/// Number of `CPos` to use in a buffer.
///
/// We try to use 8 GB for in-place sort.
/// Why 8 GB? Because my computer has only 16GB memory and I want to do
/// some other things while the sort runs for hours.
pub const CHUNK: usize = 1024 * 1024 * 1024;

/// Use a higher buffer size for our files
pub const BUFSZ: usize = 16 * 1024 * 1024;

/// Helper to print big numbers nicely
pub fn formatted64(u: u64) -> String {
    let mut result = String::with_capacity(64);
    formatu64(u, &mut result);
    result
}

/// Helper to print sizes nicely
pub fn formatted_sz(u: usize) -> String {
    formatted64(u as u64)
}

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

/// How much memory we may allocate for the compressed positions vector and the position hash map
pub const MAX_USE_MEMORY_PERCENT: usize = 75;
/// Size of a CPos
pub const SIZE_CPOS: usize = 8;
/// Average estimated size of an entry in the cache. If you expierience paging,
/// - decrease `MAX_USE_MEMORY_PERCENT` or
/// - increace this one so that less cache entries get allocated or
/// - close google and vscode during runs :)
const SIZE_CACHE_ENTRY_AVG: usize = 71 * 6; // 71 elements at 6 bytes

/// Computes number of entries for allocation in positions vector and cache for memory processing.
/// Returns two numbers, `a` and `b` such that 3/4 of the memory go to the vector and 1/4 to the cache.

pub fn compute_sizes() -> (usize, usize) {
    let mut info = sysinfo::System::new();
    info.refresh_memory();
    let total = (MAX_USE_MEMORY_PERCENT * info.total_memory() as usize / 100) * 1024;
    // RAM = t * MAX_USE_MEMORY_PERCENT / 100
    // (a * P + a/4 * C) / 1024 = total
    // (a*P) = 3 *
    let a = ((3 * total) / 4) / SIZE_CPOS;
    let b = (total / 4) / SIZE_CACHE_ENTRY_AVG;
    (a, b)
}

/// Compute the number of possible cache entries when we need `vecmax` vector entries.
pub fn compute_hash(vecmax: usize) -> usize {
    let mut info = sysinfo::System::new();
    info.refresh_memory();
    let total = (MAX_USE_MEMORY_PERCENT * info.total_memory() as usize / 100) * 1024;
    if vecmax * SIZE_CPOS > total as usize {
        0
    } else {
        (total as usize - vecmax * SIZE_CPOS) / SIZE_CACHE_ENTRY_AVG
    }
}

pub fn fac(n: u64) -> u64 {
    if n == 0 {
        1
    } else {
        n * fac(n - 1)
    }
}
/// over n 0 = 1
/// over n 1 = n
/// over n 2 = n * (n-1) / n!
pub fn over(n: u64, k: u64) -> u64 {
    if k == 0 {
        1
    } else if k == 1 {
        n
    } else {
        (n + 1 - k..n + 1).fold(1, |acc, x| x * acc) / fac(k)
    }
}
