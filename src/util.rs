//! Some functions and constants at the bottom level.

use sysinfo::SystemExt;

/// Number of `CPos` to use in a buffer.
///
/// We try to use 8 GB for in-place sort.
/// Why 8 GB? Because my computer has only 16GB memory and I want to do
/// some other things while the sort runs for hours.
pub const CHUNK: usize = 256 * 1024 * 1024;

/// Use a higher buffer size for our files
pub const BUFSZ: usize = 20 * 1024 * 1024;

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

///    formatted_h(998, "") -> " 998"
///    formatted_h(1002, "K") -> "1.0K"
///    formatted_h(9999, "K") -> "9.9K"
///    formatted_h(19999, "K") -> "19M"
pub fn formatted_h(n: usize, unit: char) -> String {
    let next_unit = |c| match c {
        'K' => 'M',
        'M' => 'G',
        'G' => 'T',
        'T' => 'P',
        'P' => '?',
        '?' => '¿',
        _ => 'K',
    };
    if next_unit(unit) == 'K' {
        if n > 9999 {
            let h = n / 1000;
            if h < 100 {
                format!("{}.{}K", h / 10, h % 10)
            } else {
                formatted_h(n / 1000, 'K')
            }
        } else {
            format!("{:>4}", n)
        }
    } else if n > 999 {
        let h = n / 100;
        if h < 100 {
            format!("{}.{}{}", h / 10, h % 10, next_unit(unit))
        } else {
            formatted_h(n / 1000, next_unit(unit))
        }
    } else {
        format!("{:>3}{}", n, unit)
    }
}
/// Formats the number of remaining items in short, human readable form.
///
/// e.g. fmt_human(15000, 1000) = " 14k"
pub fn fmt_human(w: usize, r: usize) -> String {
    if w >= r {
        formatted_h(w - r, '1')
    } else {
        "????".to_string()
    }
}

/// print a progress indication consisting of promille value and remaining whatever
///     '1234‰ 3.7G '
pub fn progress(current: usize, maximum: usize) {
    if maximum == 0 {
        eprint!("   0‰    0 ");
    } else {
        eprint!(
            "\x08\x08\x08\x08\x08\x08\x08\x08\x08\x08\x08{:4}‰ {} ",
            (current + 1) * 1000 / maximum,
            formatted_h(maximum - (current + 1).min(maximum), '1')
        );
    }
}

/// How much memory we may allocate for the compressed positions vector and the position hash map
pub const MAX_USE_MEMORY_PERCENT: usize = 50;

/// Size of a CPos
pub const SIZE_CPOS: usize = 8;

/// Average estimated size of an entry in the cache. If you expierience paging,
/// - decrease `MAX_USE_MEMORY_PERCENT` or
/// - increace this one so that less cache entries get allocated or
/// - close google and vscode during runs :)
const SIZE_CACHE_ENTRY_AVG: usize = 72 * 8; // 71 elements at 6 bytes

/// Computes number of CPos possible to allocate when `reserved` bytes are not to be used.
pub fn compute_vector_entries(reserved: usize) -> usize {
    let mut info = sysinfo::System::new();
    info.refresh_memory();
    let total_bytes = (MAX_USE_MEMORY_PERCENT * info.total_memory() as usize / 100) * 1024;
    (total_bytes - reserved) / SIZE_CPOS
}

/// Compute the number of possible cache entries when we need `vecmax` vector entries.
pub fn compute_cache_entries(vecmax: usize) -> usize {
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
