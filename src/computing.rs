#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

/*!
Atomic flag to signal end of computing.
This is only ever written by the main thread.
*/

use std::sync::atomic::{AtomicBool, Ordering};

static stopThinking: AtomicBool = AtomicBool::new(false);

/// indicate to finish thinking ASAP
pub fn finishThinking() {
    // unsafe {
    stopThinking.store(true, Ordering::Relaxed);
    // }
}

/// indicate that thinking may go on
pub fn beginThinking() {
    // unsafe {
    stopThinking.store(false, Ordering::Relaxed);
    // }
}

/// tell whether thinking should finish
pub fn thinkingFinished() -> bool {
    // unsafe {
    stopThinking.load(Ordering::Relaxed)
    // }
}
