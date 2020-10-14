//! Data structures used in protocol and strategies.

#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code
#![allow(non_upper_case_globals)] // as well as this

use super::position as P;
use super::position::Move;
use super::position::Position;
use Protocol::*;

use std::io;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;
use std::time::Instant;

pub type Channel<A> = (mpsc::SyncSender<A>, mpsc::Receiver<A>);

// Wir brauchen einen Protocol-Channel, wobei der Receiver beim
// Protokoll-Handler ist, und die Sender gecloned beim Reader/Strategy.
// Und außerdem 2 Bool-Channel für die Antworten.

/// Data structure for the "principal variation"
/// Also used to inform GUI (i.e. xboard) about progress.
///
/// Invariant: there must be at least one Move in the moves field
#[derive(Clone, Debug)]
pub struct Variation {
    /// sequence of moves
    pub moves: Vec<Move>,
    /// the given score for the position arrived at after making all
    /// moves
    pub score: i32,
    /// estimate about searched nodes
    pub nodes: i32,
    /// estimate of search depth
    pub depth: i32,
}

/// Data structure to be sent over the Protocol Channel
#[derive(Clone, Debug)]
pub enum Protocol {
    /// sent by the Reader, when it has received a line
    Line(String),
    /// sent by the Reader, when there will be no more input
    EOF,
    /// sent by strategy when it has a move
    MV(u32, Variation),
    /// sent by strategy, when it has no more moves to offer
    NoMore(u32),
}

/// Internal state of the protocol engine
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum State {
    FORCED,
    PLAYING,
    THINKING(Instant),
    TERMINATED,
}

/// State the protocol needs
pub struct GameState {
    /// the internal state
    pub state:       State,
    /// The channel where threads send `Protocol` records to the
    /// protocol handler. ;ust be cloned and passed to new threads.
    pub toMe:        mpsc::SyncSender<Protocol>,
    /// The channel where `Protocol` records sent from threads are
    /// received.
    pub fromThreads: mpsc::Receiver<Protocol>,
    /// The channel where answers to the reader are sent.
    pub toReader:    mpsc::SyncSender<bool>,
    /// The channel where answers to the strategy are sent.
    pub toStrategy:  mpsc::SyncSender<bool>,
    /// The channel where the strategy receives answers
    pub fromMe:      mpsc::Receiver<bool>,
}

pub fn reader(sender: mpsc::SyncSender<Protocol>, recv: mpsc::Receiver<bool>) {
    loop {
        let mut buffer = String::new();
        match io::stdin().read_line(&mut buffer) {
            Ok(0) | Err(_) => {
                // eprintln!("got Err({})", x);
                sender.send(EOF).unwrap();
                return;
            }
            Ok(n) => {
                // eprintln!("got {} bytes", n);
                sender.send(Line(buffer)).unwrap();
            }
        };
        match recv.recv() {
            Ok(true) => {
                continue;
            }
            _other => {
                return;
            }
        };
    }
}

pub fn protocol() {
    let (toMain, fromThreads) = mpsc::sync_channel(1);
    let (toReader, rdrRcv): Channel<bool> = mpsc::sync_channel(1);
    let rdrSender = toMain.clone();
    thread::spawn(move || reader(rdrSender, rdrRcv));
    loop {
        let p = fromThreads.recv().unwrap();
        match p {
            Line(s) => match s.trim() {
                "quit" => {
                    toReader.send(false).unwrap();
                    return;
                }
                wrong => {
                    eprintln!("not yet implemented: {}", wrong);
                    toReader.send(true).unwrap();
                }
            },
            EOF => {
                return;
            }
            _ => toReader.send(false).unwrap(),
        }
    }
}
