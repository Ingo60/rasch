//! Data structures used in protocol and strategies.

#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code
#![allow(non_upper_case_globals)] // as well as this

use super::fen;
use super::position as P;
use super::position::Move;
use super::position::Player;
use super::position::Player::*;
use super::position::Position;
use Protocol::*;
use State::*;

use std::cmp::{max, min};
use std::io;
use std::io::Write;
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
    /// The colour we are playing
    pub player:      Player,
    /// Best move so far
    pub best:        Option<Variation>,
    /// The channel where threads send `Protocol` records to the
    /// protocol handler. Must be cloned and passed to new threads.
    pub toMain:      mpsc::SyncSender<Protocol>,
    /// The channel where `Protocol` records sent from threads are
    /// received.
    pub fromThreads: mpsc::Receiver<Protocol>,
    /// The channel where answers to the reader are sent.
    pub toReader:    mpsc::SyncSender<bool>,
    /// The channel where answers to the strategy are sent.
    pub toStrategy:  Option<mpsc::SyncSender<bool>>,
    // The channel where the strategy receives answers
    // pub fromMain:    mpsc::Receiver<bool>,
    /// The history of the game
    pub history:     Vec<Position>,
    /// Our remaining time
    pub myTime:      Duration,
    /// Their remaining time
    pub oTime:       Duration,
    /// Time for entire game
    pub gameTime:    Duration,
    /// Incremental time
    pub incrTime:    Duration,
    /// Number of moves for game, or 0 for incremental
    pub gameMoves:   i32,
    /// identifier for strategy
    pub sid:         u32,
}

impl GameState {
    /// initialize game and start the reader thread
    pub fn new() -> GameState {
        let (toMain, fromThreads) = mpsc::sync_channel(1);
        let (toReader, rdrRcv) = mpsc::sync_channel(1);
        // let (toStrategy, fromMain) = mpsc::sync_channel(1);
        let rdrSender = toMain.clone();
        thread::spawn(move || reader(rdrSender, rdrRcv));
        GameState {
            state: FORCED,
            player: WHITE,
            best: None,
            toMain,
            fromThreads,
            toReader,
            toStrategy: None,
            history: vec![P::initialBoard()],
            gameMoves: 0,
            gameTime: Duration::new(0, 0),
            incrTime: Duration::new(0, 0),
            myTime: Duration::new(0, 0),
            oTime: Duration::new(0, 0),
            sid: 0,
        }
    }

    /// Current position. Will panic, if history is an empty Vec.
    pub fn current(&self) -> Position {
        match self.history.last() {
            None => {
                panic!("GameState with empty history must not happen.");
            }
            Some(&p) => p,
        }
    }

    /// Time for this move in milliseconds
    /// If we are ahead in time, adds maximum of 3 seconds to the
    /// remaining time, which is calculated to last another 25
    /// moves, but at least 1 second. If we are behind in time,
    /// subtracts 0,5 seconds from the remaining time.
    pub fn timePerMove(&self) -> i32 {
        max(
            -500,
            min(3000, self.myTime.as_millis() as i32 - self.oTime.as_millis() as i32),
        ) + (max(1000, self.myTime.as_millis() as i32 / 25))
    }

    /// This is the main loop of the protocol handler
    pub fn mainLoop(&mut self) {
        loop {
            match self.state {
                TERMINATED => return,
                FORCED => {
                    let input = self.fromThreads.recv().unwrap();
                    self.processForced(&input);
                }
                _ => panic!("not yet implemented"),
            };
        }
    }

    /// Processing of protocol commands
    pub fn processForced(&mut self, input: &Protocol) {
        match input {
            EOF => {
                if let Some(sender) = &self.toStrategy {
                    let _ = sender.try_send(false);
                }
                self.state = TERMINATED;
                return;
            }
            NoMore(u) => println!("# ignoring an unexpected NoMore from {}.", u),
            MV(u, _) => println!("# ignoring an unexpected move from {}.", u),
            Line(line) => self.xboardCommand(line.trim()),
        };
    }

    /// Processing input from xboard
    pub fn xboardCommand(&mut self, cmd: &str) {
        // tell the reader to continue unless we got the quit command
        self.toReader.send(cmd != "quit").unwrap();

        let mut iter = cmd.split_whitespace();
        match iter.next() {
            None => return, // silently ignore empty line
            Some("quit") => self.state = TERMINATED,
            Some("accepted") | Some("rejected") | Some("xboard") | Some("random") | Some("hard") | Some("easy")
            | Some("post") | Some("computer") | Some("cores") | Some("level") | Some("st") | Some("sd")
            | Some("nps") => (), // ignored
            Some("protover") => {
                println!("feature myname=\"rasch resign\"");
                println!("feature ping=0 setboard=1 playother=1 usermove=1 draw=0");
                println!("feature sigint=0 analyze=1 variants=\"normal\" colors=0 nps=0");
                println!("feature debug=1 memory=0 smp=1 done=1");
            }
            Some("new") => {
                self.history = vec![P::initialBoard()];
                self.state = PLAYING;
                self.player = BLACK;
                self.best = None;
                self.toStrategy = None;
            }
            Some("force") => self.state = FORCED,
            Some("playother") => {
                self.state = PLAYING;
                self.player = self.current().turn().opponent();
            }
            Some("go") => {
                self.state = PLAYING;
                self.player = self.current().turn();
            }
            Some("setboard") => {
                self.state = FORCED;
                let rest: Vec<_> = iter.collect();
                match fen::decodeFEN(&rest[..].join(" ")) {
                    Err(s) => println!("Error ({})", s),
                    Ok(p) => {
                        self.history = vec![p];
                        self.best = None;
                    }
                }
            }
            Some("usermove") => {
                match iter.next() {
                    Some(alg) => match Move::unAlgebraic(&self.current().moves(), alg) {
                        Ok(mv) => {
                            self.history.push(self.current().apply(mv).clearRootPlyCounter());
                            // TODO: more logic for expected move
                        }
                        Err(_) => println!("Illegal move: {}", alg),
                    },
                    None => println!("Illegal move: "),
                }
            }
            Some("result") => self.state = FORCED,
            Some("undo") => {
                self.state = FORCED;
                if self.history.len() > 1 {
                    self.history.pop();
                }
            }
            Some("remove") => {
                self.state = FORCED;
                if self.history.len() > 2 {
                    self.history.pop();
                    self.history.pop();
                }
            }
            Some("time") => match iter.next() {
                Some(number) => match number.parse::<u64>() {
                    Ok(t) => self.myTime = Duration::from_millis(10 * t),
                    Err(_) => println!("Error (time not numeric)"),
                },
                None => println!("Error (number missing after time)"),
            },
            Some("otim") => match iter.next() {
                Some(number) => match number.parse::<u64>() {
                    Ok(t) => self.oTime = Duration::from_millis(10 * t),
                    Err(_) => println!("Error (time not numeric)"),
                },
                None => println!("Error (number missing after otim)"),
            },
            Some(unknown) => {
                println!("Error (unknown command): {}", unknown);
            }
        };
        io::stdout().flush().unwrap();
    }
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
            Ok(_) => {
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
