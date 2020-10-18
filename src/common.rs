//! Data structures used in protocol and strategies.

#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code
#![allow(non_upper_case_globals)] // as well as this

use super::computing;
use super::fen;
use super::position as P;
use super::position::Move;
use super::position::Player;
use super::position::Player::*;
use super::position::Position;
use Protocol::*;
use State::*;

use std::cmp::{max, min};
use std::collections::HashMap;
use std::io;
use std::io::Write;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;
use std::time::Instant;

use rand::Rng;

// pub static mut trtable: Arc<Mutex<HashMap<i32, String>>> =
// Arc::new(Mutex::new(HashMap::new()));

// Wir brauchen einen Protocol-Channel, wobei der Receiver beim
// Protokoll-Handler ist, und die Sender gecloned beim Reader/Strategy.
// Und außerdem 2 Bool-Channel für die Antworten.

/// Size of the Moves array in a variation.
/// (Must be at least 3)
pub const VariationMoves: usize = 16;

/// Data structure for the "principal variation"
/// Also used to inform GUI (i.e. xboard) about progress.
///
/// Invariant: there must be at least one Move in the moves field
#[derive(Clone, Copy, Debug)]
pub struct Variation {
    /// the given score for the position arrived at after making all
    /// moves
    pub score:  i32,
    /// estimate about searched nodes
    pub nodes:  u32,
    /// estimate of search depth
    pub depth:  u32,
    /// actual utilization of the array
    pub length: u32,
    /// sequence of moves, mostly used for display.
    pub moves:  [Move; VariationMoves as usize],
}

impl Variation {
    /// return the moves as slice
    pub fn moves(&self) -> &[Move] { &self.moves[0..self.length as usize] }

    /// Push a move.
    /// Returns the unchanged variation when there is not enough space
    pub fn push(&self, mv: Move) -> Variation {
        let mut pv = *self;
        if pv.length < VariationMoves as u32 {
            pv.moves[pv.length as usize] = mv;
            pv.length += 1;
        }
        pv
    }

    /// Pop a move.
    /// Returns the unchanged variation when the moves array is empty
    pub fn pop(&self) -> Variation {
        let mut pv = *self;
        if pv.length > 0 {
            pv.length -= 1;
        }
        pv
    }

    /// Unshift a move, that is, push it at the front.
    /// Subsequent moves are shifted to the right, and the last one
    /// might fall out
    pub fn unshift(&self, mv: Move) -> Variation {
        let mut pv = *self;
        let newlen = min(pv.length + 1, VariationMoves as u32);
        while pv.length > 0 {
            if pv.length < VariationMoves as u32 {
                pv.moves[pv.length as usize] = pv.moves[pv.length as usize - 1]
            };
        }
        pv.moves[0] = mv;
        pv.length = newlen;
        pv
    }

    /// Return the last move
    pub fn last(&self) -> Option<Move> {
        if self.length > 0 {
            Some(self.moves[self.length as usize - 1])
        } else {
            None
        }
    }

    /// Return the first move
    pub fn first(&self) -> Option<Move> {
        if self.length > 0 {
            Some(self.moves[0])
        } else {
            None
        }
    }

    /// Fill the moves array from an iterator
    pub fn from_iter<I>(&self, iter: &mut I) -> Variation
    where
        I: Iterator<Item = Move>,
    {
        let mut pv = *self;
        pv.length = 0;
        while pv.length < VariationMoves as u32 {
            match iter.next() {
                Some(mv) => {
                    pv.moves[pv.length as usize] = mv;
                    pv.length += 1;
                }
                None => break,
            }
        }
        pv
    }

    /// Give the moves in algebraic notation, separated by spaces
    pub fn showMoves(&self) -> String { P::showMoves(self.moves()) }
}

/// Data structure to be found in the transposition table
#[derive(Clone, Debug)]
pub struct Transp {
    /// At what depth was this transposition made?
    pub depth:    u32,
    /// Score with integrated bounds.
    /// Values of the form `4×n`   mean `n` is an exact score (i.e.
    /// computed with eval()) Values of the form `4×n+1` mean `n` is
    /// a lower bound, that is, the score may be higher.
    /// Values of the form `4xn-1` mean `n` is an upper bound, that is,
    /// at most `n`
    pub score:    i32,
    /// moves of the principal variation
    pub pvMoves:  Vec<Move>,
    /// (ordered) moves of the associated position
    pub posMoves: Vec<Move>,
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

pub type Strategy = fn(StrategyState) -> ();

#[derive(Debug)]
pub struct StrategyState {
    /// unique identifier for this instance
    pub sid:      u32,
    /// The sender used to send `Protocol` messages to the protocol
    /// handler
    pub sender:   mpsc::SyncSender<Protocol>,
    /// The receiver used to get acknowledgements from the protocol
    /// handler
    pub receiver: mpsc::Receiver<bool>,
    /// The game history. The last pushed position is the current one.
    /// There will be at least one position.
    pub history:  Vec<Position>,
    /// If the user move matched the move we expected, the previously
    /// computed variation is given back here. It may be sufficient
    /// to analyze just our suggested move, which is at 3rd
    /// position, if any.
    pub plan:     Option<Variation>,
    /// Transposition table
    pub trtable:  Arc<Mutex<HashMap<Position, Transp>>>,
}

impl StrategyState {
    /// Current position. Will panic, if history is an empty Vec.
    pub fn current(&self) -> Position {
        match self.history.last() {
            None => {
                panic!("StrategyState with empty history must not happen.");
            }
            Some(&p) => p,
        }
    }
    /// Current player. Will panic, if history is an empty Vec.
    pub fn player(&self) -> Player { self.current().turn() }
    /// send something to the protocol handler and wait for the answer
    pub fn talkDriver(&self, p: Protocol) -> bool {
        match self.sender.send(p) {
            Ok(_) => match self.receiver.recv() {
                Ok(cont) => cont,
                Err(_) => false,
            },
            Err(_) => false,
        }
    }
    /// send a variation to the protocol handler and wait for the answer
    pub fn talkPV(&self, v: Variation) -> bool { self.talkDriver(MV(self.sid, v)) }

    /// Send something to the protocol handler and don't wait for the
    /// answer This should be the last thing the strategy ever
    /// sends.
    pub fn tellDriver(&self, p: Protocol) -> () { self.sender.send(p).unwrap_or_default() }

    /// send a final NoMore
    pub fn tellNoMore(&self) { self.tellDriver(NoMore(self.sid)) }
}

/// State the protocol needs
pub struct GameState {
    /// the internal state
    pub state:       State,
    /// The colour we are playing
    pub player:      Player,
    /// Best move so far
    pub best:        Option<Variation>,
    /// nodes searched
    pub nodes:       u32,
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
    /// Transposition table
    pub trtable:     Arc<Mutex<HashMap<Position, Transp>>>,
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
            nodes: 0,
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
            trtable: Arc::new(Mutex::new(HashMap::new())),
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
    pub fn timePerMove(&self) -> u64 {
        let remaining = max(
            -500,
            min(3000, self.myTime.as_millis() as i32 - self.oTime.as_millis() as i32),
        ) + (max(1000, self.myTime.as_millis() as i32 / 25));
        assert!(remaining >= 500);
        return remaining as u64;
    }

    /// This is the main loop of the protocol handler
    pub fn mainLoop(&mut self, strategy: Strategy)
    // where
    //    S: 'static + Strategy + Clone + Send,
    {
        loop {
            match self.state {
                TERMINATED => return,
                FORCED => {
                    let input = self.fromThreads.recv().unwrap();
                    self.processForced(&input);
                }
                PLAYING => {
                    if self.current().turn() == self.player {
                        // it's our turn, nevertheless, we make sure there is no other request
                        // at this time before starting the thread
                        match self.fromThreads.try_recv() {
                            Ok(input) => self.processForced(&input),
                            Err(mpsc::TryRecvError::Empty) => self.start(strategy),
                            Err(mpsc::TryRecvError::Disconnected) => return,
                        }
                    } else {
                        let input = self.fromThreads.recv().unwrap();
                        self.processForced(&input);
                    }
                }
                THINKING(since) => {
                    let used = since.elapsed().as_millis() as u64;
                    let have = self.timePerMove();
                    let time = if have < used { 500 } else { max(500, have - used) };
                    let todo = if let None = self.best {
                        // when we have no move yet, wait for one, no matter what
                        match self.fromThreads.recv() {
                            Ok(x) => Ok(x),
                            Err(mpsc::RecvError) => Err(mpsc::RecvTimeoutError::Disconnected),
                        }
                    } else {
                        // wait between 500ms and the calculated millis per move
                        self.fromThreads.recv_timeout(Duration::from_millis(time))
                    };
                    // at this point, we have an answer, a timeout or a disconnect
                    // note that xboard may send commands even while we are thinking
                    match todo {
                        Err(mpsc::RecvTimeoutError::Disconnected) => {
                            panic!("should not happen - channel disconnected while thinking")
                        }
                        Err(mpsc::RecvTimeoutError::Timeout) => {
                            println!("# Thinking timed out.");
                            io::stdout().flush().unwrap_or_default();
                            if let Some(sender) = &self.toStrategy {
                                // ask strategy to finish if it is still listening
                                sender.try_send(false).unwrap_or_default();
                            }
                            computing::finishThinking();
                            self.sendMove();
                        }
                        Ok(EOF) => {
                            println!("# EOF on standard input while thinking!?");
                            io::stdout().flush().unwrap_or_default();
                            if let Some(sender) = &self.toStrategy {
                                // ask strategy to finish if it is still listening
                                sender.try_send(false).unwrap_or_default();
                            }
                            computing::finishThinking();
                            self.state = TERMINATED;
                        }
                        Ok(Line(input)) => self.xboardCommand(input.trim()),
                        Ok(MV(sid, var)) if sid == self.sid => {
                            let usedMillis = since.elapsed().as_millis() as u64;
                            // make sure the next depth is not tried if we have already used more
                            // than the half of the assigned time
                            let goOn = usedMillis < (2 * self.timePerMove() / 3);
                            self.nodes += var.nodes;
                            if let Some(sender) = &self.toStrategy {
                                sender.send(goOn).unwrap();
                            };
                            println!(
                                "# we have used {}ms of {}ms, continue={}",
                                usedMillis,
                                self.timePerMove(),
                                goOn
                            );
                            // show the progress
                            println!(
                                " {} {} {} {} {}",
                                var.depth,
                                self.player.factor() * var.score,
                                (usedMillis + 5) / 10,
                                self.nodes,
                                var.showMoves()
                            );
                            io::stdout().flush().unwrap_or_default();
                            // TODO: compute best
                            let oracle: bool = rand::thread_rng().gen();
                            let best = match &self.best {
                                None => var,
                                Some(old) => {
                                    if var.length > 0 && old.length > 0 && var.moves[0] == old.moves[0] {
                                        var
                                    } else if (var.score - old.score).abs() <= 5 {
                                        if oracle {
                                            var
                                        } else {
                                            old.clone()
                                        }
                                    } else if self.player == WHITE && var.score > old.score
                                        || self.player == BLACK && var.score < old.score
                                    {
                                        var
                                    } else {
                                        old.clone()
                                    }
                                }
                            };
                            self.best = Some(best);
                            if !goOn {
                                self.nodes = 0;
                                self.sendMove();
                            }
                        }
                        Ok(MV(_, _)) => {
                            println!("# ignoring move from previous strategy instance.");
                            io::stdout().flush().unwrap_or_default();
                        }
                        Ok(NoMore(id)) if id == self.sid => {
                            println!("# No more moves.");
                            self.nodes = 0;
                            self.sendMove();
                        }
                        Ok(NoMore(_)) => {
                            println!("# ignoring NoMore from previous strategy instance.");
                            io::stdout().flush().unwrap_or_default();
                        }
                    }
                }
            };
        }
    }

    /// Start a new thread that'll figure out a move
    pub fn start(&mut self, strategy: Strategy) {
        let mvs = self.current().moves();
        let mate = mvs.len() == 0 && self.current().inCheck(self.current().turn());
        let stalemate = mvs.len() == 0 && !mate;

        if mate || stalemate {
            println!("# thinking finds game that has ended");
            io::stdout().flush().unwrap_or_default();
            self.state = FORCED;
            self.best = None;
            self.nodes = 0;
        } else {
            let since = Instant::now();
            let (snd, rcv) = mpsc::sync_channel(1);
            computing::beginThinking();
            let state = StrategyState {
                sid:      self.sid,
                sender:   self.toMain.clone(),
                receiver: rcv,
                history:  self.history.clone(),
                plan:     self.best,
                trtable:  Arc::clone(&self.trtable),
            };
            thread::spawn(move || strategy(state));
            self.state = THINKING(since);
            self.best = None;
            self.nodes = 0;
            self.toStrategy = Some(snd);
        }
    }

    /// Send the best move, if any
    pub fn sendMove(&mut self) {
        match &self.best {
            None => {
                println!("# strategy busy, but no move found yet!");
                println!("resign");
                io::stdout().flush().unwrap_or_default();
                self.state = FORCED;
                self.nodes = 0;
                computing::finishThinking();
                if let Some(sender) = &self.toStrategy {
                    sender.try_send(false).unwrap_or_default();
                    self.toStrategy = None;
                }
                self.sid += 1;
            }
            Some(pv) => {
                assert!(pv.length > 0); // there must be moves
                println!("move {}", pv.moves[0].algebraic());
                let pos = self.current().apply(pv.moves[0]);
                let ms = pos.moves();
                let mate = ms.len() == 0 && pos.inCheck(pos.turn());
                let stalemate = ms.len() == 0 && !mate;
                let moves50 = !mate && !stalemate && pos.getPlyCounter() >= 100;
                // let repetition =
                //     !mate && !stalemate && !moves50 &&
                // self.history.iter().filter(|&p| *p == pos).count() >= 3;
                let finished = mate || stalemate || moves50;
                if stalemate {
                    println!("1/2-1/2 {{Stalemate}}");
                }
                if mate {
                    match pos.turn() {
                        WHITE => println!("0-1 {{Black mates}}"),
                        BLACK => println!("1-0 {{White mates}}"),
                    }
                };
                if moves50 {
                    println!("1/2-1/2 {{50 moves}}");
                }
                io::stdout().flush().unwrap_or_default();
                self.history.push(pos.clearRootPlyCounter());
                self.sid += 1;
                self.nodes = 0;
                if finished {
                    self.state = FORCED;
                    self.best = None;
                } else {
                    self.state = PLAYING
                }
            }
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
        self.toReader.send(cmd != "quit").unwrap_or_default();
        // note the current state
        let wasThinking = if let THINKING(_) = self.state { true } else { false };

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
            Some("usermove") => match iter.next() {
                Some(alg) => match Move::unAlgebraic(&self.current().moves(), alg) {
                    Ok(mv) => {
                        self.history.push(self.current().apply(mv).clearRootPlyCounter());
                        self.sid += 1;
                        if let Some(plan) = self.best {
                            if self.state == PLAYING && plan.length > 2 && mv == plan.moves[1] {
                                println!(
                                    "# user played expected move {} our answer may be {}",
                                    mv.algebraic(),
                                    plan.moves[2]
                                );
                            } else {
                                self.best = None;
                                println!("# not PLAYING, variation too short or unexpected user move - no plan");
                            }
                        }
                    }
                    Err(_) => println!("Illegal move: {}", alg),
                },
                None => println!("Illegal move: "),
            },
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
            // "move now" command only in THINKING mode, but gets ignored if we haven't any move yet
            Some("?") if wasThinking => {
                if let Some(_) = self.best {
                    self.sendMove();
                } else {
                    println!("# cannot honour '?' command, I have no move yet.");
                }
            }
            Some("?") => println!("Error (command not legal now): ?"),
            Some(unknown) => println!("Error (unknown command): {}", unknown),
        };
        // flush the output stream
        io::stdout().flush().unwrap_or_default();

        // try to shut down thinking thread gracefully
        if wasThinking {
            match self.state {
                THINKING(_) => (),
                _other => {
                    computing::finishThinking();
                    if let Some(sender) = &self.toStrategy {
                        sender.try_send(false).unwrap_or_default();
                        self.toStrategy = None;
                    }
                }
            }
        };
    }
}

pub fn reader(sender: mpsc::SyncSender<Protocol>, recv: mpsc::Receiver<bool>) {
    loop {
        let mut buffer = String::new();
        match io::stdin().read_line(&mut buffer) {
            Ok(0) | Err(_) => {
                // eprintln!("got Err({})", x);
                sender.send(EOF).unwrap_or_default();
                return;
            }
            Ok(_) => {
                // eprintln!("got {} bytes", n);
                sender.send(Line(buffer)).unwrap_or_default();
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
