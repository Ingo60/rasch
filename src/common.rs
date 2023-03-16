//! Data structures used in protocol and strategies.

#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code
#![allow(non_upper_case_globals)] // as well as this

use super::computing;
use super::endgamedb as E;
use super::fen;
use super::opening::{setupOpening, OpeningMap};
use super::position as P;
use super::position::Move;
use super::position::Player;
use super::position::Player::*;
use super::position::Position;
use Protocol::*;
use State::*;

use rand::Rng;
use std::cmp::{max, min};
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::fmt::Formatter;
use std::io;
use std::io::Write;
use std::ops::Sub;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;
use std::time::Instant;

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
    pub score: i32,
    /// estimate about searched nodes
    pub nodes: u32,
    /// estimate of search depth
    pub depth: u32,
    /// actual utilization of the array
    pub length: u32,
    /// sequence of moves, mostly used for display.
    pub moves: [Move; VariationMoves as usize],
}

impl Variation {
    /// Return the moves as slice.
    /// Not that the moves come out in **reverse order**!
    pub fn rmoves(&self) -> &[Move] {
        &self.moves[0..self.length as usize]
    }

    /// Return the moves as slice in the correct order
    pub fn moves(&self) -> Vec<Move> {
        let mut v: Vec<Move> = self.rmoves().iter().copied().collect();
        // let mut arr = v[..];
        v[..].reverse();
        v
    }
    /// Push a move.
    /// Returns the unchanged variation when there is not enough space
    pub fn push(&self, mv: Move) -> Variation {
        let mut pv = *self;
        if pv.length < VariationMoves as u32 {
            pv.moves[pv.length as usize] = mv;
            pv.length += 1;
        } else {
            // make room by shifting left, moving the farthest away move out
            for i in 1..VariationMoves {
                pv.moves[i - 1] = pv.moves[i];
            }
            pv.moves[VariationMoves - 1] = mv;
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
            pv.length -= 1;
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
        for m in iter {
            pv.moves[pv.length as usize] = m;
            pv.length += 1;
        }
        // reverse
        if pv.length > 1 {
            let mut i: usize = 0;
            let mut j: usize = pv.length as usize - 1;
            while i < j {
                let m = pv.moves[i];
                pv.moves[i] = pv.moves[j];
                pv.moves[j] = m;
                i += 1;
                j -= 1;
            }
        }
        pv
    }

    /// Give the moves in coordinate notation, separated by spaces
    pub fn showMoves(&self) -> String {
        P::showMoves(&self.moves()[..])
    }

    /// Give the moves in standard algebraic notation, separated by
    /// spaces
    pub fn showMovesSAN(&self, pos: Position) -> String {
        P::showMovesSAN(&self.moves()[..], pos)
    }
}

/// Data structure to be found in the transposition table
#[derive(Clone, Debug)]
pub struct Transp {
    /// To what half-move does this correspond, or is it an permanent
    /// entry? Used in hash cleaning, only entries that relate to
    /// positions reached earlier are removed. Also, if the value is
    /// the maximum value, it is permanent and never removed.
    pub halfmove: u32,
    /// At what depth was this transposition made?
    pub depth: u32,
    /// Score with integrated bounds.
    /// Values of the form `4×n`   mean `n` is an exact score (i.e.
    /// computed with eval()) Values of the form `4×n+1` mean `n` is
    /// a lower bound, that is, the score may be higher.
    /// Values of the form `4xn-1` mean `n` is an upper bound, that is,
    /// at most `n`
    pub score: i32,
    /// moves of the principal variation
    pub pvMoves: [Move; VariationMoves],
    /// number of valid moves in pvMoves
    pub pvLength: u32,
    /// (ordered) moves of the associated position
    pub posMoves: Vec<Move>,
}

/// Data structure for simple transposition table
#[derive(Clone, Debug)]
pub struct SimpleTransp {
    /// To what half-move does this correspond, or is it an permanent
    /// entry? Used in hash cleaning, only entries that relate to
    /// positions reached earlier are removed. Also, if the value is
    /// the maximum value, it is permanent and never removed.
    pub halfmove: u32,
    /// The variation computed for a position, depth is exact and
    /// pv.depth and pv.length > 0
    pub pv: Variation,
    /// The (ordered) moves in this position
    pub moves: Vec<Move>,
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
    /// THINKING(x, None) is regular thinking
    /// THINKING(x, Some(mv)) is pondering on move mv
    THINKING(Instant, Option<Move>),
    TERMINATED,
}

impl Display for State {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            FORCED => write!(f, "FORCED"),
            PLAYING => write!(f, "PLAYING"),
            TERMINATED => write!(f, "TERMINATED"),
            THINKING(since, Some(mv)) => {
                write!(f, "PONDERING({}ms on {})", since.elapsed().as_millis(), mv.algebraic())
            }
            THINKING(since, None) => write!(f, "THINKING({}ms)", since.elapsed().as_millis()),
        }
    }
}

pub type Strategy = fn(StrategyState) -> ();

/// In the simplest form, a transposition table degenerates into a
/// single move
pub type SimpleTranspositionHash = HashMap<Position, SimpleTransp>;

#[derive(Debug)]
pub struct StrategyState {
    /// unique identifier for this instance
    pub sid: u32,
    /// The sender used to send `Protocol` messages to the protocol
    /// handler
    pub sender: mpsc::SyncSender<Protocol>,
    /// The receiver used to get acknowledgements from the protocol
    /// handler
    pub receiver: mpsc::Receiver<bool>,
    /// The game history. The last pushed position is the current one.
    /// There will be at least one position.
    pub history: Vec<Position>,
    /// Transposition table
    pub trtable: Arc<Mutex<HashMap<Position, Transp>>>,
    /// Opening map
    pub openings: Arc<Mutex<OpeningMap>>,
    /// Simple transposition table
    pub strtab: Arc<Mutex<SimpleTranspositionHash>>,
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
    pub fn player(&self) -> Player {
        self.current().turn()
    }
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
    pub fn talkPV(&self, v: Variation) -> bool {
        self.talkDriver(MV(self.sid, v))
    }

    /// Send something to the protocol handler and don't wait for the
    /// answer This should be the last thing the strategy ever
    /// sends.
    pub fn tellDriver(&self, p: Protocol) -> () {
        self.sender.send(p).unwrap_or_default()
    }

    /// send a final NoMore
    pub fn tellNoMore(&self) {
        self.tellDriver(NoMore(self.sid))
    }
}

/// State the protocol needs
pub struct GameState {
    /// The name of the strategy
    pub name: String,
    /// the internal state
    pub state: State,
    /// The colour we are playing
    pub player: Player,
    /// Best move so far
    pub best: Option<Variation>,
    /// nodes searched
    pub nodes: u32,
    /// The channel where threads send `Protocol` records to the
    /// protocol handler. Must be cloned and passed to new threads.
    pub toMain: mpsc::SyncSender<Protocol>,
    /// The channel where `Protocol` records sent from threads are
    /// received.
    pub fromThreads: mpsc::Receiver<Protocol>,
    /// The channel where answers to the reader are sent.
    pub toReader: mpsc::SyncSender<bool>,
    /// The channel where answers to the strategy are sent.
    pub toStrategy: Option<mpsc::SyncSender<bool>>,
    // The channel where the strategy receives answers
    // pub fromMain:    mpsc::Receiver<bool>,
    /// The history of the game
    pub history: Vec<Position>,
    /// Our remaining time
    pub myTime: Duration,
    /// Their remaining time
    pub oTime: Duration,
    /// Time for entire game
    pub gameTime: Duration,
    /// Incremental time
    pub incrTime: Duration,
    /// Number of moves for game, or 0 for incremental
    pub gameMoves: i32,
    /// True when pondering (thinking while it's the other users turn)
    /// is allowed through GUI (xboard xommands "hard", "easy")
    pub ponderMode: bool,
    /// True if PV output should be posted (xboard commands "post",
    /// "nopost")
    pub postMode: bool,
    /// Whether to choose from moves with almost equal choices,
    /// where 0 moves "don't!" and any other value is a number of
    /// centipawns such that two moves count as "equal" if their scores
    /// differ at most that much.
    /// Note that when this is 0, the last move communicated by the
    /// strategy is taken no matter what the score is.
    pub oracleDiff: u32,
    /// Unique identifier supply for strategy, incremented with every
    /// strategy start, thus no strategy ever has a sid of 0
    pub sid: u32,
    /// Unique identifier for actually running strategy thread or 0 for
    /// none.
    pub running: u32,
    /// transposition table
    pub trtable: Arc<Mutex<HashMap<Position, Transp>>>,
    /// Opening Map
    pub openings: Arc<Mutex<OpeningMap>>,
    /// Simple transposition table
    pub strtab: Arc<Mutex<SimpleTranspositionHash>>,
    /// Indicator that the Transposition table may need a cleanup.
    /// This must be done when no strategy threads are running.
    /// The flag is set when the thinking decides that there is not
    /// enough time for a next iteration and at least 500ms are left
    /// over. But at this time, the strategy is most likely still
    /// active, as the shutdown occurs only when the state changes
    /// from THINKING to PLAYING (or FORCED). Hence we actually want
    /// to run this when we got a NoMore from a strategy.
    pub ttCleanup: bool,
}

impl GameState {
    /// initialize game and start the reader thread
    pub fn new(name: String) -> GameState {
        let (toMain, fromThreads) = mpsc::sync_channel(1);
        let (toReader, rdrRcv) = mpsc::sync_channel(1);
        // let (toStrategy, fromMain) = mpsc::sync_channel(1);
        let rdrSender = toMain.clone();
        thread::spawn(move || reader(rdrSender, rdrRcv));
        GameState {
            name,
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
            running: 0,
            trtable: Arc::new(Mutex::new(HashMap::new())),
            openings: Arc::new(Mutex::new(setupOpening())),
            strtab: Arc::new(Mutex::new(HashMap::new())),
            ponderMode: true,
            postMode: false,
            ttCleanup: false,
            oracleDiff: 5,
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

    /// Time for this move in milliseconds.
    ///
    /// The base for the time per move is the remaining time as told by
    /// xboard through the "time" command divided by 24 so as to
    /// last another 20 moves.
    ///
    /// If we are behind in time, that is, if the other player's time as
    /// told by the "otime" command is greater than our's, we
    /// decrease our time per move by an amount that leaves us at
    /// least 3 seconds.
    ///
    /// If we are ahead on time, we consume 2/3 of it.
    /// In addition, if there
    /// is an increment, that what exceeds 1 second will be added.
    pub fn timePerMove(&self) -> u64 {
        let ahead = self.myTime.as_millis() as i32 - self.oTime.as_millis() as i32;
        let tpm = self.myTime.as_millis() as i32 / 24; // raw time per move
        let incr = self.incrTime.as_millis() as i32;
        let remaining = if tpm < 4500 {
            2000 // it's getting tight, only 45s left
        } else if ahead < 0 {
            max(3000, tpm - 3000) + max(0, incr - 1000) // try to catch
                                                        // up 3s
        } else {
            // if ahead = 0 and incr = 0, it's still 1000
            max(1000, tpm) + 2 * ahead / 3 + max(0, incr - 1000)
        };
        assert!(remaining >= 1000);
        return remaining as u64;
    }

    /// Sets the stop thinking flag and tries to send the thread a final
    /// **false** command. This should end the thread after short time.
    ///
    /// It doesn't matter if there is no thread. The post-condition is:
    ///
    /// ```text
    /// computing::thinkingFinished() && self.toStrategy == None
    /// ```
    ///
    /// The `running` attribute is not reset by this method, it must be
    /// reset in the command processing.
    pub fn shutdownThread(&mut self) {
        computing::finishThinking();
        if let Some(sender) = &self.toStrategy {
            sender.try_send(false).unwrap_or_default();
            self.toStrategy = None;
        }
    }

    /// Check if the Mutex for the Transposition Table is locked
    pub fn ttIsLocked(&self) -> bool {
        match self.trtable.try_lock() {
            Ok(_) => false,
            Err(_) => true,
        }
    }

    /// Collect garbage. Remove all entries where halfmoves < limit,
    /// except when they are members of the history or opening moves
    /// (first ten moves)
    pub fn ttGarbage(&mut self) {
        match self.trtable.try_lock() {
            Ok(mut hash) => {
                let size = hash.len();
                let mut hist = HashSet::with_capacity(self.history.len());
                for p in &self.history {
                    hist.insert(p);
                }
                let mut n = size as i32 - 5_000_000;
                if size > 5_000_000 {
                    hash.retain(|p, t| {
                        n -= 1;
                        n < 0 || hist.contains(p) || t.halfmove < 50 || t.halfmove == u32::MAX
                    });
                    // hash.retain(|p, t| hist.contains(p) || t.halfmove
                    // >= limit && t.halfmove < 30);
                }
                self.ttCleanup = false;
                let size2 = hash.len();
                println!(
                    "# removed {} positions from transposition table with size {}",
                    size - size2,
                    size
                );
            }
            Err(_) => println!("# Can't lock the transposition table right now for cleanup."),
        }
    }

    /// Read a command from the threads and process it.
    /// Waits indefinitely for the next input, so may not be used while
    /// thinking.
    pub fn nextCommand(&mut self) -> State {
        let input = self.fromThreads.recv().unwrap();
        self.processCommand(&input)
    }

    /// Handle the `PLAYING` state.
    /// If it is our turn, we start a thread and change to `THINKING`
    /// Otherwise, if pondering is allowed and makes sense, we start
    /// pondering. Otherwise we wait for the next command.
    pub fn playing(&mut self, strategy: Strategy) -> State {
        if self.current().turn() == self.player {
            // it's our turn, nevertheless, we make sure there is no other request
            // at this time before starting the thread
            match self.fromThreads.try_recv() {
                // there is still input, process it first
                Ok(input) => self.processCommand(&input),
                // nothing on input channel, let's do something
                Err(mpsc::TryRecvError::Empty) => {
                    let pos = self.current();
                    let omv = {
                        let openings = self.openings.lock().unwrap();
                        match openings.get(&pos) {
                            None if pos.validEndgame() => match E::findEndgameMove(&pos) {
                                Ok(mv) => Some(mv),
                                Err(e) => {
                                    println!("# findEndgameMove: {}", e);
                                    None
                                }
                            },
                            None => None,
                            Some(recs) => {
                                let mut choices = Vec::new();
                                for r in recs {
                                    for _ in 0..r.ntimes {
                                        choices.push(r.mv);
                                    }
                                }
                                if choices.len() > 0 {
                                    println!("# match from opening book, choices are {}", P::showMoves(&choices[..]));
                                    let choice = rand::thread_rng().gen::<usize>() % choices.len();
                                    Some(choices[choice])
                                } else {
                                    // only forbidden moves
                                    None
                                }
                            }
                        }
                    };
                    // lock be gone!
                    match omv {
                        None => {
                            if self.running > 0 {
                                println!(
                                    "# Can't start thinking: Strategy{} is still running, waiting for more input.",
                                    self.running
                                );
                                self.nextCommand()
                            } else {
                                self.start(strategy, None)
                            }
                        }
                        Some(mv) => {
                            self.best = Some(Variation {
                                moves: [mv; VariationMoves],
                                length: 1,
                                nodes: 1,
                                depth: 1,
                                score: 0,
                            });
                            self.sendMove()
                        }
                    }
                }
                Err(mpsc::TryRecvError::Disconnected) => TERMINATED,
            }
        } else {
            match self.best {
                // should we ponder?
                Some(pv) if pv.length > 1 && self.ponderMode => {
                    let emove = pv.moves[pv.length as usize - 2];
                    let nextpos = self.current().apply(emove);
                    let nextmoves = nextpos.moves();
                    if self.running == 0 {
                        // check whether we will have an endgame move
                        let egmove = nextpos.validEndgame() && E::findEndgameMove(&nextpos).is_ok();
                        // does it make sense?
                        if !egmove && nextmoves.len() > 1 {
                            self.start(strategy, Some(emove))
                        } else {
                            self.nextCommand()
                        }
                    } else {
                        println!(
                            "# Can't start pondering: Strategy{} is still running, waiting for more input.",
                            self.running
                        );
                        self.nextCommand()
                    }
                }
                _other => self.nextCommand(),
            }
        }
    }

    /// Implements the thinking state
    pub fn thinking(&mut self, since: Instant, mbMove: Option<Move>) -> State {
        let pondering = if let Some(_) = mbMove { true } else { false };
        let used = since.elapsed().as_millis() as u64;
        let have = self.timePerMove();
        let time = if have < used { 500 } else { max(500, have - used) };
        let todo = if self.best.is_none() || pondering {
            // when we have no move yet, wait for one, no matter what
            // also, when we're pondering, we can just wait forever
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
                assert!(!pondering);
                println!("# Thinking timed out.");
                self.sendMove()
            }
            Ok(EOF) => {
                println!("# EOF on standard input while thinking!?");
                TERMINATED
            }
            Ok(Line(input)) => self.xboardCommand(input.trim()),
            Ok(MV(sid, var)) if sid == self.running => {
                let usedMillis = since.elapsed().as_millis() as u64;
                // make sure the next one is not tried if we have already used more
                // than 3/4 of the time.
                let timeOK = usedMillis <= (75 * self.timePerMove()) / 100;
                // stop strategy when we have a mate
                let mateIn1 = var.length == 1 && var.score == P::BLACK_IS_MATE;
                // should we go on?
                let goOn = pondering || timeOK && !mateIn1;
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
                if self.postMode {
                    match mbMove {
                        Some(mv) => println!(
                            " {} {} {} {} {}",
                            var.depth,
                            var.score,
                            (usedMillis + 5) / 10,
                            self.nodes,
                            var.push(mv).showMovesSAN(self.current())
                        ),
                        None => println!(
                            " {} {} {} {} {}",
                            var.depth,
                            var.score,
                            (usedMillis + 5) / 10,
                            self.nodes,
                            var.showMoves()
                        ),
                    }
                }
                io::stdout().flush().unwrap_or_default();
                // TODO: compute best
                let oracle: bool = rand::thread_rng().gen();
                let best = match &self.best {
                    _ if self.oracleDiff == 0 => {
                        println!(
                            "# taking newer move {}({})",
                            var.last().map(|m| m.algebraic()).unwrap_or("????".to_string()),
                            var.score
                        );
                        var
                    }
                    None => {
                        println!(
                            "# taking first move {}({})",
                            var.last().map(|m| m.algebraic()).unwrap_or("????".to_string()),
                            var.score
                        );
                        var
                    }
                    Some(old) => {
                        if var.length > 0 && old.length > 0 && var.last().unwrap() == old.last().unwrap()
                            || var.depth > old.depth
                        {
                            // println!("# var.length {} > 0  {}", var.length, var.length > 0);
                            // println!("# old.length {} > 0  {}", old.length, old.length > 0);
                            // println!(
                            //     "# var.moves.last {} == old.moves.last {}  {}",
                            //     var.last().unwrap(),
                            //     old.last().unwrap(),
                            //     var.last().unwrap() == old.last().unwrap()
                            // );
                            // println!(
                            //     "# var.depth {} > old.depth {}  {}",
                            //     var.depth,
                            //     old.depth,
                            //     var.depth > old.depth
                            // );
                            println!(
                                "# taking over deeper move {}({})",
                                var.last().map(|m| m.algebraic()).unwrap_or("????".to_string()),
                                var.score
                            );
                            var
                        } else if var.score.abs() < P::BLACK_IS_MATE - 1000
                            && (var.score - old.score).abs() <= self.oracleDiff as i32
                        {
                            if oracle {
                                println!(
                                    "# replacing because oracle {}({}) with {}({})",
                                    old.last().map(|m| m.algebraic()).unwrap_or("????".to_string()),
                                    old.score,
                                    var.last().map(|m| m.algebraic()).unwrap_or("????".to_string()),
                                    var.score
                                );
                                var
                            } else {
                                println!(
                                    "# keeping because oracle {}({}) with {}({})",
                                    old.last().map(|m| m.algebraic()).unwrap_or("????".to_string()),
                                    old.score,
                                    var.last().map(|m| m.algebraic()).unwrap_or("????".to_string()),
                                    var.score
                                );
                                old.clone()
                            }
                        } else if var.score > old.score {
                            println!(
                                "# replacing {}({}) with better {}({})",
                                old.last().map(|m| m.algebraic()).unwrap_or("????".to_string()),
                                old.score,
                                var.last().map(|m| m.algebraic()).unwrap_or("????".to_string()),
                                var.score
                            );
                            var
                        } else {
                            println!(
                                "# keeping {}({}) instead worse {}({})",
                                old.last().map(|m| m.algebraic()).unwrap_or("????".to_string()),
                                old.score,
                                var.last().map(|m| m.algebraic()).unwrap_or("????".to_string()),
                                var.score
                            );
                            old.clone()
                        }
                    }
                };
                self.best = Some(best);
                if goOn {
                    self.state
                } else {
                    self.ttCleanup |= usedMillis + 500 < self.timePerMove();
                    self.sendMove()
                }
            }
            Ok(NoMore(id)) if id == self.running && pondering => {
                println!("# Pondering strategy{} has ended unexpectedly.", self.running);
                self.shutdownThread();
                self.running = 0;
                self.state
            }
            Ok(NoMore(id)) if id == self.running && !pondering => {
                println!("# Strategy{} has no more moves.", id);
                self.running = 0;
                self.sendMove()
            }
            Ok(NoMore(u)) => {
                println!(
                    "# WHOA! Should not happen while {}: ignoring an unexpected NoMore from {}.",
                    self.state, u
                );
                self.state
            }
            Ok(MV(u, mv)) => {
                println!(
                    "# WHOA! SHould not happen while {}: ignoring an unexpected move sequence ({}) from {}.",
                    self.state,
                    mv.showMoves(),
                    u
                );
                self.state
            }
        }
    }

    /// This is the main loop of the protocol handler
    pub fn mainLoop(&mut self, strategy: Strategy)
    // where
    //    S: 'static + Strategy + Clone + Send,
    {
        while self.state != TERMINATED {
            let newstate = match self.state {
                TERMINATED => TERMINATED,
                FORCED => self.nextCommand(),
                PLAYING => self.playing(strategy),
                THINKING(since, mbMove) => self.thinking(since, mbMove),
            };
            match (self.state, newstate) {
                (TERMINATED, _) => (),
                (FORCED, _) => (),
                (PLAYING, FORCED) => {
                    self.best = None;
                    self.nodes = 0;
                }
                (PLAYING, _) => (),
                (THINKING(_, _), THINKING(_, _)) => (),
                (THINKING(_, _), PLAYING) => {
                    self.shutdownThread();
                }
                (THINKING(_, _), _) => {
                    self.shutdownThread();
                    self.best = None;
                    self.nodes = 0;
                } // _other => (),
            }
            if self.state != newstate {
                println!("# state changed from {} to {}", self.state, newstate);
            }
            self.state = newstate;
        }
    }

    /// Start a new thread that'll figure out a move
    pub fn start(&mut self, strategy: Strategy, expected: Option<Move>) -> State {
        let mvs = self.current().moves();
        let mate = mvs.len() == 0 && self.current().inCheck(self.current().turn());
        let stalemate = mvs.len() == 0 && !mate;

        if mate || stalemate {
            println!("# thinking finds game that has ended");
            io::stdout().flush().unwrap_or_default();
            FORCED
        } else {
            self.sid = self.sid + 1;
            let since = Instant::now();
            let (snd, rcv) = mpsc::sync_channel(1);
            let mut history = self.history.clone();
            if let Some(mv) = expected {
                // pretend the user already moved
                history.push(self.current().apply(mv));
                println!("Hint: {}", mv.showSAN(self.current()));
            }
            computing::beginThinking();
            let state = StrategyState {
                sid: self.sid,
                sender: self.toMain.clone(),
                receiver: rcv,
                history,
                trtable: Arc::clone(&self.trtable),
                openings: Arc::clone(&self.openings),
                strtab: Arc::clone(&self.strtab),
            };
            self.running = self.sid;
            self.best = None;
            self.nodes = 0;
            self.toStrategy = Some(snd);
            thread::spawn(move || strategy(state));
            println!("# Strategy{} spawned.", self.running);
            THINKING(since, expected)
        }
    }

    /// Send the best move, if any
    pub fn sendMove(&mut self) -> State {
        match self.best {
            None => {
                println!("# strategy busy, but no move found yet!");
                println!("resign");
                io::stdout().flush().unwrap_or_default();
                FORCED
            }
            Some(pv) => {
                assert!(pv.length > 0); // there must be moves
                let theMove = pv.last().unwrap();
                println!("move {}", theMove.algebraic());
                // println!("# ply counter before move: {}",
                // self.current().getPlyCounter());
                let pos = self.current().apply(theMove);
                // println!("# ply counter after  move: {}", pos.getPlyCounter());
                let ms = pos.moves();
                let mate = ms.len() == 0 && pos.inCheck(pos.turn());
                let stalemate = ms.len() == 0 && !mate;
                let moves50 = !mate && !stalemate && pos.getPlyCounter() > 100;
                let repetition =
                    !mate && !stalemate && !moves50 && self.history.iter().filter(|&p| *p == pos).count() >= 3;
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
                if repetition {
                    println!("1/2-1/2 {{repetition}}");
                }
                io::stdout().flush().unwrap_or_default();
                self.history.push(pos.clearRootPlyCounter());
                if finished {
                    FORCED
                } else {
                    PLAYING
                }
            }
        }
    }

    /// Processing of protocol commands
    pub fn processCommand(&mut self, input: &Protocol) -> State {
        match input {
            EOF => TERMINATED,
            Line(line) => self.xboardCommand(line.trim()),
            NoMore(u) => {
                if self.running == *u {
                    println!("# Strategy{} finally ended, it seems.", self.running);
                    self.running = 0;
                    if self.ttCleanup {
                        self.ttGarbage();
                    }
                    io::stdout().flush().unwrap_or_default();
                    self.state
                } else {
                    println!(
                        "# WHOA! should not happen while {}: ignoring an unexpected NoMore from {}.",
                        self.state, u
                    );
                    io::stdout().flush().unwrap_or_default();
                    self.state
                }
            }
            MV(u, mv) => {
                println!(
                    "# WHOA! should not happen while {}: ignoring an unexpected move sequence ({}) from {}.",
                    self.state,
                    mv.showMoves(),
                    u
                );
                io::stdout().flush().unwrap_or_default();
                self.state
            }
        }
    }

    /// Processing input from xboard
    pub fn xboardCommand(&mut self, cmd: &str) -> State {
        // tell the reader to continue unless we got the quit command
        self.toReader.send(cmd != "quit").unwrap_or_default();

        let mut iter = cmd.split_whitespace();
        let newstate = match iter.next() {
            None => self.state, // silently ignore empty line
            Some("quit") => TERMINATED,
            Some("accepted") | Some("rejected") | Some("xboard") | Some("random") | Some("computer")
            | Some("cores") | Some("st") | Some("sd") | Some("nps") => self.state, // ignored
            Some("protover") => {
                println!("feature myname=\"rasch {}\"", self.name);
                println!("feature ping=0 setboard=1 playother=1 usermove=1 draw=0");
                println!("feature sigint=0 analyze=1 variants=\"normal\" colors=0 nps=0");
                println!("feature debug=1 memory=0 smp=1 done=1");
                {
                    let hash = self.openings.lock().unwrap();
                    println!("# Opening table knows {} position.", hash.len());
                }
                self.state
            }
            Some("new") => {
                self.history = vec![P::initialBoard()];
                self.player = BLACK;
                // read the opening table again in case something has changed
                let hash = setupOpening();
                println!("# opening table size: {}", hash.len());
                self.openings = Arc::new(Mutex::new(hash));
                PLAYING
            }
            Some("playother") => {
                self.player = self.current().turn().opponent();
                PLAYING
            }
            Some("go") => {
                self.player = self.current().turn();
                PLAYING
            }
            Some("force") => FORCED,
            Some("setboard") => {
                let rest: Vec<_> = iter.collect();
                match fen::decodeFEN(&rest[..].join(" ")) {
                    Err(s) => println!("Error ({})", s),
                    Ok(p) => {
                        self.history = vec![p];
                    }
                }
                FORCED
            }
            Some("usermove") => match iter.next() {
                Some(alg) => match Move::unAlgebraic(&self.current().moves(), alg) {
                    Ok(mv) => {
                        self.history.push(self.current().apply(mv).clearRootPlyCounter());
                        match self.state {
                            PLAYING | FORCED => {
                                println!("# no pondering today?");
                                self.state
                            }
                            THINKING(_, Some(expected)) => {
                                if mv == expected {
                                    println!("# user played expected move {}", mv);
                                    // pondering complete?
                                    if self.running == 0 {
                                        println!("# pondering already complete");
                                        self.sendMove()
                                    } else if self.myTime.as_millis() < 4500 && self.best.is_some() {
                                        println!("# It's about time, take whatever we pondered so far!");
                                        self.sendMove()
                                    } else {
                                        // continue regular thinking
                                        // make it appear as if we just consumed 1/2 of our time
                                        THINKING(
                                            Instant::now().sub(Duration::from_millis(self.timePerMove() as u64 / 2)),
                                            None,
                                        )
                                    }
                                } else {
                                    PLAYING
                                }
                            }
                            _other => {
                                println!("Error (command not legal now): usermove");
                                println!("# got usermove in status {}", self.state);
                                PLAYING
                            }
                        }
                    }
                    Err(_) => {
                        println!("Illegal move: {}", alg);
                        FORCED
                    }
                },
                None => {
                    println!("Illegal move: ");
                    FORCED
                }
            },
            Some("result") => FORCED,
            Some("undo") => {
                if self.history.len() > 1 {
                    self.history.pop();
                }
                FORCED
            }
            // TODO: what happens when we are pondering?
            Some("remove") => {
                if self.history.len() > 2 {
                    self.history.pop();
                    self.history.pop();
                }
                self.state
            }
            Some("level") => {
                match iter.next() {
                    Some(_gamemoves) => match iter.next() {
                        Some(_gametime) => match iter.next() {
                            Some(incr) => match incr.parse::<u64>() {
                                Ok(n) => {
                                    self.incrTime = Duration::from_millis(n * 1000);
                                    println!("# increment of {}ms per move", self.incrTime.as_millis());
                                }
                                Err(_) => (),
                            },
                            None => (),
                        },
                        None => (),
                    },
                    None => (),
                }
                self.state
            }
            Some("time") => {
                match iter.next() {
                    Some(number) => match number.parse::<u64>() {
                        Ok(t) => self.myTime = Duration::from_millis(10 * t),
                        Err(_) => println!("Error (time not numeric)"),
                    },
                    None => println!("Error (number missing after time)"),
                }
                self.state
            }
            Some("otim") => {
                match iter.next() {
                    Some(number) => match number.parse::<u64>() {
                        Ok(t) => self.oTime = Duration::from_millis(10 * t),
                        Err(_) => println!("Error (time not numeric)"),
                    },
                    None => println!("Error (number missing after otim)"),
                }
                self.state
            }
            // "move now" command only in THINKING mode, but gets ignored if we haven't any move yet
            Some("?") => {
                if let THINKING(_, None) = self.state {
                    if let Some(_) = self.best {
                        self.sendMove()
                    } else {
                        println!("# cannot honour '?' command, I have no move yet.");
                        self.state
                    }
                } else {
                    println!("Error (command not legal now): ?");
                    self.state
                }
            }
            Some("hint") => match self.state {
                THINKING(_, Some(mv)) => {
                    println!("Hint: {}", mv);
                    self.state
                }
                THINKING(_, None) if self.best.is_some() => {
                    match self.best {
                        Some(pv) if pv.length > 1 => {
                            println!("Hint: {}", pv.moves[pv.length as usize - 2]);
                        }
                        _other => println!("Hint: e2e4"),
                    }
                    self.state
                }
                _other => {
                    println!("Error (command not legal now): hint");
                    self.state
                }
            },
            Some("post") => {
                self.postMode = true;
                self.state
            }
            Some("nopost") => {
                self.postMode = false;
                self.state
            }
            Some("hard") => {
                self.ponderMode = true;
                self.state
            }
            Some("easy") => {
                self.ponderMode = false;
                if let THINKING(_, Some(_)) = self.state {
                    PLAYING
                } else {
                    self.state
                }
            }
            Some(unknown) => {
                println!("Error (unknown command): {}", unknown);
                self.state
            }
        };
        // flush the output stream
        io::stdout().flush().unwrap_or_default();
        newstate
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
