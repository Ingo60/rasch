#![allow(uncommon_codepoints)]
#![allow(non_snake_case)]

use std::cmp::{max, min, Ordering};
use std::collections::{HashMap, HashSet};
use std::env::args;
use std::io;
use std::io::Write;
use std::sync::MutexGuard;
// use std::sync::{Arc, Mutex};
use std::time::Instant;

use rasch::common::GameState;
use rasch::{common, position::MoveCollector};
// use rasch::common::Protocol;
use rasch::common::Protocol::*;
// use rasch::common::StrategyState;
// use rasch::common::Variation;
use rasch::common::*;
use rasch::computing;
use rasch::fen::decodeFEN;
// use rasch::fieldset::*;
use rasch::mdb;
use rasch::position as P;
use rasch::position::Move;
use rasch::position::Piece::*;
// use rasch::position::Player;
use rasch::position::Position;
// use rasch::zobrist as Z;
use rasch::cposmove as CM;
use rasch::endgamedb as E;
use sysinfo::SystemExt;

fn main() {
    mdb::initStatic();
    let argv: Vec<String> = args().collect();
    if argv.len() == 1 {
        let mut gs = GameState::new(String::from("negamin"));
        gs.mainLoop(strategy_negamin);
        return;
    }
    let mut gs = GameState::new(String::from(argv[1].clone()));
    if argv[1].starts_with("flamegraph") {
        let n = if argv.len() >= 3 {
            match argv[2].parse::<u32>() {
                Ok(k) => k,
                Err(_) => 7,
            }
        } else {
            7
        };
        flamegraph(gs, n)
    } else if argv[1].starts_with("negamin") {
        gs.mainLoop(strategy_negamin)
    } else if argv[1].starts_with("negamax") {
        gs.mainLoop(strategy_negamax)
    } else if argv[1].starts_with("pvs") {
        gs.mainLoop(strategy_pvs)
    } else if argv[1].starts_with("simple") {
        gs.mainLoop(strategy_simple)
    } else if argv[1].starts_with("bns") {
        gs.oracleDiff = 0; // take no chances
        gs.mainLoop(strategy_bns)
    } else if argv[1].starts_with("mtdf") {
        gs.oracleDiff = 0; // take no chances
        gs.mainLoop(strategy_mtdf)
    } else if argv[1] == "sysinfo" {
        let mut info = sysinfo::System::new_all();
        info.refresh_memory();
        println!("We have {}M memory.", info.total_memory() / 1024);
    } else if argv[1] == "gen" && argv.len() >= 3 {
        match E::gen(&argv[2]) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
                std::process::exit(1)
            }
        }
    } else if argv[1] == "make" && argv.len() >= 3 {
        match E::make(&argv[2]) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
                std::process::exit(1)
            }
        }
    } else if argv[1] == "debug" && argv.len() > 2 {
        match E::debug(&argv[2..]) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
                std::process::exit(1)
            }
        }
    } else if argv[1] == "test2" && argv.len() >= 3 {
        match CM::test2(&argv[2]) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
                std::process::exit(1)
            }
        }
    } else if argv[1] == "test3" && argv.len() >= 3 {
        match CM::test3(&argv[2]) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
                std::process::exit(1)
            }
        }
    } else if argv[1] == "test4" {
        match CM::test4() {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
            }
        }
    } else if argv[1] == "test5" && argv.len() >= 3 {
        match CM::test5(&argv[2]) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
                std::process::exit(1)
            }
        }
    } else if argv[1] == "stats" && argv.len() >= 3 {
        match E::check_egtb(&argv[2]) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
                std::process::exit(1);
            }
        }
    } else if argv[1] == "check" && argv.len() >= 3 {
        match E::check_moves(&argv[2]) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
                std::process::exit(1);
            }
        }
    } else if argv[1] == "sane" && argv.len() >= 3 {
        match E::check_sane_via_moves(&argv[2]) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
                std::process::exit(1);
            }
        }
    } else if argv[1] == "dtm-fwd" && argv.len() >= 3 {
        match E::dtm_command(&argv[2], true) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
                std::process::exit(1);
            }
        }
    } else if argv[1] == "dtm-bck" && argv.len() >= 3 {
        match E::dtm_command(&argv[2], false) {
            Ok(_) => {}
            Err(s) => {
                eprintln!();
                eprintln!("error: {}", s);
                std::process::exit(1);
            }
        }
    } else if argv[1].starts_with("play") && argv.len() == 3 {
        match E::play(&String::from(argv[2].clone())) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
            }
        }
    } else if argv[1].starts_with("play") && argv.len() == 8 {
        let mut fen = String::from(argv[2].clone());
        for i in 3..8 {
            fen.push(' ');
            fen.push_str(&argv[i]);
        }
        match E::play(&fen) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
            }
        }
    } else if argv[1].starts_with("win") && argv.len() == 3 {
        match E::check_win(&String::from(argv[2].clone())) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
            }
        }
    } else if argv[1].starts_with("win") && argv.len() == 8 {
        let mut fen = String::from(argv[2].clone());
        for i in 3..8 {
            fen.push(' ');
            fen.push_str(&argv[i]);
        }
        match E::check_win(&fen) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
            }
        }
    } else if argv[1].starts_with("move") && argv.len() == 3 {
        match decodeFEN(&String::from(argv[2].clone())).and_then(|p| E::findEndgameMove(&p)) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
            }
        }
    } else if argv[1].starts_with("move") && argv.len() == 8 {
        let mut fen = String::from(argv[2].clone());
        for i in 3..8 {
            fen.push(' ');
            fen.push_str(&argv[i]);
        }
        match decodeFEN(&fen).and_then(|p| E::findEndgameMove(&p)) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
            }
        }
    } else {
        eprintln!("Illegal command line argument: `{}´", argv[1]);
        eprintln!(
            "Usage (e.g. as xboard client): \
            \n    {0} [negamin|negamax|pvs|bns|mtdf]     # default is `negamin`
            \nDeveloper tools:\
            \n    {0} flamegraph [N]    # used to get data for cargo flamegraph\
            \n    {0} gen     sig       # genereate end game table\
            \n    {0} make    sig       # make end game table after its predecessors\
            \n    {0} stats   sig       # check end game table & print statistics\
            \n    {0} check   sig       # check moves table\
            \n    {0} dtm-fwd sig       # make the DTM table and optimize moves\
            \n    {0} dtm-bck sig       # make the DTM table and optimize moves\
            \n    {0} play 'fen'        # simulate end game from position given in FEN notation\
            \n    {0} move 'fen'        # like \"play\", but only one move\
            \n    {0} win  'fen'        # list all winning variants for the given position\
            \n\
            \nThe directory where end game tables reside is given with environment variable EGTB.\
            \nDefault is ./egtb\
            \n",
            argv[0]
        );
    };
}

fn flamegraph(gs: GameState, depth: u32) {
    let mut hist = vec![P::initialBoard()];
    let mut hash = gs.trtable.lock().unwrap();
    let mut killers: KillerSet = [[P::NO_MOVE; 2]; 128];
    let before = Instant::now();
    let pv = negaMax(
        &mut hist,
        &mut hash,
        &mut killers,
        false,
        depth,
        P::WHITE_IS_MATE,
        P::BLACK_IS_MATE,
    );
    let usedMillis = before.elapsed().as_millis();
    println!(
        " {} {} {} {} {}",
        pv.depth,
        pv.score,
        (usedMillis + 5) / 10,
        pv.nodes,
        pv.showMovesSAN(P::initialBoard())
    );
    // hash.clear();
    killers = [[P::NO_MOVE; 2]; 128];
    let before = Instant::now();
    let pv = negaMax(
        &mut hist,
        &mut hash,
        &mut killers,
        false,
        depth,
        P::WHITE_IS_MATE,
        P::BLACK_IS_MATE,
    );
    let usedMillis = before.elapsed().as_millis();
    println!(
        " {} {} {} {} {}",
        pv.depth,
        pv.score,
        (usedMillis + 5) / 10,
        pv.nodes,
        pv.showMovesSAN(P::initialBoard())
    );
}

type TransTable<'x> = MutexGuard<'x, HashMap<Position, Transp>>;
type SimpleTransTable<'x> = MutexGuard<'x, SimpleTranspositionHash>;
type Positions = Vec<Position>;
type Variations = Vec<Variation>;
type KillerSet = [[Move; 2]; 128];

pub fn strategy_resign(mut state: StrategyState) {
    state.sender.send(NoMore(state.sid)).unwrap();
    state.sid = 0;
}

pub fn strategy_best(mut state: StrategyState) {
    let p = state.current();
    let moves = p.moves(); // This is a MoveList
    for mv in moves.iter().copied() {
        let p2 = p.apply(mv);
        state
            .sender
            .send(MV(
                state.sid,
                Variation {
                    depth: 1,
                    moves: [mv; VariationMoves as usize],
                    length: 1,
                    nodes: 1,
                    score: p2.eval(),
                },
            ))
            .unwrap();
        match state.receiver.recv() {
            Ok(true) => continue,
            _ => break,
        }
    }
    state.sender.send(NoMore(state.sid)).unwrap();
    // just to silence warnings
    state.history = vec![];
}

/// estimate a rating for a move
#[rustfmt::skip]
pub fn moveRating(pos: &Position, killers: &KillerSet, mv: Move) -> i32 {
    let piece = mv.piece();
    let target_piece = pos.pieceOn(mv.to());
    let ply = pos.getRootDistance() as usize;

    // 1. MVV-LVA (Most Valuable Victim - Least Valuable Attacker)
    // Wir bewerten Schläge basierend auf dem Wert des Ziels und der Angreifer
    if target_piece != P::Piece::EMPTY {
        return 10000 + (target_piece.score() * 10) - piece.score();
    }

    // 2. Killer Moves (Züge, die in anderen Zweigen zu Cutoffs geführt haben)
    if ply < 128 && (killers[ply][0] == mv || killers[ply][1] == mv) {
        return 5000;
    }

    // 3. Beförderungen
    if mv.promote() != P::Piece::EMPTY { return 8000; }

    0
}

/// order a bunch of moves so that the most useful one will processed
/// first
pub fn orderMoves(pos: &Position, killers: &mut KillerSet, ml: &mut P::MoveList) {
    let mut ratings = [0i32; P::MAX_MOVES];
    let moves = ml.as_mut_slice();
    for (i, &mv) in moves.iter().enumerate() {
        ratings[i] = moveRating(pos, &*killers, mv);
    }

    // Einfache In-Place Sortierung (Insertion Sort ist bei kleinen Listen oft schneller)
    for i in 1..moves.len() {
        let mut j = i;
        while j > 0 && ratings[j] > ratings[j - 1] {
            moves.swap(j, j - 1);
            ratings.swap(j, j - 1);
            j -= 1;
        }
    }
}

/// Correct a variation score of mate to slightly smaller value
/// so that shorter ways to mate have a better score than longer ones.
pub fn correctMateDistanceNo(var: &Variation) -> Variation {
    let dist = var.length as i32 * 2;
    let mut pv = *var;
    if pv.score == P::WHITE_IS_MATE {
        pv.score += dist
    } else if pv.score == P::BLACK_IS_MATE {
        pv.score -= dist
    } else {
    }
    pv
}

/// Find the bound of a score with integrated bounds.
/// - Equal indicates an exact score
/// - Less  indicates an upper bound, that is, real score is less
/// - Greater indicates a lower bound, that is, real score is greater
pub fn checkBound(n: i32) -> Ordering {
    if n & 3 == 0 {
        Ordering::Equal
    } else if (n - 1) & 3 == 0 {
        Ordering::Less
    } else {
        Ordering::Greater
    }
}

/// reconstruct the original score
pub fn boundedScore(n: i32) -> i32 {
    if n & 3 == 0 {
        n >> 2
    } else if (n - 1) & 3 == 0 {
        (n - 1) >> 2
    } else {
        (n + 1) >> 2
    }
}

/// Create an upper bound score.
pub fn upperBound(n: i32) -> i32 {
    4 * n - 1
}

/// Create a lower bound score.
pub fn lowerBound(n: i32) -> i32 {
    4 * n + 1
}

/// Create an exact score.
pub fn exactScore(n: i32) -> i32 {
    4 * n
}

/// Helper functin for negaMax
pub fn negaMaxGo(
    hist: &mut Positions, hash: &mut TransTable, killers: &mut KillerSet, ext: bool, depth: u32, alpha0: i32,
    beta: i32, moves: &P::MoveList,
) -> Variation {
    let mut best = Variation { nodes: 0, length: 0, moves: NONE, depth, score: -999_999_999 };
    let current = *hist.last().unwrap();
    let mut alpha = alpha0;
    for m in moves.as_slice().iter().copied() {
        let pos = current.apply(m);
        let capture = !ext
            && depth == 1
            && (m.promote() != EMPTY
                || current.inCheck(current.turn())
                || !current.isEmpty(m.to())
                || pos.inCheck(pos.turn()));
        let d = if capture { depth } else { depth - 1 };
        hist.push(pos);
        let pv = negaMax(hist, hash, killers, capture, d, -beta, -alpha);
        hist.pop();
        let score = -pv.score;
        if let Some(killer) = pv.last() {
            let ply = current.getRootDistance() as usize;
            if ply < 128 && killers[ply][0] != killer {
                killers[ply][1] = killers[ply][0];
                killers[ply][0] = killer;
            }
        }
        if score > beta {
            // killer move
            let mut killerpv = pv.push(m);
            killerpv.score = score;
            killerpv.nodes += best.nodes;
            return killerpv;
        }
        if score > alpha || score > best.score {
            best = Variation { nodes: best.nodes + pv.nodes, score, ..pv }.push(m);
        } else {
            best.nodes += pv.nodes;
        }
        alpha = max(score, alpha);
    }
    best
}

/// Helper function for PVS
pub fn pvsGo(
    hist: &mut Positions, hash: &mut TransTable, killers: &mut KillerSet, ext: bool, depth: u32, alpha0: i32,
    beta: i32, moves: &P::MoveList,
) -> Variation {
    let mut best = Variation { nodes: 0, length: 0, moves: NONE, depth, score: -999_999_999 };
    let current = *hist.last().unwrap();
    let mut alpha = alpha0;
    for m in moves.as_slice().iter().copied() {
        let first = best.score == -999_999_999;
        let nullWindow = !ext && !first && depth > 3;
        let pos = current.apply(m);
        let capture = !ext
            && depth == 1
            && (m.promote() != EMPTY
                || current.inCheck(current.turn())
                || !current.isEmpty(m.to())
                || pos.inCheck(pos.turn()));
        let d = if capture { depth } else { depth - 1 };
        hist.push(pos);
        let pv = if nullWindow {
            let pvx = pvsSearch(hist, hash, killers, capture, d, -alpha, 1 - alpha);
            if -pvx.score > best.score {
                best.nodes += pvx.nodes;
                pvsSearch(hist, hash, killers, capture, d, -beta, -alpha)
            } else {
                pvx
            }
        } else {
            pvsSearch(hist, hash, killers, capture, d, -beta, -alpha)
        };
        hist.pop();

        let score = -pv.score; // shorter ways are better

        if score > beta {
            // killer move
            let mut killerpv = pv.push(m);
            killerpv.score = score;
            killerpv.nodes += best.nodes;
            return killerpv;
        }

        if score > alpha || score > best.score {
            if let Some(killer) = pv.last() {
                let ply = current.getRootDistance() as usize;
                if ply < 128 && killers[ply][0] != killer {
                    killers[ply][1] = killers[ply][0];
                    killers[ply][0] = killer;
                }
            }
            best = Variation { nodes: best.nodes + pv.nodes, score, ..pv }.push(m);
        } else {
            best.nodes += pv.nodes;
        }
        alpha = max(score, alpha);
    }
    best
}

/// Helper function to insert a Variation into the hash
/// It is guaranteed that only variations with at least one move are
/// collected.
pub fn insertPV(
    hash: &mut TransTable, pos: Position, pv: Variation, depth: u32, alpha: i32, beta: i32, halfmove: u32,
) {
    if pv.score != 0
        && pv.length > 0
        && depth > 2
        && pv.score < P::BLACK_IS_MATE - 30
        && pv.score > P::WHITE_IS_MATE + 30
    {
        let bound = if pv.score >= beta {
            lowerBound(pv.score)
        } else if pv.score > alpha {
            exactScore(pv.score)
        } else {
            upperBound(pv.score)
        };
        let tr = common::Transp { halfmove, depth, score: bound, pvLength: pv.length, pvMoves: pv.moves };
        hash.insert(pos, tr);
    };
}

/// Quiescence Search: Continues searching capturing moves after depth 0
/// to avoid the horizon effect.
pub fn quiesce(
    hist: &mut Positions, hash: &mut TransTable, killers: &mut KillerSet, mut alpha: i32, beta: i32,
) -> Variation {
    let pos = *hist.last().unwrap();
    let stand_pat = pos.turn().factor() * pos.eval();

    // "Standing Pat" Score: Wenn die statische Bewertung schon gut genug für einen Cutoff ist
    if stand_pat >= beta {
        return Variation { score: stand_pat, nodes: 1, ..DRAW };
    }
    if alpha < stand_pat {
        alpha = stand_pat;
    }

    let mut best = Variation { score: stand_pat, nodes: 1, ..DRAW };
    let mut ml = pos.captures();
    orderMoves(&pos, killers, &mut ml);

    for m in ml.iter().copied() {
        let next_pos = pos.apply(m);
        hist.push(next_pos);
        let pv = quiesce(hist, hash, killers, -beta, -alpha);
        hist.pop();

        let score = -pv.score;
        best.nodes += pv.nodes;

        if score >= beta {
            return Variation { score, nodes: best.nodes, ..pv }.push(m);
        }
        if score > alpha {
            alpha = score;
            best = Variation { score, nodes: best.nodes, ..pv }.push(m);
        }
    }
    best
}

const NONE: [Move; VariationMoves] = [P::NO_MOVE; VariationMoves];
const DRAW: Variation = Variation { score: 0, nodes: 1, depth: 0, length: 0, moves: NONE };

pub type Search = fn(&mut Positions, hash: &mut TransTable, &mut KillerSet, bool, u32, i32, i32) -> Variation;

/// Move searching with NegaMax
pub fn negaMax(
    hist: &mut Positions, hash: &mut TransTable, killers: &mut KillerSet, ext: bool, depth: u32, alpha: i32,
    beta: i32,
) -> Variation {
    let pos = *hist.last().unwrap(); // the history must not be empty
    let halfmoves = hist.len() as u32;
    if depth > 2 && computing::thinkingFinished() {
        return DRAW;
    }
    if depth == 0 {
        return Variation { score: pos.turn().factor() * pos.eval(), ..DRAW };
    }
    // the follwoing is needed because else there is an immutable reference
    // to the hash
    let maybe: Option<common::Transp> = hash.get(&pos).copied();

    let rpv = match maybe {
        Some(te) => match checkBound(te.score) {
            Ordering::Equal if te.depth >= depth => {
                Variation { length: te.pvLength, moves: te.pvMoves, score: te.score >> 2, nodes: 1, depth }
            }

            Ordering::Less if te.depth >= depth => {
                let alpha2 = max(alpha, (te.score - 1) >> 2);
                if alpha2 > beta {
                    Variation { length: te.pvLength, moves: te.pvMoves, score: alpha2, nodes: 1, depth }
                } else {
                    let hashmove = te.pvMoves[(te.pvLength - 1) as usize];
                    let mut ml = pos.moves();
                    P::reorderHashMove(&mut ml, hashmove);

                    let pv = negaMaxGo(hist, hash, killers, ext, depth, alpha2, beta, &ml);
                    if depth >= te.depth && !ext {
                        insertPV(hash, pos, pv, depth, alpha2, beta, halfmoves);
                    };
                    pv
                }
            }

            Ordering::Greater if te.depth >= depth => {
                let beta2 = min(beta, (te.score + 1) >> 2);
                if alpha >= beta2 {
                    Variation { length: te.pvLength, moves: te.pvMoves, score: beta2, nodes: 1, depth }
                } else {
                    let hashmove = te.pvMoves[(te.pvLength - 1) as usize];
                    let mut ml = pos.moves();
                    P::reorderHashMove(&mut ml, hashmove);

                    let pv = negaMaxGo(hist, hash, killers, ext, depth, alpha, beta2, &ml);
                    if depth >= te.depth && !ext {
                        insertPV(hash, pos, pv, depth, alpha, beta2, halfmoves);
                    };
                    pv
                }
            }

            _other => {
                let hashmove = te.pvMoves[(te.pvLength - 1) as usize];
                let mut ml = pos.moves();
                P::reorderHashMove(&mut ml, hashmove);

                let pv = negaMaxGo(hist, hash, killers, ext, depth, alpha, beta, &ml);
                if !ext {
                    insertPV(hash, pos, pv, depth, alpha, beta, halfmoves);
                };
                pv
            }
        },
        None => {
            let mut ml = pos.moves();
            if depth > 1 {
                orderMoves(&pos, killers, &mut ml)
            }
            if ml.len == 0 {
                if pos.inCheck(pos.turn()) {
                    let mate = Variation {
                        score: P::WHITE_IS_MATE + (pos.getRootDistance() as i32 >> 1) * 3,
                        ..DRAW
                    };
                    mate
                } else {
                    DRAW
                }
            } else {
                let pv = negaMaxGo(hist, hash, killers, ext, depth, alpha, beta, &ml);
                if !ext {
                    insertPV(hash, pos, pv, depth, alpha, beta, halfmoves);
                };
                pv
            }
        }
    };
    // overwrite the evaluated score with zero if it is repetition or 50
    // moves rule
    if pos.getPlyCounter() >= 100 || hist[0..hist.len() - 1].contains(&pos) {
        Variation { score: 0, ..rpv }
    } else {
        rpv
    }
}

/// Iterative deepening for strategy negamin
pub fn negaSimple(state: StrategyState, killers: &mut KillerSet, depth: u32, alpha: i32, beta: i32) {
    let mut depth = depth;
    // let mut killers = HashSet::with_capacity(64);
    println!("# negaSimple{} depth {}", state.sid, depth);
    loop {
        let mut hist = state.history.clone();
        let mut pv = {
            // acquire mutable access to the transposition table
            // the hash is locked during search
            let mut hash: TransTable = state.trtable.lock().unwrap();
            // println!("# hash size: {}", hash.len());
            // let kvec: Vec<Move> = killers.iter().copied().collect();
            // println!(
            //     "# killer size: {} {}",
            //     killers.len(),
            //     P::showMoves(&kvec[0..min(8, killers.len())])
            // );
            negaMax(&mut hist, &mut hash, killers, false, depth, -beta, -alpha)
        };
        if computing::thinkingFinished() {
            break;
        }
        pv.score *= state.player().factor();
        pv.depth = depth;
        if state.talkPV(pv) {
            if pv.length + 1 < depth || pv.score >= P::BLACK_IS_MATE - 2 || pv.score == 0 {
                break;
            } else {
                depth += 1;
            }
        } else {
            break;
        }
    }
    state.tellNoMore();
}

/// search with the *negamin* algorithm
pub fn strategy_negamin(state: StrategyState) {
    let mut killers: KillerSet = [[P::NO_MOVE; 2]; 128];
    let mut allMoves = state.current().moves();
    orderMoves(&state.current(), &mut killers, &mut allMoves);
    println!(
        "# Started strategy negamin {}, we have {} moves to consider.",
        state.sid, allMoves.len
    );
    io::stdout().flush().unwrap_or_default();

    // if there's just 1 move left, we have no choice
    if allMoves.len == 0 {
        // are you kidding?
        state.tellNoMore();
    }
    if allMoves.len == 1 {
        if state.talkPV(Variation {
            depth: 1,
            nodes: 1,
            score: -9999,
            moves: [allMoves.moves[0]; VariationMoves as usize],
            length: 1,
        }) {
            state.tellNoMore();
        }
    } else {
        negaSimple(state, &mut killers, 3, P::WHITE_IS_MATE, P::BLACK_IS_MATE);
    }
}

/// Principal Variation Search
pub fn pvsSearch(
    hist: &mut Positions, hash: &mut TransTable, killers: &mut KillerSet, ext: bool, depth: u32, alpha: i32,
    beta: i32,
) -> Variation {
    let pos = *hist.last().unwrap(); // the history must not be empty
    let halfmoves = hist.len() as u32;
    if depth > 2 && computing::thinkingFinished() {
        return DRAW;
    }
    // This is the only point where we ever evaluate a position.
    // Nevertheless, it happens often, as this is at depth 0
    if depth == 0 {
        return quiesce(hist, hash, killers, alpha, beta);
    }
    // the follwoing is needed because else there is an immutable reference
    // to the hash
    let maybe: Option<common::Transp> = match hash.get(&pos) {
        Some(te) => Some(*te), // Transp is Copy now
        None => None,          // Can be simplified to hash.get(&pos).copied()
    };
    let rpv = match maybe {
        Some(te) => {
            let hashmove = te.pvMoves[(te.pvLength - 1) as usize];
            match checkBound(te.score) {
                Ordering::Equal if te.depth >= depth => Variation {
                    length: te.pvLength,
                    moves: te.pvMoves,
                    score: te.score >> 2,
                    nodes: 1,
                    depth,
                },

                Ordering::Less if te.depth >= depth => {
                    let alpha2 = max(alpha, (te.score - 1) >> 2);
                    if alpha2 > beta {
                        Variation { length: te.pvLength, moves: te.pvMoves, score: alpha2, nodes: 1, depth }
                    } else {
                        let mut ml = pos.moves();
                        P::reorderHashMove(&mut ml, hashmove);

                        let pv = pvsGo(hist, hash, killers, ext, depth, alpha2, beta, &ml);
                        if depth >= te.depth && !ext {
                            insertPV(hash, pos, pv, depth, alpha2, beta, halfmoves);
                        };
                        pv
                    }
                }

                Ordering::Greater if te.depth >= depth => {
                    let beta2 = min(beta, (te.score + 1) >> 2);
                    if alpha >= beta2 {
                        Variation { length: te.pvLength, moves: te.pvMoves, score: beta2, nodes: 1, depth }
                    } else {
                        let mut ml = pos.moves();
                        P::reorderHashMove(&mut ml, hashmove);

                        let pv = pvsGo(hist, hash, killers, ext, depth, alpha, beta2, &ml);
                        if depth >= te.depth && !ext {
                            insertPV(hash, pos, pv, depth, alpha, beta, halfmoves);
                        };
                        pv
                    }
                }

                _other => {
                    let mut ml = pos.moves();
                    P::reorderHashMove(&mut ml, hashmove);

                    let pv = pvsGo(hist, hash, killers, ext, depth, alpha, beta, &ml);
                    if !ext {
                        insertPV(hash, pos, pv, depth, alpha, beta, halfmoves);
                    };
                    pv
                }
            }
        }
        None => {
            let mut ml = pos.moves();
            if depth > 1 {
                orderMoves(&pos, killers, &mut ml)
            }
            if ml.len == 0 {
                if pos.inCheck(pos.turn()) {
                    let mate = Variation {
                        score: P::WHITE_IS_MATE + (pos.getRootDistance() as i32 >> 1) * 3,
                        ..DRAW
                    };
                    mate
                } else {
                    DRAW
                }
            } else {
                let pv = pvsGo(hist, hash, killers, ext, depth, alpha, beta, &ml);
                if !ext {
                    insertPV(hash, pos, pv, depth, alpha, beta, halfmoves);
                };
                pv
            }
        }
    };
    // overwrite the evaluated score with zero if it is repetition or 50
    // moves rule
    if pos.getPlyCounter() >= 100 || hist[0..hist.len() - 1].contains(&pos) {
        Variation { score: 0, ..rpv }
    } else {
        rpv
    }
}

pub fn iterDeep(state: StrategyState, depth: u32, search: Search) {
    let mut depth = depth;
    let mut killers = [[P::NO_MOVE; 2]; 128];

    let myPos = state.current(); // Copy of the current position
    let all_raw_moves = myPos.moves(); // Get the MoveList
    let mut pvs = Vec::with_capacity(all_raw_moves.len);
    let mut filtered_moves = P::MoveList::default();
    let mut current_iteration_moves: Vec<Move>; // Will hold moves for the current iteration

    let mut nodes = 0;

    // remove forbidden moves for this position
    let mut forbidden: HashSet<Move> = HashSet::new();
    {
        let openings = state.openings.lock().unwrap();
        match openings.get(&myPos) {
            None => (),
            Some(rs) => {
                for r in rs {
                    if r.ntimes == 0 {
                        forbidden.insert(r.mv);
                        println!("# excluded move: {}", r.mv.showSAN(myPos));
                    }
                }
            }
        }
    };
    for i in 0..all_raw_moves.len {
        let m = all_raw_moves.moves[i];
        if !forbidden.contains(&m) {
            filtered_moves.push(m);
        }
    }

    // for increasing depth
    loop
    /* forever! */
    {
        println!("# iterDeep{} depth {}", state.sid, depth);
        let myOrderedMoves = if pvs.len() == 0 {
            // First iteration: sort the initial filtered moves
            orderMoves(&myPos, &mut killers, &mut filtered_moves);
            filtered_moves.to_vec() // MoveList implements Deref<Target=[Move]>
        } else {
            // subsequent iterations, order the PVs by decreasing score and extract
            // our moves
            pvs.sort_unstable_by(|p1: &Variation, p2: &Variation| p2.score.cmp(&p1.score));
            // the unwrap should be safe as we pushed our move into the PV before
            pvs.iter().copied().map(|pv| pv.last().unwrap()).collect()
        };
        current_iteration_moves = myOrderedMoves; // Use this for the loop
        pvs.clear();
        println!(
            "# iterDeep{} we have {} ordered moves {}",
            state.sid,
            current_iteration_moves.len(),
            P::showMoves(&current_iteration_moves[..])
        );
        if current_iteration_moves.len() == 0 {
            state.tellNoMore();
            return;
        }
        let mut alpha = P::WHITE_IS_MATE; // Reset alpha for each depth iteration
                                          // for all moves in the current iteration
        for m in current_iteration_moves {
            let opos = myPos.apply(m);
            let mut hist = state.history.clone();
            hist.push(opos);
            let locking = Instant::now();
            let pv1 = {
                // acquire mutable access to the transposition table
                // the hash is locked during search
                let mut hash: TransTable = state.trtable.lock().unwrap();
                // println!("# hash size: {}", hash.len());
                // let kvec: Vec<Move> = killers.iter().copied().take(8).collect();
                // println!("# killer size: {} {}", killers.len(),
                // P::showMoves(&kvec[..]));
                let dur = locking.elapsed().as_millis();
                if dur > 1 {
                    println!(
                        "# negaDeep{}: it took only {}ms to lock the hash.",
                        state.sid, dur
                    );
                }
                search(
                    &mut hist,
                    &mut hash,
                    &mut killers,
                    false,
                    depth,
                    P::WHITE_IS_MATE,
                    -alpha + 6,
                )
            };
            if computing::thinkingFinished() {
                state.tellNoMore();
                return;
            }

            // show the result of the search
            // println!(
            //     "# after {}, search answers with depth:{} score:{} nodes:{} {}",
            //     m,
            //     pv0.depth,
            //     pv0.score,
            //     pv0.nodes,
            //     pv0.showMoves()
            // );

            // nodes += pv1.nodes;
            // the final version with our move pushed onto the end
            let pv = Variation { score: -pv1.score, depth: depth + 1, nodes: pv1.nodes + nodes + 1, ..pv1 }
                .push(m);
            // make sure good counter moves are treated as killers
            if let Some(killer) = pv1.last() {
                let ply = opos.getRootDistance() as usize - 1;
                if pv.score < alpha || pvs.len() == 0 {
                    if ply < 128 && killers[ply][0] != killer {
                        killers[ply][1] = killers[ply][0];
                        killers[ply][0] = killer;
                    }
                }
            }
            if pv.score >= alpha - 5 {
                if !state.talkPV(pv) {
                    println!("# iterDeep{} was asked to finish.", state.sid);
                    state.tellNoMore();
                    return;
                }
                nodes = 0;
            } else {
                nodes = pv.nodes;
            }
            alpha = max(pv.score, alpha);
            pvs.push(pv);
        }
        depth += 1;
    }
}

pub fn iterPVS(state: StrategyState, depth: u32) {
    let mut depth = depth;
    let mut killers: KillerSet = [[P::NO_MOVE; 2]; 128];
    let myPos = state.current(); // Copy of the current position
    let mut myMoves = myPos.moves(); // Get the MoveList
    let mut pvs: Variations = Vec::with_capacity(myMoves.len);
    let mut best = Variation { depth: 0, length: 0, moves: NONE, nodes: 0, score: P::WHITE_IS_MATE };
    let mut nodes = 0;
    let mut current_iteration_moves: Vec<Move>; // Will hold moves for the current iteration
                                                // for increasing depth
    loop
    /* forever! */
    {
        println!("# iterPVS{} depth {}", state.sid, depth);
        let myOrderedMoves = if pvs.len() == 0 {
            // First iteration: sort the initial moves
            orderMoves(&myPos, &mut killers, &mut myMoves);
            myMoves.as_slice().to_vec() // Convert to Vec for iteration
        } else {
            // subsequent iterations, order the PVs by decreasing score and extract
            // our moves
            pvs.sort_unstable_by(|p1, p2| p2.score.cmp(&p1.score));
            // the unwrap should be safe as we pushed our move into the PV before
            pvs.iter().copied().map(|pv| pv.last().unwrap()).collect()
        };
        current_iteration_moves = myOrderedMoves; // Use this for the loop
        pvs.clear();
        println!(
            "# iterPVS{} we have {} ordered moves {}",
            state.sid,
            current_iteration_moves.len(),
            P::showMoves(&current_iteration_moves[..])
        );
        if current_iteration_moves.len() == 0 {
            state.tellNoMore();
            return;
        }
        let mut alpha = P::WHITE_IS_MATE;
        // for all moves
        for m in current_iteration_moves {
            let opos = myPos.apply(m);
            let mut hist = state.history.clone();
            hist.push(opos);
            let locking = Instant::now();
            let pv1 = {
                // acquire mutable access to the transposition table
                // the hash is locked during search
                let mut hash: TransTable = state.trtable.lock().unwrap();
                // println!("# hash size: {}", hash.len());
                // let kvec: Vec<Move> = killers.iter().copied().take(8).collect();
                // println!("# killer size: {} {}", killers.len(),
                // P::showMoves(&kvec[..]));
                let dur = locking.elapsed().as_millis();
                if dur > 1 {
                    println!("# iterPVS{}: it took only {}ms to lock the hash.", state.sid, dur);
                }
                if pvs.len() == 0 {
                    // full width search

                    pvsSearch(
                        &mut hist,
                        &mut hash,
                        &mut killers,
                        false,
                        depth,
                        P::WHITE_IS_MATE,
                        P::BLACK_IS_MATE,
                    )
                } else {
                    let pvx = pvsSearch(
                        &mut hist,
                        &mut hash,
                        &mut killers,
                        false,
                        depth,
                        -alpha,
                        1 - alpha,
                    );
                    if computing::thinkingFinished() {
                        state.tellNoMore();
                        return;
                    }
                    if -pvx.score > alpha {
                        // re-search
                        // show the result of the search
                        println!(
                            "# iterPVS best so far {} {} {}  {}",
                            best.depth,
                            best.score,
                            best.nodes,
                            best.showMovesSAN(myPos)
                        );
                        println!(
                            "# iterPVS re-search   {} {} {}  {} {}",
                            pvx.depth,
                            -pvx.score,
                            pvx.nodes,
                            m.showSAN(myPos),
                            pvx.showMovesSAN(opos)
                        );
                        io::stdout().flush().unwrap_or_default();
                        nodes += pvx.nodes;
                        pvsSearch(
                            &mut hist,
                            &mut hash,
                            &mut killers,
                            false,
                            depth,
                            -(alpha + 6),
                            P::BLACK_IS_MATE,
                        )
                    } else {
                        pvx
                    }
                }
            };
            if computing::thinkingFinished() {
                state.tellNoMore();
                return;
            }

            // the final version with our move pushed onto the end
            let pv = Variation { score: -pv1.score, depth: depth + 1, nodes: pv1.nodes + nodes + 1, ..pv1 }
                .push(m);

            // make sure good counter moves are treated as killers
            if let Some(killer) = pv1.last() {
                let ply = opos.getRootDistance() as usize - 1;
                if pv.score < alpha || pvs.len() == 0 {
                    if ply < 128 && killers[ply][0] != killer {
                        killers[ply][1] = killers[ply][0];
                        killers[ply][0] = killer;
                    }
                }
            }
            if pvs.len() == 0 || pv.score > alpha {
                if !state.talkPV(pv) {
                    println!("# iterDeep{} was asked to finish.", state.sid);
                    state.tellNoMore();
                    return;
                }
                nodes = 0;
                best = pv;
            } else {
                nodes = pv.nodes;
            }
            alpha = max(pv.score, alpha);
            pvs.push(pv);
        }
        // don't go deeper if this is reasonably good
        if best.score > P::BLACK_IS_MATE - 30 {
            // let mut hash: TransTable = state.trtable.lock().unwrap();
            // let size = hash.len();
            // hash.retain(|_, t| t.score >= exactScore(P::blackIsMate - 30));
            // let size2 = hash.len();
            // println!(
            //     "# removed {} high score positions from transposition table with
            // size {}",     size - size2,
            //     size
            // );
            state.tellNoMore();
            return;
        }
        depth += 1;
    }
}

/// search with the *negamax* algorithm
pub fn strategy_negamax(state: StrategyState) {
    let allMoves = state.current().moves();
    println!(
        "# Started strategy{} negamax, we have {} moves to consider.",
        state.sid,
        allMoves.len()
    );
    io::stdout().flush().unwrap_or_default();

    // if there's just 1 move left, we have no choice
    if allMoves.len() == 1 {
        state.talkPV(Variation {
            depth: 1,
            nodes: 1,
            score: -9999,
            moves: [allMoves[0]; VariationMoves as usize],
            length: 1,
        });
        state.tellNoMore();
    } else {
        iterDeep(state, 3, negaMax);
    }
}

/// apply pribcipal variation search
pub fn strategy_pvs(state: StrategyState) {
    let current = state.current();
    let allMoves = current.moves();
    println!(
        "# Started strategy{} PVS, we have {} moves to consider.",
        state.sid,
        allMoves.len()
    );
    io::stdout().flush().unwrap_or_default();

    // if there's just 1 move left, we have no choice
    if allMoves.len() == 1 {
        state.talkPV(Variation {
            depth: 1,
            nodes: 1,
            score: -9999,
            moves: [allMoves[0]; VariationMoves as usize],
            length: 1,
        });
        state.tellNoMore();
    } else {
        if state.history.len() > 2 && !current.inEndgame() {
            // users position before his move
            let usrPos = state.history[state.history.len() - 2];
            match usrPos
                .moves()
                .iter()
                .copied()
                .find(|m| usrPos.apply(*m) == current)
            {
                Some(userMv) => match state.trtable.try_lock() {
                    Ok(hash) => match hash.get(&usrPos) {
                        Some(tr)
                            if tr.halfmove < u32::MAX && tr.pvLength > 0 && userMv != tr.pvMoves[0]
                                || usrPos.moves().iter().all(|&m| userMv != m) =>
                        {
                            if tr.halfmove < u32::MAX {
                                println!(
                                    "# learning: user moved {} instead of hash move {}",
                                    userMv.showSAN(usrPos),
                                    tr.pvMoves[0].showSAN(usrPos)
                                );
                            } else {
                                println!("# learning: user moved new variant {}", userMv.showSAN(usrPos));
                            }
                        }
                        Some(_) => {
                            println!(
                                "# learning: We knew already that {} may be played here.",
                                userMv.showSAN(usrPos)
                            );
                        }
                        None => {
                            println!("# learning: unknown position");
                        }
                    },
                    Err(_) => println!("# learning: Can't lock transposition table now."),
                },
                None => {
                    println!("# learning: position must have been edited");
                }
            };
        }
        iterPVS(state, 3);
    }
}

/// search with a variant of the *negamax* algorithm that uses simple
/// transposition tables
pub fn strategy_simple(state: StrategyState) {
    let allMoves = state.current().moves();
    println!(
        "# Started strategy{} simple, we have {} moves to consider.",
        state.sid,
        allMoves.len()
    );
    io::stdout().flush().unwrap_or_default();

    // if there's just 1 move left, we have no choice
    if allMoves.len() == 1 {
        state.talkPV(Variation {
            depth: 1,
            nodes: 1,
            score: -9999,
            moves: [allMoves[0]; VariationMoves as usize],
            length: 1,
        });
        state.tellNoMore();
    } else {
        iterSimple(state, 3);
    }
}

pub fn iterSimple(state: StrategyState, depth: u32) {
    let mut depth = depth;
    let mut killers: KillerSet = [[P::NO_MOVE; 2]; 128];
    let myPos = state.current(); // Copy of the current position
    let mut myMoves = myPos.moves(); // Get the MoveList
    let mut pvs: Variations = Vec::with_capacity(myMoves.len);
    let mut nodes = 0;
    let mut current_iteration_moves: Vec<Move>; // Will hold moves for the current iteration
                                                // for increasing depth
    loop
    /* forever! */
    {
        println!("# iterSimple{} depth {}", state.sid, depth);
        let myOrderedMoves = if pvs.len() == 0 {
            // First iteration: sort the initial moves
            orderMoves(&myPos, &mut killers, &mut myMoves);
            myMoves.as_slice().to_vec() // Convert to Vec for iteration
        } else {
            // subsequent iterations, order the PVs by decreasing score and extract
            // our moves
            pvs.sort_unstable_by(|p1, p2| p2.score.cmp(&p1.score));
            // the unwrap should be safe as we pushed our move into the PV before
            pvs.iter().copied().map(|pv| pv.last().unwrap()).collect()
        };
        current_iteration_moves = myOrderedMoves; // Use this for the loop
        pvs.clear();
        println!(
            "# iterSimple{} we have {} ordered moves {}",
            state.sid,
            current_iteration_moves.len(),
            P::showMoves(&current_iteration_moves[..])
        );
        if current_iteration_moves.len() == 0 {
            state.tellNoMore();
            return;
        }
        let mut alpha = P::WHITE_IS_MATE;
        // for all moves
        for m in current_iteration_moves {
            let opos = myPos.apply(m);
            let mut hist = state.history.clone();
            hist.push(opos);
            let locking = Instant::now();
            let pv1 = {
                // acquire mutable access to the transposition table
                // the hash is locked during search
                let mut hash: SimpleTransTable = state.strtab.lock().unwrap();
                // println!("# hash size: {}", hash.len());
                // let kvec: Vec<Move> = killers.iter().copied().take(8).collect();
                // println!("# killer size: {} {}", killers.len(),
                // P::showMoves(&kvec[..]));
                let dur = locking.elapsed().as_millis();
                if dur > 1 {
                    println!(
                        "# iterSimple{}: it took only {}ms to lock the hash.",
                        state.sid, dur
                    );
                }
                if pvs.len() == 0 {
                    hash.retain(|p, t| {
                        hist.contains(p) || t.halfmove >= (state.history.len() as u32) || t.halfmove < 20
                    });
                }
                simpleMax(
                    &mut hist,
                    &mut hash,
                    &mut killers,
                    false,
                    depth,
                    P::WHITE_IS_MATE,
                    P::BLACK_IS_MATE,
                )
            };
            if computing::thinkingFinished() {
                state.tellNoMore();
                return;
            }

            // show the result of the search
            // println!(
            //     "# after {}, search answers with depth:{} score:{} nodes:{} {}",
            //     m,
            //     pv0.depth,
            //     pv0.score,
            //     pv0.nodes,
            //     pv0.showMoves()
            // );

            // nodes += pv1.nodes;
            // the final version with our move pushed onto the end
            let pv = Variation { score: -pv1.score, depth: depth + 1, nodes: pv1.nodes + nodes + 1, ..pv1 }
                .push(m);
            // make sure good counter moves are treated as killers
            if let Some(killer) = pv1.last() {
                if pv.score < alpha || pvs.len() == 0 {
                    let ply = opos.getRootDistance() as usize - 1;
                    if ply < 128 && killers[ply][0] != killer {
                        killers[ply][1] = killers[ply][0];
                        killers[ply][0] = killer;
                    }
                }
            }
            if pv.score >= alpha - 5 {
                if !state.talkPV(pv) {
                    println!("# iterSimple{} was asked to finish.", state.sid);
                    state.tellNoMore();
                    return;
                }
                nodes = 0;
            } else {
                nodes = pv.nodes;
            }
            alpha = max(pv.score, alpha);
            pvs.push(pv);
        }
        if alpha > P::BLACK_IS_MATE - 10 {
            println!("# iterSimple{} opponent almost mated.", state.sid);
            state.tellNoMore();
        }
        depth += 1;
    }
}

/* pub fn simpleLookup(pos: Position, hash: &SimpleTransTable) -> Variation {
    let mut hist: HashSet<Position> = HashSet::new();
    let mut moves: Vec<Move> = Vec::with_capacity(VariationMoves);
    let mut pv = Variation {
        length: 0,
        moves:  NONE,
        score:  0,
        nodes:  1,
        depth:  0,
    };
    let mut current = pos;
    loop {
        if hist.contains(&current) {
            // without this, we face the danger to loop forever, especially in
            // endgames
            pv.score = pos.turn().factor() * current.eval();
            break;
        }
        match hash.get(&current) {
            None => {
                pv.score = pos.turn().factor() * current.eval();
                break;
            }
            Some(te) => {
                // if let Some(mv) = te.mv {
                hist.insert(current);
                current = current.apply(te.mv);
                pv.depth += 1;
                if moves.len() < VariationMoves {
                    moves.push(te.mv)
                }
                // } else {
                //     pv.score = pos.turn().factor() * te.score;
                //     pv.score = if pv.score >= 0 {
                //         pv.score - 3 * pv.depth as i32
                //     } else {
                //         pv.score + 3 * pv.depth as i32
                //     };
                //     break;
                // }
            }
        }
    }
    // copy moves from vector into pv
    let mut dst: usize = 0;
    let mut src: usize = min(VariationMoves, moves.len());
    while src > 0 {
        src -= 1;
        pv.moves[dst] = moves[src];
        dst += 1;
    }
    pv.length = dst as u32;
    pv
    // pv.from_iter(&mut moves.into_iter())
}
*/

/// Move searching with SimpleTransTable
pub fn simpleMax(
    hist: &mut Positions, hash: &mut SimpleTransTable, killers: &mut KillerSet, ext: bool, depth: u32,
    alpha0: i32, beta: i32,
) -> Variation {
    let pos = *hist.last().unwrap(); // the history must not be empty
    if depth > 2 && computing::thinkingFinished() {
        return DRAW;
    }
    let maybeTE = hash.get(&pos);
    let ordered = match maybeTE {
        Some(te) => {
            let hashmove = te.pv.last().unwrap();
            let mut aux: P::MoveList = P::MoveList { moves: [P::NO_MOVE; P::MAX_MOVES], len: 0 };
            aux.push(hashmove);
            // te.moves is a MoveList, need to iterate its slice
            for &m in te.moves.as_slice() {
                if m != hashmove {
                    aux.push(m);
                }
            }
            // if we have a match, insert the answer to the proposed move as killer
            if te.pv.length > 1 {
                let ply = pos.getRootDistance() as usize;
                if ply < 128 {
                    killers[ply][0] = te.pv.moves[(te.pv.length - 2) as usize];
                }
            }
            aux
        }
        None => {
            let moves = pos.moves();
            let mut ml = moves; // Make it mutable for orderMoves
            if depth > 1 {
                orderMoves(&pos, killers, &mut ml);
                ml // Return the sorted MoveList
            } else {
                ml // Return the original MoveList
            }
        }
    };

    // mate or draw checking
    if ordered.len() == 0 {
        if pos.inCheck(pos.turn()) {
            let mate = Variation { score: P::WHITE_IS_MATE + (pos.getRootDistance() as i32), ..DRAW };
            return mate;
        } else {
            return DRAW;
        }
    }

    let mut best = match maybeTE {
        Some(te) if te.pv.depth >= depth => Variation { nodes: 1, score: te.pv.score, ..te.pv },
        _other => Variation { nodes: 0, length: 0, moves: NONE, depth: 0, score: -999_999_999 },
    };

    if depth == 0 {
        Variation { score: pos.eval_have_moves(&ordered) * pos.turn().factor(), ..DRAW }
    } else {
        let current = pos;
        let mut alpha = max(alpha0, best.score);

        for m in ordered.iter().copied() {
            let pos = current.apply(m);
            let capture = !ext
                && depth == 1
                && (m.promote() != EMPTY
                    || current.inCheck(current.turn())
                    || !current.isEmpty(m.to())
                    || pos.inCheck(pos.turn()));
            let d = if capture { depth } else { depth - 1 };
            hist.push(pos);
            let pv = simpleMax(hist, hash, killers, capture, d, -beta, -alpha);
            hist.pop();
            // farther away moves are less worth, prefer the shorter variant
            let score = -pv.score;
            if let Some(killer) = pv.last() {
                let ply = pos.getRootDistance() as usize;
                if ply < 128 && killers[ply][0] != killer {
                    killers[ply][1] = killers[ply][0];
                    killers[ply][0] = killer;
                }
            }
            if score > beta || score > alpha || score > best.score {
                best = Variation { nodes: best.nodes + pv.nodes, depth: pv.depth + 1, score, ..pv }.push(m);
                if score > beta {
                    break;
                }
            } else {
                best.nodes += pv.nodes;
            }
            alpha = max(score, alpha);
        }
        // overwrite the evaluated score with zero if it is repetition or 50
        // moves rule
        // if we have found a move, record it in the hash table
        if let Some(bestmove) = best.last() {
            let npos = pos.apply(bestmove);
            if npos.getPlyCounter() > 100 || hist[0..hist.len() - 1].contains(&npos) {
                best.score = 0;
            } else if depth > 2 && best.depth > 0 && best.length > 0 && best.score != 0 {
                hash.insert(
                    current,
                    SimpleTransp {
                        halfmove: hist.len() as u32, // This is fine
                        moves: ordered,
                        pv: Variation { nodes: 1, ..best },
                    },
                );
            };
        }
        best
    }
}

/// best node search strategy
/// https://www.bjmc.lu.lv/fileadmin/user_upload/lu_portal/projekti/bjmc/Contents/770_7.pdf
pub fn strategy_bns(state: StrategyState) {
    let pos = state.current();
    let allMoves = pos.moves();
    println!(
        "# Started strategyBNS{}, we have {} moves to consider.",
        state.sid,
        allMoves.len()
    );
    io::stdout().flush().unwrap_or_default();

    // if there's just 1 move left, we have no choice
    if allMoves.len() == 1 {
        state.talkPV(Variation {
            depth: 1,
            nodes: 1,
            score: -9999,
            moves: [allMoves[0]; VariationMoves as usize],
            length: 1,
        });
        state.tellNoMore();
    } else if allMoves.len() == 0 {
        state.tellNoMore();
    } else {
        iterBNS(state);
    }
}

pub fn iterBNS(state: StrategyState) {
    let mut depth = 4;
    let mut killers: KillerSet = [[P::NO_MOVE; 2]; 128];
    let pos = state.current();
    let score0 = pos.eval() * pos.turn().factor();
    let mut beta = score0 + 50;
    let mut alpha = score0 - 50;
    let mut all_raw_moves = pos.moves(); // Get MoveList
    let mut pvs: Vec<Variation> = Vec::with_capacity(all_raw_moves.len);
    let mut nodes = 0;
    let mut best = DRAW;
    let mut current_iteration_moves: Vec<Move>; // Will hold moves for the current iteration
                                                // for increasing depth
    while alpha < beta
    /* && pvs.len() != 1 */
    {
        current_iteration_moves = if pvs.len() > 0 {
            pvs.iter().copied().map(|pv| pv.last().unwrap()).collect()
        } else {
            orderMoves(&pos, &mut killers, &mut all_raw_moves); // Sort in-place
            all_raw_moves.to_vec() // Convert to Vec for iteration
        };

        // Die `current_iteration_moves` Variable ist nun korrekt als Vec<Move> typisiert
        // und wird für die Ausgabe und Iteration verwendet.
        println!(
            "# iterBNS{} depth {}, score {}, alpha {}, beta {}, {} {} ",
            state.sid,
            depth,
            score0,
            alpha,
            beta,
            if pvs.len() > 0 { "variations" } else { "moves" },
            current_iteration_moves.len(),
        );

        println!(
            "# iterBNS{} we have {} ordered moves {}",
            state.sid,
            current_iteration_moves.len(),
            P::showMoves(&current_iteration_moves[..])
        );

        let mut hash: TransTable = state.trtable.lock().unwrap();
        // hash.clear();

        pvs.clear();
        best.score = 2 * P::WHITE_IS_MATE;
        // for all moves
        for m in current_iteration_moves {
            let opos = pos.apply(m);
            let mut hist = state.history.clone();
            hist.push(opos);

            let pv1 = mtdfMax(&mut hist, &mut hash, &mut killers, false, depth, -beta, -alpha);
            if computing::thinkingFinished() {
                println!("# iterBNS{} thinking finished.", state.sid);
                state.tellNoMore();
                return;
            }
            nodes += pv1.nodes;

            // nodes += pv1.nodes;
            // the final version with our move pushed onto the end
            let pv = Variation { score: -pv1.score, depth: depth, nodes, ..pv1 }.push(m);

            // show the result of the search
            println!(
                "# iterBNS{}:  after {}, search answers with depth:{} score:{} nodes:{} {}",
                state.sid,
                m.showSAN(pos),
                pv.depth,
                pv.score,
                pv.nodes,
                pv.showMovesSAN(pos)
            );

            // make sure good counter moves are treated as killers
            if let Some(killer) = pv1.last() {
                if pv.score < alpha || pvs.len() == 0 {
                    let ply = opos.getRootDistance() as usize - 1;
                    if ply < 128 && killers[ply][0] != killer {
                        killers[ply][1] = killers[ply][0];
                        killers[ply][0] = killer;
                    }
                }
            }

            if pv.score >= alpha {
                pvs.push(pv);
                if pv.score > best.score || pvs.len() == 1 {
                    if !state.talkPV(pv) {
                        println!("# iterBNS{} was asked to finish.", state.sid);
                        // TODO: choose among the variations we have so far
                        state.tellNoMore();
                        return;
                    }
                    best = pv;
                }
                beta += pv.score - alpha;
                alpha = pv.score;
            }

            if pv.score > best.score {
                best = pv;
            }
        } // for all subtrees
        if pvs.len() > 0 {
            // subsequent iterations, order the PVs by decreasing score and extract
            // our moves.
            pvs.sort_unstable_by(|p1, p2| p2.score.cmp(&p1.score));
            depth += 1;
            alpha += 10;
            beta += 10;
        } else {
            println!(
                "# iterBNS{} no sub trees made it, widening search window",
                state.sid
            );
            alpha -= 50;
            beta += 50;
        }
    } // while loop
    println!("# iterBNS{} this is good enough.", state.sid);
    state.tellNoMore();
    return;
}

/// Move searching with NegaMax for BNS
pub fn mtdfMax(
    hist: &mut Positions, hash: &mut TransTable, killers: &mut KillerSet, ext: bool, depth: u32, alpha: i32,
    beta: i32,
) -> Variation {
    let pos = *hist.last().unwrap(); // the history must not be empty
    let halfmoves = hist.len() as u32;
    if depth > 2 && computing::thinkingFinished() {
        return DRAW;
    }

    // the follwoing is needed because else there is an immutable reference
    // to the hash
    let maybe: Option<common::Transp> = hash.get(&pos).copied();

    let ml = match maybe {
        None => {
            let mut moves = pos.moves();
            if depth > 0 {
                orderMoves(&pos, killers, &mut moves);
            }
            moves
        }
        Some(te) => {
            let hashmove = te.pvMoves[(te.pvLength - 1) as usize];
            let mut moves = pos.moves();
            P::reorderHashMove(&mut moves, hashmove);
            moves
        }
    };
    if depth == 0 {
        return Variation { score: pos.turn().factor() * pos.eval(), ..DRAW };
    };
    if ml.len == 0 {
        if pos.inCheck(pos.turn()) {
            return Variation { score: P::WHITE_IS_MATE + (pos.getRootDistance() as i32), ..DRAW };
        } else {
            return DRAW;
        }
    };
    match hash.get(&pos) {
        Some(te) if te.depth >= depth => {
            let score = boundedScore(te.score);
            if checkBound(te.score) == Ordering::Equal
                || checkBound(te.score) == Ordering::Less && score <= alpha
                || checkBound(te.score) == Ordering::Greater && score >= beta
            {
                return Variation { length: te.pvLength, moves: te.pvMoves, score, nodes: 1, depth };
            };
        }
        _otherwise => {}
    };
    let mut best = Variation { nodes: 0, length: 0, moves: NONE, depth, score: -999_999_999 };
    let current = pos;
    let mut a = alpha;
    for m in ml.iter().copied() {
        if best.score > beta {
            break;
        };
        let pos = current.apply(m);
        let capture = !ext
            && depth == 1
            && (m.promote() != EMPTY
                || current.inCheck(current.turn())
                || !current.isEmpty(m.to())
                || pos.inCheck(pos.turn()));
        let d = if capture { depth } else { depth - 1 };
        hist.push(pos);
        let pv = mtdfMax(hist, hash, killers, capture, d, -beta, -a);
        hist.pop();
        let score = -pv.score;
        if let Some(killer) = pv.last() {
            if score < alpha {
                let ply = current.getRootDistance() as usize;
                if ply < 128 && killers[ply][0] != killer {
                    killers[ply][1] = killers[ply][0];
                    killers[ply][0] = killer;
                }
            }
        }
        if score > best.score {
            best = Variation { nodes: best.nodes + pv.nodes, score, depth, ..pv }.push(m);
        } else {
            best.nodes += pv.nodes;
        }
        a = max(best.score, a);
    }
    if !ext {
        insertPV(hash, pos, best, depth, a, beta, halfmoves);
    };
    // overwrite the evaluated score with zero if it is repetition or 50
    // moves rule
    if pos.getPlyCounter() >= 100 || hist[0..hist.len() - 1].contains(&pos) {
        Variation { score: 0, ..best }
    } else {
        best
    }
}

/// MTD(f) strategy
/// https://arxiv.org/pdf/1505.01603.pdf
pub fn strategy_mtdf(state: StrategyState) {
    let pos = state.current();
    let allMoves = pos.moves();
    println!(
        "# Started strategyMTDF{}, we have {} moves to consider.",
        state.sid,
        allMoves.len()
    );
    io::stdout().flush().unwrap_or_default();

    // if there's just 1 move left, we have no choice
    if allMoves.len() == 1 {
        state.talkPV(Variation {
            depth: 1,
            nodes: 1,
            score: -9999,
            moves: [allMoves[0]; VariationMoves as usize],
            length: 1,
        });
        state.tellNoMore();
    } else if allMoves.len() == 0 {
        state.tellNoMore();
    } else {
        let mut depth = 4;
        let mut killers: KillerSet = [[P::NO_MOVE; 2]; 128];
        let mut hist = state.history.clone();
        let mut g = {
            let mut hash: TransTable = state.trtable.lock().unwrap();
            mtdfMax(
                &mut hist,
                &mut hash,
                &mut killers,
                false,
                3,
                P::WHITE_IS_MATE,
                P::BLACK_IS_MATE,
            )
            .score
        };
        let pos = state.current();
        loop {
            let mut f_high = P::BLACK_IS_MATE;
            let mut f_low = P::WHITE_IS_MATE;
            // state.trtable.lock().unwrap().clear();
            while f_low < f_high {
                println!(
                    "# iterMTDF{} continuing with f_low {}, f_high {}",
                    state.sid, f_low, f_high
                );

                let guess = if g == f_low { g + 1 } else { g };
                let pv = {
                    let mut hash: TransTable = state.trtable.lock().unwrap();
                    mtdfMax(&mut hist, &mut hash, &mut killers, false, depth, guess - 1, guess)
                };
                if computing::thinkingFinished() {
                    println!("# iterMTDF{} thinking finished.", state.sid);
                    state.tellNoMore();
                    return;
                }
                println!(
                    "# iterMTDF{}:  with guess {} search answers with depth:{} score:{} nodes:{} {}",
                    state.sid,
                    guess,
                    pv.depth,
                    pv.score,
                    pv.nodes,
                    pv.showMovesSAN(pos)
                );
                g = pv.score;
                if g < guess {
                    f_high = g
                } else {
                    f_low = g
                };
                if !state.talkPV(pv) {
                    println!("# iterBNS{} was asked to finish.", state.sid);
                    // TODO: choose among the variations we have so far
                    state.tellNoMore();
                    return;
                }
            }
            depth += 1;
        }
    }
}
