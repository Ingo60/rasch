#![allow(non_snake_case)]

use std::cmp::{max, min, Ordering};
use std::collections::{HashMap, HashSet};
use std::env::args;
use std::io;
use std::io::Write;
use std::sync::MutexGuard;
// use std::sync::{Arc, Mutex};
use std::time::Instant;

use rasch::common;
use rasch::common::GameState;
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
use rasch::sortegtb as S;
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
        E::debug(&argv[2..]);
    } else if argv[1] == "test1" && argv.len() >= 3 {
        match CM::test1(&argv[2]) {
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
    } else if argv[1] == "test3" && argv.len() == 8 {
        let mut fen = String::from(argv[2].clone());
        for i in 3..8 {
            fen.push(' ');
            fen.push_str(&argv[i]);
        }
        match decodeFEN(&fen).and_then(|p| CM::test3(&p)) {
            Ok(_) => {}
            Err(s) => {
                eprintln!("error: {}", s);
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
    } else if argv[1] == "dtm" && argv.len() >= 3 {
        match E::dtm_command(&argv[2]) {
            Ok(_) => {}
            Err(s) => {
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
    } else if argv[1].starts_with("sort") && argv.len() >= 3 {
        match S::sort(&String::from(argv[2].clone())) {
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
            \n    {0} gen  sig          # genereate end game table\
            \n    {0} make sig          # make end game table\
            \n    {0} stats sig         # check end game table & print statistics\
            \n    {0} check sig         # check moves table\
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
    let mut killers = HashSet::with_capacity(64);
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
    killers.clear();
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
type KillerSet = HashSet<Move>;

pub fn strategy_resign(mut state: StrategyState) {
    state.sender.send(NoMore(state.sid)).unwrap();
    state.sid = 0;
}

pub fn strategy_best(mut state: StrategyState) {
    let p = state.current();
    let moves = p.moves();
    for mv in moves {
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
pub fn moveRating(pos: &Position, killers: &mut KillerSet, mv: Move) -> i32 {
    let rpos = pos.apply(mv);
    let before = pos.pieceOn(mv.from());
    let after = rpos.pieceOn(mv.to());
    let vt = pos.pieceOn(mv.to()).score();
    let killer = if killers.contains(&mv)                                   { 11 } else { 0 };
    let goodCapture = if mv.piece().score() < vt - 10                       {  9 } else { 0 };
    let badCapturing = if vt > 0                                            {  2 } else { 0 };
    let castling = if mv.piece() == KING && mv.promote() != EMPTY           {  3 } else { 0 };
    let checking = if rpos.inCheck(rpos.turn())                             {  1 } else { 0 };
    let attacked = if rpos.isAttacked(mv.to(), rpos.turn())                 { -2 } else { 0 };
    let hanging  = if rpos.isAttacked(mv.to(), pos.turn()) && attacked != 0 { -5 } else { 0 };
    
    let attacking_before = (P::pieceTargets(before, mv.player(), mv.from()) 
                            * pos.occupiedBy(mv.player().opponent())).card() as i32;
    
    let attacking_after  = (P::pieceTargets(after, mv.player(), mv.to()) 
                            * rpos.occupiedBy(mv.player().opponent())).card() as i32;
    let attacking = attacking_after - attacking_before;
    let pawnMove =  if rpos.inEndgame() && mv.piece() == PAWN {
                        if mv.promote() != EMPTY                  { 10 } else { 5 }
                    } else { 0 }; 
    
    goodCapture + badCapturing + castling + checking + attacking + pawnMove + killer + attacked + hanging
}

/// order a bunch of moves so that the most useful one will processed
/// first
pub fn orderMoves(pos: &Position, killers: &mut KillerSet, moves: &[Move]) -> Vec<Move> {
    let ratings = moves.iter().copied().map(|mv| moveRating(pos, killers, mv));
    let mut tuples = ratings.zip(moves.iter().copied()).collect::<Vec<_>>();
    tuples.sort_unstable_by(|(r1, _), (r2, _)| r2.cmp(r1)); // descending

    tuples.iter().copied().map(|(_, m)| m).collect()
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
    hist: &mut Positions,
    hash: &mut TransTable,
    killers: &mut KillerSet,
    ext: bool,
    depth: u32,
    alpha0: i32,
    beta: i32,
    moves: &Vec<Move>,
) -> Variation {
    let mut best = Variation { nodes: 0, length: 0, moves: NONE, depth, score: -999_999_999 };
    let current = *hist.last().unwrap();
    let mut alpha = alpha0;
    for m in moves.iter().copied() {
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
            killers.insert(killer);
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
    hist: &mut Positions,
    hash: &mut TransTable,
    killers: &mut KillerSet,
    ext: bool,
    depth: u32,
    alpha0: i32,
    beta: i32,
    moves: &[Move],
) -> Variation {
    let mut best = Variation { nodes: 0, length: 0, moves: NONE, depth, score: -999_999_999 };
    let current = *hist.last().unwrap();
    let mut alpha = alpha0;
    for &m in moves.iter() {
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
                killers.insert(killer);
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
    hash: &mut TransTable,
    pos: Position,
    pv: Variation,
    moves: Vec<Move>,
    depth: u32,
    alpha: i32,
    beta: i32,
    halfmove: u32,
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
        let tr = common::Transp {
            halfmove,
            depth,
            score: bound,
            pvLength: pv.length,
            pvMoves: pv.moves,
            posMoves: moves,
        };
        hash.insert(pos, tr);
    };
}

const NONE: [Move; VariationMoves] = [P::NO_MOVE; VariationMoves];
const DRAW: Variation = Variation { score: 0, nodes: 1, depth: 0, length: 0, moves: NONE };

pub type Search = fn(&mut Positions, hash: &mut TransTable, &mut KillerSet, bool, u32, i32, i32) -> Variation;

/// Move searching with NegaMax
pub fn negaMax(
    hist: &mut Positions,
    hash: &mut TransTable,
    killers: &mut KillerSet,
    ext: bool,
    depth: u32,
    alpha: i32,
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
        let epv = Variation { score: pos.turn().factor() * pos.eval(), ..DRAW };
        return epv;
    }
    // the follwoing is needed because else there is an immutable reference
    // to the hash
    let maybe: Option<common::Transp> = match hash.get(&pos) {
        Some(te) => Some(te.clone()),
        None => None,
    };
    let rpv = match maybe {
        Some(te) => {
            let hashmove = te.pvMoves[(te.pvLength - 1) as usize];
            let mut ordered: Vec<Move> = Vec::with_capacity(te.posMoves.len());
            ordered.push(hashmove);
            ordered.extend(te.posMoves.iter().filter(|&&m| m != hashmove));
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
                        let pv = negaMaxGo(hist, hash, killers, ext, depth, alpha2, beta, &ordered);
                        if depth >= te.depth && !ext {
                            insertPV(hash, pos, pv, ordered, depth, alpha2, beta, halfmoves);
                        };
                        pv
                    }
                }

                Ordering::Greater if te.depth >= depth => {
                    let beta2 = min(beta, (te.score + 1) >> 2);
                    if alpha >= beta2 {
                        Variation { length: te.pvLength, moves: te.pvMoves, score: beta2, nodes: 1, depth }
                    } else {
                        let pv = negaMaxGo(hist, hash, killers, ext, depth, alpha, beta2, &ordered);
                        if depth >= te.depth && !ext {
                            insertPV(hash, pos, pv, ordered, depth, alpha, beta2, halfmoves);
                        };
                        pv
                    }
                }

                _other => {
                    let pv = negaMaxGo(hist, hash, killers, ext, depth, alpha, beta, &ordered);
                    if !ext {
                        insertPV(hash, pos, pv, ordered, depth, alpha, beta, halfmoves);
                    };
                    pv
                }
            }
        }
        None => {
            let moves = pos.moves();
            let ordered = if depth > 1 { orderMoves(&pos, killers, &moves[..]) } else { moves };
            if ordered.len() == 0 {
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
                let pv = negaMaxGo(hist, hash, killers, ext, depth, alpha, beta, &ordered);
                if !ext {
                    insertPV(hash, pos, pv, ordered, depth, alpha, beta, halfmoves);
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
    let mut killers = HashSet::with_capacity(2048);
    let allMoves = orderMoves(&state.current(), &mut killers, &state.current().moves());
    println!(
        "# Started strategy negamin {}, we have {} moves to consider.",
        state.sid,
        allMoves.len()
    );
    io::stdout().flush().unwrap_or_default();

    // if there's just 1 move left, we have no choice
    if allMoves.len() == 0 {
        // are you kidding?
        state.tellNoMore();
    }
    if allMoves.len() == 1 {
        if state.talkPV(Variation {
            depth: 1,
            nodes: 1,
            score: -9999,
            moves: [allMoves[0]; VariationMoves as usize],
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
    hist: &mut Positions,
    hash: &mut TransTable,
    killers: &mut KillerSet,
    ext: bool,
    depth: u32,
    alpha: i32,
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
        let epv = Variation { score: pos.turn().factor() * pos.eval(), ..DRAW };
        return epv;
    }
    // the follwoing is needed because else there is an immutable reference
    // to the hash
    let maybe: Option<common::Transp> = match hash.get(&pos) {
        Some(te) => Some(te.clone()),
        None => None,
    };
    let rpv = match maybe {
        Some(te) => {
            let hashmove = te.pvMoves[(te.pvLength - 1) as usize];
            let mut ordered: Vec<Move> = Vec::with_capacity(te.posMoves.len());
            ordered.push(hashmove);
            ordered.extend(te.posMoves.iter().filter(|&&m| m != hashmove));
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
                        let pv = pvsGo(hist, hash, killers, ext, depth, alpha2, beta, &ordered);
                        if depth >= te.depth && !ext {
                            insertPV(hash, pos, pv, ordered, depth, alpha2, beta, halfmoves);
                        };
                        pv
                    }
                }

                Ordering::Greater if te.depth >= depth => {
                    let beta2 = min(beta, (te.score + 1) >> 2);
                    if alpha >= beta2 {
                        Variation { length: te.pvLength, moves: te.pvMoves, score: beta2, nodes: 1, depth }
                    } else {
                        let pv = pvsGo(hist, hash, killers, ext, depth, alpha, beta2, &ordered);
                        if depth >= te.depth && !ext {
                            insertPV(hash, pos, pv, ordered, depth, alpha, beta2, halfmoves);
                        };
                        pv
                    }
                }

                _other => {
                    let pv = pvsGo(hist, hash, killers, ext, depth, alpha, beta, &ordered);
                    if !ext {
                        insertPV(hash, pos, pv, ordered, depth, alpha, beta, halfmoves);
                    };
                    pv
                }
            }
        }
        None => {
            let moves = pos.moves();
            let ordered = if depth > 1 { orderMoves(&pos, killers, &moves[..]) } else { moves };
            if ordered.len() == 0 {
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
                let pv = pvsGo(hist, hash, killers, ext, depth, alpha, beta, &ordered);
                if !ext {
                    insertPV(hash, pos, pv, ordered, depth, alpha, beta, halfmoves);
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
    let mut killers = HashSet::with_capacity(4096);
    let myPos = state.current();
    let myMoves = myPos.moves();
    let mut pvs: Variations = Vec::with_capacity(myMoves.len());
    let mut nodes = 0;
    // for increasing depth
    loop
    /* forever! */
    {
        println!("# iterDeep{} depth {}", state.sid, depth);
        let myOrderedMoves = if pvs.len() == 0 {
            // first iteration
            orderMoves(&myPos, &mut killers, &myMoves[..])
        } else {
            // subsequent iterations, order the PVs by decreasing score and extract
            // our moves
            pvs.sort_unstable_by(|p1, p2| p2.score.cmp(&p1.score));
            // the unwrap should be safe as we pushed our move into the PV before
            pvs.iter().copied().map(|pv| pv.last().unwrap()).collect()
        };
        pvs.clear();
        println!(
            "# iterDeep{} we have {} ordered moves {}",
            state.sid,
            myOrderedMoves.len(),
            P::showMoves(&myOrderedMoves[..])
        );
        if myOrderedMoves.len() == 0 {
            state.tellNoMore();
            return;
        }
        let mut alpha = P::WHITE_IS_MATE;
        // for all moves
        for m in myOrderedMoves {
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
                if pv.score < alpha || pvs.len() == 0 {
                    killers.insert(killer);
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
    let mut killers = HashSet::with_capacity(4096);
    let myPos = state.current();
    let myMoves = myPos.moves();
    let mut pvs: Variations = Vec::with_capacity(myMoves.len());
    let mut best = Variation { depth: 0, length: 0, moves: NONE, nodes: 0, score: P::WHITE_IS_MATE };
    let mut nodes = 0;
    // for increasing depth
    loop
    /* forever! */
    {
        println!("# iterPVS{} depth {}", state.sid, depth);
        let myOrderedMoves = if pvs.len() == 0 {
            // first iteration
            orderMoves(&myPos, &mut killers, &myMoves[..])
        } else {
            // subsequent iterations, order the PVs by decreasing score and extract
            // our moves
            pvs.sort_unstable_by(|p1, p2| p2.score.cmp(&p1.score));
            // the unwrap should be safe as we pushed our move into the PV before
            pvs.iter().copied().map(|pv| pv.last().unwrap()).collect()
        };
        pvs.clear();
        println!(
            "# iterPVS{} we have {} ordered moves {}",
            state.sid,
            myOrderedMoves.len(),
            P::showMoves(&myOrderedMoves[..])
        );
        if myOrderedMoves.len() == 0 {
            state.tellNoMore();
            return;
        }
        let mut alpha = P::WHITE_IS_MATE;
        // for all moves
        for m in myOrderedMoves {
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
                if pv.score < alpha || pvs.len() == 0 {
                    killers.insert(killer);
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
                            if tr.halfmove < u32::MAX && tr.pvMoves.len() > 0 && userMv != tr.pvMoves[0]
                                || tr.posMoves.iter().all(|m| userMv != *m) =>
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
    let mut killers = HashSet::with_capacity(4096);
    let myPos = state.current();
    let myMoves = myPos.moves();
    let mut pvs: Variations = Vec::with_capacity(myMoves.len());
    let mut nodes = 0;
    // for increasing depth
    loop
    /* forever! */
    {
        println!("# iterSimple{} depth {}", state.sid, depth);
        let myOrderedMoves = if pvs.len() == 0 {
            // first iteration
            orderMoves(&myPos, &mut killers, &myMoves[..])
        } else {
            // subsequent iterations, order the PVs by decreasing score and extract
            // our moves
            pvs.sort_unstable_by(|p1, p2| p2.score.cmp(&p1.score));
            // the unwrap should be safe as we pushed our move into the PV before
            pvs.iter().copied().map(|pv| pv.last().unwrap()).collect()
        };
        pvs.clear();
        println!(
            "# iterSimple{} we have {} ordered moves {}",
            state.sid,
            myOrderedMoves.len(),
            P::showMoves(&myOrderedMoves[..])
        );
        if myOrderedMoves.len() == 0 {
            state.tellNoMore();
            return;
        }
        let mut alpha = P::WHITE_IS_MATE;
        // for all moves
        for m in myOrderedMoves {
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
                    killers.insert(killer);
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
    hist: &mut Positions,
    hash: &mut SimpleTransTable,
    killers: &mut KillerSet,
    ext: bool,
    depth: u32,
    alpha0: i32,
    beta: i32,
) -> Variation {
    let pos = *hist.last().unwrap(); // the history must not be empty
    if depth > 2 && computing::thinkingFinished() {
        return DRAW;
    }
    let maybeTE = hash.get(&pos);
    let ordered = match maybeTE {
        Some(te) => {
            let hashmove = te.pv.last().unwrap();
            let mut aux: Vec<Move> = Vec::with_capacity(te.moves.len());
            aux.push(hashmove);
            aux.extend(te.moves.iter().copied().filter(|&m| m != hashmove));
            // if we have a match, insert the answer to the proposed move as killer
            if te.pv.length > 1 {
                killers.insert(te.pv.moves[(te.pv.length - 2) as usize]);
            }
            aux
        }
        None => {
            let moves = pos.moves();
            if depth > 1 {
                orderMoves(&pos, killers, &moves[..])
            } else {
                moves
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
                killers.insert(killer);
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
                        halfmove: hist.len() as u32,
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
    let mut killers = HashSet::with_capacity(4096);
    let pos = state.current();
    let score0 = pos.eval() * pos.turn().factor();
    let mut beta = score0 + 50;
    let mut alpha = score0 - 50;
    let moves = pos.moves();
    let mut pvs: Vec<Variation> = Vec::with_capacity(moves.len());
    let mut nodes = 0;
    let mut best = DRAW;
    // for increasing depth
    while alpha < beta
    /* && pvs.len() != 1 */
    {
        let ordered = if pvs.len() > 0 {
            pvs.iter().copied().map(|pv| pv.last().unwrap()).collect()
        } else {
            orderMoves(&pos, &mut killers, &moves[..])
        };

        println!(
            "# iterBNS{} depth {}, score {}, alpha {}, beta {}, {} {} ",
            state.sid,
            depth,
            score0,
            alpha,
            beta,
            if pvs.len() > 0 { "variations" } else { "moves" },
            ordered.len(),
        );

        println!(
            "# iterBNS{} we have {} ordered moves {}",
            state.sid,
            ordered.len(),
            P::showMoves(&ordered[..])
        );

        let mut hash: TransTable = state.trtable.lock().unwrap();
        // hash.clear();

        pvs.clear();
        best.score = 2 * P::WHITE_IS_MATE;
        // for all moves
        for m in ordered {
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
                    killers.insert(killer);
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
    hist: &mut Positions,
    hash: &mut TransTable,
    killers: &mut KillerSet,
    ext: bool,
    depth: u32,
    alpha: i32,
    beta: i32,
) -> Variation {
    let pos = *hist.last().unwrap(); // the history must not be empty
    let halfmoves = hist.len() as u32;
    if depth > 2 && computing::thinkingFinished() {
        return DRAW;
    }

    // the follwoing is needed because else there is an immutable reference
    // to the hash
    let maybe: Option<common::Transp> = match hash.get(&pos) {
        Some(te) => Some(te.clone()),
        None => None,
    };
    let moves = match maybe {
        None => {
            if depth == 0 {
                Vec::with_capacity(0)
            } else {
                orderMoves(&pos, killers, &(pos.moves())[..])
            }
        }
        Some(te) => {
            let hashmove = te.pvMoves[(te.pvLength - 1) as usize];
            let mut ordered: Vec<Move> = Vec::with_capacity(te.posMoves.len());
            ordered.push(hashmove);
            ordered.extend(te.posMoves.iter().filter(|&&m| m != hashmove));
            ordered
        }
    };
    if depth == 0 {
        return Variation { score: pos.turn().factor() * pos.eval(), ..DRAW };
    };
    if moves.len() == 0 {
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
    for m in moves.iter().copied() {
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
                killers.insert(killer);
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
        insertPV(hash, pos, best, moves, depth, a, beta, halfmoves);
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
        let mut killers = HashSet::with_capacity(4096);
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
