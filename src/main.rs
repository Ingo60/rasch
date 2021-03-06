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
// use rasch::fen;
use rasch::computing;
// use rasch::fieldset::*;
use rasch::mdb;
use rasch::position as P;
use rasch::position::Move;
use rasch::position::Piece::*;
// use rasch::position::Player;
use rasch::position::Position;
// use rasch::zobrist as Z;

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
    } else {
        eprintln!("Illegal command line argument: `{}´", argv[1]);
        eprintln!("Usage: {} [flamegraph [N]|negamin|negamax|pvs]", argv[0]);
        eprintln!("The default is `negamin´");
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
        P::whiteIsMate,
        P::blackIsMate,
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
    hash.clear();
    killers.clear();
    let before = Instant::now();
    let pv = pvsSearch(
        &mut hist,
        &mut hash,
        &mut killers,
        false,
        depth,
        P::whiteIsMate,
        P::blackIsMate,
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
                    depth:  1,
                    moves:  [mv; VariationMoves as usize],
                    length: 1,
                    nodes:  1,
                    score:  p2.eval(),
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
    let killer = if killers.contains(&mv)                         { 11 } else { 0 };
    let goodCapture = if mv.piece().score() < vt - 10             {  9 } else { 0 };
    let badCapturing = if vt > 0                                  {  2 } else { 0 };
    let castling = if mv.piece() == KING && mv.promote() != EMPTY {  3 } else { 0 };
    let checking = if rpos.inCheck(rpos.turn())                   {  1 } else { 0 };
    
    let attacking_before = (P::pieceTargets(before, mv.player(), mv.from()) 
                            * pos.occupiedBy(mv.player().opponent())).card() as i32;
    
    let attacking_after  = (P::pieceTargets(after, mv.player(), mv.to()) 
                            * rpos.occupiedBy(mv.player().opponent())).card() as i32;
    let attacking = attacking_after - attacking_before;
    let pawnMove =  if rpos.inEndgame() && mv.piece() == PAWN {
                        if mv.promote() != EMPTY                  { 10 } else { 5 }
                    } else { 0 }; 
    
    goodCapture + badCapturing + castling + checking + attacking + pawnMove + killer
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
    if pv.score == P::whiteIsMate {
        pv.score += dist
    } else if pv.score == P::blackIsMate {
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

/// Create an upper bound score.
pub fn upperBound(n: i32) -> i32 { 4 * n - 1 }

/// Create a lower bound score.
pub fn lowerBound(n: i32) -> i32 { 4 * n + 1 }

/// Create an exact score.
pub fn exactScore(n: i32) -> i32 { 4 * n }

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
    let mut best = Variation {
        nodes: 0,
        length: 0,
        moves: NONE,
        depth,
        score: -999_999_999,
    };
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
            best = Variation {
                nodes: best.nodes + pv.nodes,
                score,
                ..pv
            }
            .push(m);
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
    let mut best = Variation {
        nodes: 0,
        length: 0,
        moves: NONE,
        depth,
        score: -999_999_999,
    };
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
            best = Variation {
                nodes: best.nodes + pv.nodes,
                score,
                ..pv
            }
            .push(m);
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
    if pv.score != 0 && pv.length > 0 && depth > 2 {
        let bound = if pv.score >= beta {
            lowerBound(pv.score)
        } else if pv.score >= alpha {
            exactScore(pv.score)
        } else {
            upperBound(pv.score)
        };
        let tr = common::Transp {
            halfmove,
            depth,
            score: bound,
            pvMoves: pv.moves(),
            posMoves: moves,
        };
        hash.insert(pos, tr);
    };
}

const NONE: [Move; VariationMoves] = [P::noMove; VariationMoves];

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
    #[rustfmt::skip]
    let draw = Variation {
        score: 0, nodes: 1, depth, length: 0, moves: NONE,
    };
    if depth > 2 && computing::thinkingFinished() {
        return draw;
    }
    // This is the only point where we ever evaluate a position.
    // Nevertheless, it happens often, as this is at depth 0
    if depth == 0 {
        let epv = Variation {
            score: pos.turn().factor() * pos.eval(),
            ..draw
        };
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
            let hashmove = te.pvMoves[0];
            let mut ordered: Vec<Move> = Vec::with_capacity(te.posMoves.len());
            ordered.push(hashmove);
            ordered.extend(te.posMoves.iter().filter(|&&m| m != hashmove));
            match checkBound(te.score) {
                Ordering::Equal if te.depth >= depth => {
                    let found = Variation {
                        length: 0,
                        moves: NONE,
                        score: te.score >> 2,
                        nodes: 1,
                        depth,
                    }
                    .from_iter(&mut te.pvMoves.into_iter());
                    found
                }

                Ordering::Less if te.depth >= depth => {
                    let alpha2 = max(alpha, (te.score - 1) >> 2);
                    if alpha2 > beta {
                        Variation {
                            length: 0,
                            moves: NONE,
                            score: alpha2,
                            nodes: 1,
                            depth,
                        }
                        .from_iter(&mut te.pvMoves.into_iter())
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
                        Variation {
                            length: 0,
                            moves: NONE,
                            score: beta2,
                            nodes: 1,
                            depth,
                        }
                        .from_iter(&mut te.pvMoves.into_iter())
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
            let ordered = if depth > 1 {
                orderMoves(&pos, killers, &moves[..])
            } else {
                moves
            };
            if ordered.len() == 0 {
                if pos.inCheck(pos.turn()) {
                    let mate = Variation {
                        score: P::whiteIsMate + (pos.getRootDistance() as i32 >> 1) * 3,
                        ..draw
                    };
                    mate
                } else {
                    draw
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
            if pv.length + 1 < depth || pv.score >= P::blackIsMate - 2 || pv.score == 0 {
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
            depth:  1,
            nodes:  1,
            score:  -9999,
            moves:  [allMoves[0]; VariationMoves as usize],
            length: 1,
        }) {
            state.tellNoMore();
        }
    } else {
        negaSimple(state, &mut killers, 3, P::whiteIsMate, P::blackIsMate);
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
    #[rustfmt::skip]
    let draw = Variation {
        score: 0, nodes: 1, depth, length: 0, moves: NONE,
    };
    if depth > 2 && computing::thinkingFinished() {
        return draw;
    }
    // This is the only point where we ever evaluate a position.
    // Nevertheless, it happens often, as this is at depth 0
    if depth == 0 {
        let epv = Variation {
            score: pos.turn().factor() * pos.eval(),
            ..draw
        };
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
            let hashmove = te.pvMoves[0];
            let mut ordered: Vec<Move> = Vec::with_capacity(te.posMoves.len());
            ordered.push(hashmove);
            ordered.extend(te.posMoves.iter().filter(|&&m| m != hashmove));
            match checkBound(te.score) {
                Ordering::Equal if te.depth >= depth => {
                    let found = Variation {
                        length: 0,
                        moves: NONE,
                        score: te.score >> 2,
                        nodes: 1,
                        depth,
                    }
                    .from_iter(&mut te.pvMoves.into_iter());
                    found
                }

                Ordering::Less if te.depth >= depth => {
                    let alpha2 = max(alpha, (te.score - 1) >> 2);
                    if alpha2 > beta {
                        Variation {
                            length: 0,
                            moves: NONE,
                            score: alpha2,
                            nodes: 1,
                            depth,
                        }
                        .from_iter(&mut te.pvMoves.into_iter())
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
                        Variation {
                            length: 0,
                            moves: NONE,
                            score: beta2,
                            nodes: 1,
                            depth,
                        }
                        .from_iter(&mut te.pvMoves.into_iter())
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
            let ordered = if depth > 1 {
                orderMoves(&pos, killers, &moves[..])
            } else {
                moves
            };
            if ordered.len() == 0 {
                if pos.inCheck(pos.turn()) {
                    let mate = Variation {
                        score: P::whiteIsMate + (pos.getRootDistance() as i32 >> 1) * 3,
                        ..draw
                    };
                    mate
                } else {
                    draw
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
        let mut alpha = P::whiteIsMate;
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
                    println!("# negaDeep{}: it took only {}ms to lock the hash.", state.sid, dur);
                }
                search(
                    &mut hist,
                    &mut hash,
                    &mut killers,
                    false,
                    depth,
                    P::whiteIsMate,
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
            let pv = Variation {
                score: -pv1.score,
                depth: depth + 1,
                nodes: pv1.nodes + nodes + 1,
                ..pv1
            }
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
    let mut best = Variation {
        depth:  0,
        length: 0,
        moves:  NONE,
        nodes:  0,
        score:  P::whiteIsMate,
    };
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
        let mut alpha = P::whiteIsMate;
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
                        P::whiteIsMate,
                        P::blackIsMate,
                    )
                } else {
                    let pvx = pvsSearch(&mut hist, &mut hash, &mut killers, false, depth, -alpha, 1 - alpha);
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
                            P::blackIsMate,
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
            let pv = Variation {
                score: -pv1.score,
                depth: depth + 1,
                nodes: pv1.nodes + nodes + 1,
                ..pv1
            }
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
            depth:  1,
            nodes:  1,
            score:  -9999,
            moves:  [allMoves[0]; VariationMoves as usize],
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
            depth:  1,
            nodes:  1,
            score:  -9999,
            moves:  [allMoves[0]; VariationMoves as usize],
            length: 1,
        });
        state.tellNoMore();
    } else {
        if state.history.len() > 2 && !current.inEndgame() {
            // users position before his move
            let usrPos = state.history[state.history.len() - 2];
            match usrPos.moves().iter().copied().find(|m| usrPos.apply(*m) == current) {
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
