#![allow(non_snake_case)]

use std::cmp::{max, min, Ordering};
use std::collections::HashMap;
use std::io;
use std::io::Write;
use std::sync::MutexGuard;

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
    let mut gs = GameState::new();
    gs.mainLoop(strategy_negamin);
    // match fen::decodeFEN("r3kbnr/pppnpp1p/6p1/q7/3P4/2N1B2P/PPP1BPP1/
    // R2QK2R w KQkq - 1 9") {     Err(s) => println!("decode FEN
    // impossible {}", s),     Ok(p) => {
    //         println!("posted position: {}", p);
    //         let p1 = p.apply(position::whiteShortCastlingMove1);
    //         println!("after Kf1:      {}", p1);
    //         let p2 = p1.apply(position::whiteShortCastlingMove2);
    //         println!("after Kg1:      {}", p2);
    //         let p3 = p2.apply(position::whiteShortCastlingMove3);
    //         println!("after Rf1:      {}", p3);
    //     }
    // }
}

type TransTable<'x> = MutexGuard<'x, HashMap<Position, Transp>>;
type Positions = Vec<Position>;

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
pub fn moveRating(pos: &Position, mv: Move) -> i32 {
    let rpos = pos.apply(mv);
    let before = pos.pieceOn(mv.from());
    let after = rpos.pieceOn(mv.to());
    let vt = pos.pieceOn(mv.to()).score();
    let goodCapture = if mv.piece().score() < vt - 10             { 9 } else { 0 };
    let badCapturing = if vt > 0                                  { 1 } else { 0 };
    let castling = if mv.piece() == KING && mv.promote() != EMPTY { 3 } else { 0 };
    let checking = if rpos.inCheck(rpos.turn())                   { 1 } else { 0 };
    
    let attacking_before = (P::pieceTargets(before, mv.player(), mv.from()) 
                            * pos.occupiedBy(mv.player().opponent())).card() as i32;
    
    let attacking_after  = (P::pieceTargets(after, mv.player(), mv.to()) 
                            * rpos.occupiedBy(mv.player().opponent())).card() as i32;
    let attacking = attacking_after - attacking_before;
    let pawnMove =  if rpos.inEndgame() && mv.piece() == PAWN {
                        if mv.promote() != EMPTY                  { 10 } else { 5 }
                    } else { 0 }; 
    
    goodCapture + badCapturing + castling + checking + attacking + pawnMove
}

/// order a bunch of moves so that the most useful one will processed
/// first
pub fn orderMoves(pos: &Position, moves: &[Move]) -> Vec<Move> {
    let ratings = moves.iter().copied().map(|mv| moveRating(pos, mv));
    let mut tuples = ratings.zip(moves.iter().copied()).collect::<Vec<_>>();
    tuples.sort_unstable_by(|(r1, _), (r2, _)| r2.cmp(r1)); // descending

    tuples.iter().copied().map(|(_, m)| m).collect()
}

/// Correct a variation score of mate to slightly smaller value
/// so that shorter ways to mate have a better score than longer ones.
pub fn correctMateDistance(var: &Variation) -> Variation {
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

pub fn insertTest(mut hash: TransTable) -> TransTable {
    hash.insert(
        P::initialBoard(),
        Transp {
            depth:    0,
            score:    0,
            posMoves: Vec::new(),
            pvMoves:  Vec::new(),
        },
    );
    hash
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

pub fn negaMaxGo<'tt>(
    hist: &mut Positions,
    mut hash: TransTable<'tt>,
    ext: bool,
    depth: u32,
    alpha0: i32,
    beta: i32,
    moves: &Vec<Move>,
) -> (Variation, TransTable<'tt>) {
    let mut best = Variation {
        nodes: 0,
        length: 0,
        moves: [P::noMove; VariationMoves],
        depth,
        score: -999_999_999,
    };
    let current = *hist.last().unwrap();
    let mut alpha = alpha0;
    for m in moves.iter().copied() {
        let pos = current.apply(m);
        let capture = !ext && depth == 1 && (!current.isEmpty(m.to()) || pos.inCheck(pos.turn()));
        let d = if capture { depth } else { depth - 1 };
        hist.push(pos);
        let (pv, h) = negaMax(hist, hash, capture, d, -beta, -alpha);
        hash = h;
        hist.pop();
        let score = -pv.score;
        if score > beta {
            // killer move
            let killer = m.flagKiller();
            let mut killerpv = pv.push(killer);
            killerpv.score = score;
            killerpv.nodes += best.nodes;
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
    (best, hash)
}

/// Helper function to insert a Variation into the hash
/// It is guaranteed that only variations with at least one move are
/// collected.
pub fn insertPV<'tt>(
    mut hash: TransTable<'tt>,
    pos: Position,
    pv: Variation,
    moves: Vec<Move>,
    depth: u32,
    alpha: i32,
    beta: i32,
) -> TransTable<'tt> {
    if pv.score != 0 && pv.length > 0 && depth > 2 {
        let bound = if pv.score >= beta {
            lowerBound(pv.score)
        } else if pv.score >= alpha {
            exactScore(pv.score)
        } else {
            upperBound(pv.score)
        };
        let tr = common::Transp {
            depth,
            score: bound,
            pvMoves: pv.moves().iter().copied().collect(),
            posMoves: moves,
        };
        // pv moves must get reversed
        hash.insert(pos, tr);
    }
    hash
}

/// Move searching with NegaMax
pub fn negaMax<'tt>(
    hist: &mut Positions,
    hash: TransTable<'tt>,
    ext: bool,
    depth: u32,
    alpha: i32,
    beta: i32,
) -> (Variation, TransTable<'tt>) {
    let pos = *hist.last().unwrap(); // the history must not be empty
    let none = [P::noMove; VariationMoves];
    #[rustfmt::skip]
    let draw = Variation {
        score: 0, nodes: 1, depth, length: 0, moves: none,
    };
    let mate = Variation {
        score: P::whiteIsMate,
        ..draw
    };
    if depth > 3 && computing::thinkingFinished()
        || pos.getPlyCounter() >= 100
        || hist[0..hist.len() - 1].contains(&pos)
    {
        return (draw, hash);
    }
    // This is the only point where we ever evaluate a position.
    // Nevertheless, it happens often, as this is at depth 0
    if depth == 0 {
        let epv = Variation {
            score: pos.turn().factor() * pos.eval(),
            ..draw
        };
        return (epv, hash);
    }
    // the follwoing is needed because else there is a immutable reference
    // to the hash
    let maybe: Option<common::Transp> = match hash.get(&pos) {
        Some(te) => None, // Some(te.clone()),
        None => None,
    };
    match maybe {
        Some(te) => {
            let hashmove = te.pvMoves[0];
            let mut ordered: Vec<Move> = Vec::with_capacity(te.posMoves.len());
            ordered.push(hashmove);
            ordered.extend(te.posMoves.iter().filter(|&&m| m != hashmove));
            match checkBound(te.score) {
                Ordering::Equal if te.depth >= depth => {
                    let found = Variation {
                        length: 0,
                        moves: none,
                        score: te.score >> 2,
                        nodes: 1,
                        depth,
                    }
                    .from_iter(&mut te.pvMoves.into_iter());
                    (found, hash)
                }

                Ordering::Less if te.depth >= depth => {
                    let alpha2 = max(alpha, (te.score - 1) >> 2);
                    if alpha2 > beta {
                        (
                            Variation {
                                length: 0,
                                moves: none,
                                score: alpha2,
                                nodes: 1,
                                depth,
                            }
                            .from_iter(&mut te.pvMoves.into_iter()),
                            hash,
                        )
                    } else {
                        let (pv, hash2) = negaMaxGo(hist, hash, ext, depth, alpha2, beta, &ordered);
                        if depth >= te.depth && !ext {
                            let hash3 = insertPV(hash2, pos, pv, ordered, depth, alpha2, beta);
                            (pv, hash3)
                        } else {
                            (pv, hash2)
                        }
                    }
                }

                Ordering::Greater if te.depth >= depth => {
                    let beta2 = min(beta, (te.score + 1) >> 2);
                    if alpha >= beta2 {
                        (
                            Variation {
                                length: 0,
                                moves: none,
                                score: beta2,
                                nodes: 1,
                                depth,
                            }
                            .from_iter(&mut te.pvMoves.into_iter()),
                            hash,
                        )
                    } else {
                        let (pv, hash2) = negaMaxGo(hist, hash, ext, depth, alpha, beta2, &ordered);
                        if depth >= te.depth && !ext {
                            let hash3 = insertPV(hash2, pos, pv, ordered, depth, alpha, beta2);
                            (pv, hash3)
                        } else {
                            (pv, hash2)
                        }
                    }
                }

                _otherwise => {
                    let (pv, hash2) = negaMaxGo(hist, hash, ext, depth, alpha, beta, &ordered);
                    if !ext {
                        let hash3 = insertPV(hash2, pos, pv, ordered, depth, alpha, beta);
                        (pv, hash3)
                    } else {
                        (pv, hash2)
                    }
                }
            }
        }
        None => {
            let moves = pos.moves();
            let ordered = if depth > 1 {
                orderMoves(&pos, &moves[..])
            } else {
                pos.moves()
            };
            if ordered.len() == 0 {
                if pos.inCheck(pos.turn()) {
                    (mate, hash)
                } else {
                    (draw, hash)
                }
            } else {
                let (pv, hash2) = negaMaxGo(hist, hash, ext, depth, alpha, beta, &ordered);
                if !ext {
                    let hash3 = insertPV(hash2, pos, pv, ordered, depth, alpha, beta);
                    (pv, hash3)
                } else {
                    (pv, hash2)
                }
            }
        }
    }
    // hash = insertTest(hash);
    // hash = insertTest(hash);
    // draw
}

/// Iterative deepening for negaMax
pub fn negaSimple(state: StrategyState, depth: u32, alpha: i32, beta: i32) {
    let mut depth = depth;
    println!("depth {}", depth);
    loop {
        if computing::thinkingFinished() {
            state.tellNoMore();
            break;
        }
        // acquire mutable access to the transposition table
        // the hash is locked during search
        let hash: TransTable = state.trtable.lock().unwrap();
        let mut hist = state.history.clone();
        let (pv0, _) = negaMax(&mut hist, hash, false, depth, -beta, -alpha);
        let mut pv = correctMateDistance(&pv0);
        pv.score *= state.player().factor();
        pv.depth = depth;
        if state.talkPV(pv) {
            if pv.length + 1 < depth
                || pv.score <= P::whiteIsMate + 2
                || pv.score >= P::blackIsMate - 2
                || pv.score == 0
            {
                state.tellNoMore();
                break;
            } else {
                depth += 1;
            }
        } else {
            break;
        }
    }
}

/// search with the *negamin* algorithm
pub fn strategy_negamin(state: StrategyState) {
    let allMoves = orderMoves(&state.current(), &state.current().moves());
    println!(
        "# Started strategy negamin, we have {} moves to consider.",
        allMoves.len()
    );
    io::stdout().flush().unwrap_or_default();

    // we need this all the time
    let factor = state.player().factor();

    // if there's just 1 move left, we have no choice
    if allMoves.len() == 1 {
        if state.talkPV(Variation {
            depth:  1,
            nodes:  1,
            score:  factor * (-9999),
            moves:  [allMoves[0]; VariationMoves as usize],
            length: 1,
        }) {
            state.tellNoMore();
        }
        return;
    };
    // we do have more than 1 move at this point
    match state.plan {
        // If the plan promises a mate in a few moves, we unconditionally play it.
        // Note that if this is so, the 50 moves rule doesn't get in the way.
        Some(plan) if plan.length > 2 && plan.score * factor > P::blackIsMate - 100 => {
            println!("# We have a mate plan: {}", P::showMoves(&plan.moves[2..]));
            io::stdout().flush().unwrap_or_default();
            if state.talkPV(
                Variation {
                    depth: plan.depth - 2,
                    nodes: 1,
                    moves: [P::noMove; VariationMoves],
                    ..plan
                }
                .from_iter(&mut plan.moves().into_iter().skip(2)),
            ) {
                state.tellNoMore();
            };
            return;
        }
        // The plan should promise a win of at least 1/4 pawn.
        Some(plan) if plan.length > 2 && plan.score * factor > 25 => {
            let planmv = plan.moves[2];
            println!("# We have a plan: {}", P::showMoves(&plan.moves[2..]));
            io::stdout().flush().unwrap_or_default();
            if !state.talkPV(
                Variation {
                    depth: plan.depth - 2,
                    nodes: 1,
                    moves: [P::noMove; VariationMoves],
                    ..plan
                }
                .from_iter(&mut plan.moves().into_iter().skip(2)),
            ) {
                // for some reason, we must stop
                return;
            };
            // re-search the move
            let pos = state.current().apply(planmv);
            let mut hist = state.history.clone();
            hist.push(pos);
            let pv2 = {
                // acquire mutable access to the transposition table
                // the hash is locked during search
                let hash: TransTable = state.trtable.lock().unwrap();

                let (pv, _) = negaMax(
                    &mut hist,
                    hash,
                    false,
                    min(8, plan.depth - 1),
                    P::whiteIsMate,
                    P::blackIsMate,
                );
                correctMateDistance(&pv)
            };
            let mut pv = pv2.unshift(planmv);
            pv.score *= pos.turn().factor();
            pv.depth = plan.depth;
            println!("# Plan continuation: {}", pv.showMoves());
            io::stdout().flush().unwrap_or_default();
            let cont = state.talkPV(pv);
            let newscore = factor * pv.score;
            if newscore > P::percent(95, factor * plan.score)
                && (newscore == P::blackIsMate || newscore <= P::blackIsMate - 100)
            {
                println!("# Plan looks promising.");
                state.tellNoMore();
            } else {
                println!("# We better look for another plan.");
                if cont {
                    negaSimple(state, plan.depth, P::whiteIsMate, P::blackIsMate);
                }
            }
            io::stdout().flush().unwrap_or_default();
        }
        _ => {
            println!("We do not have a plan.");
            io::stdout().flush().unwrap_or_default();
            negaSimple(state, 3, P::whiteIsMate, P::blackIsMate);
        }
    };
}
