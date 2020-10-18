#![allow(non_snake_case)]

use std::cmp::{max, min};
use std::collections::HashMap;
use std::io;
use std::io::Write;
use std::sync::MutexGuard;

// use rasch::common;
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
    gs.mainLoop(strategy_best);
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

pub fn insertTest(mut hash: TransTable) {
    hash.insert(
        P::initialBoard(),
        Transp {
            depth:    0,
            score:    0,
            posMoves: Vec::new(),
            pvMoves:  Vec::new(),
        },
    );
}

/// Move searching with NegaMax
pub fn negaMax(
    hist: &mut Vec<Position>,
    mut hash: TransTable,
    extended: bool,
    depth: u32,
    alpha: i32,
    beta: i32,
) -> Variation {
    insertTest(hash);
    Variation {
        depth:  1,
        nodes:  1,
        score:  1234,
        moves:  [P::noMove; VariationMoves],
        length: 1,
    }
}

/// Iterative deepening for negaMax
pub fn negaSimple(mut state: StrategyState, depth: u32, alpha: i32, beta: i32) {
    let mut depth = depth;
    println!("depth {}", depth);
    loop {
        if computing::thinkingFinished() {
            state.tellNoMore();
            break;
        }
        // acquire mutable access to the transposition table
        // the hash is locked during search
        let mut hash: TransTable = state.trtable.lock().unwrap();
        let mut hist = state.history.clone();
        let mut pv = correctMateDistance(&negaMax(&mut hist, hash, false, depth, -beta, -alpha));
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
pub fn strategy_negamin(mut state: StrategyState) {
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
                .from_iter(&mut plan.moves().into_iter().skip(2).copied()),
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
                .from_iter(&mut plan.moves().into_iter().skip(2).copied()),
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
                let mut hash: TransTable = state.trtable.lock().unwrap();

                correctMateDistance(&negaMax(
                    &mut hist,
                    hash,
                    false,
                    min(8, plan.depth - 1),
                    P::whiteIsMate,
                    P::blackIsMate,
                ))
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
            negaSimple(state, 4, P::whiteIsMate, P::blackIsMate);
        }
    };
}
