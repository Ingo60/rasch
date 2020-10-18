#![allow(non_snake_case)]

// use std::sync::mpsc;
// use std::collections::HashMap;
use std::io;
use std::io::Write;

// use rasch::common;
use rasch::common::GameState;
// use rasch::common::Protocol;
use rasch::common::Protocol::*;
// use rasch::common::StrategyState;
// use rasch::common::Variation;
use rasch::common::*;
// use rasch::fen;
// use rasch::computing as C;
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
                    moves: vec![mv],
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
pub fn correctMateDistance(pv: &mut Variation) {
    let dist = pv.moves.len() as i32 * 2;
    if pv.score == P::whiteIsMate {
        pv.score += dist
    } else if pv.score == P::blackIsMate {
        pv.score -= dist
    } else {
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

    // if there's just 1 move left, we have no choice
    if allMoves.len() == 1 {
        if state.talkPV(Variation {
            depth: 1,
            nodes: 1,
            score: state.player().factor() * (-9999),
            moves: allMoves,
        }) {
            state.tellNoMore();
        }
        return;
    };
    // we do have more than 1 move at this point
    match &state.plan {
        // If the plan promises a mate in a few moves, we unconditionally play it.
        // Note that if this is so, the 50 moves rule doesn't get in the way.
        Some(plan) if plan.moves.len() > 2 && P::blackIsMate - 100 < plan.score * state.player().factor() => {
            println!("We have a mate plan: {}", P::showMoves(&plan.moves[2..]));
            io::stdout().flush().unwrap_or_default();
            if state.talkPV(Variation {
                depth: plan.depth - 2,
                nodes: 1,
                moves: plan.moves[2..].iter().copied().collect(),
                ..*plan
            }) {
                state.tellNoMore();
            };
            return;
        }
        // The plan should promise a win of at least 1/4 pawn.
        Some(plan) if plan.moves.len() > 2 && plan.score * state.player().factor() > 25 => {
            let planmv = plan.moves[2];
            println!("We have a plan: {}", P::showMoves(&plan.moves[2..]));
            io::stdout().flush().unwrap_or_default();
            if !state.talkPV(Variation {
                depth: plan.depth - 2,
                nodes: 1,
                moves: plan.moves[2..].iter().copied().collect(),
                ..*plan
            }) {
                // for some reason, we must stop
                return;
            };
            // re-search the move
            let pos = state.current().apply(planmv);
            let mut pv = Variation {
                nodes: 0,
                depth: 0,
                score: 0,
                moves: Vec::with_capacity(16),
            };
            pv.moves.push(planmv);
            state.history.push(pos);
            // TODO: continue with negaMax
        }
        _ => {
            // TODO: processing without a plan
        }
    };
}
