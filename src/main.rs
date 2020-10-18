#![allow(non_snake_case)]

// use std::sync::mpsc;
// use std::collections::HashMap;

// use rasch::common;
use rasch::common::GameState;
// use rasch::common::Protocol;
use rasch::common::Protocol::*;
use rasch::common::StrategyState;
use rasch::common::Variation;
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
pub fn orderMoves(pos: &Position, moves: &Vec<Move>) -> Vec<Move> {
    let ratings = moves.iter().copied().map(|mv| moveRating(pos, mv));
    let mut tuples = ratings.zip(moves.iter().copied()).collect::<Vec<_>>();
    tuples.sort_unstable_by(|(r1, _), (r2, _)| r2.cmp(r1)); // descending

    tuples.iter().copied().map(|(_, m)| m).collect()
}
pub fn strategy_negamin() {}
