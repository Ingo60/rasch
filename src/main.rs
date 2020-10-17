#![allow(non_snake_case)]

// use std::sync::mpsc;

// use rasch::common;
use rasch::common::GameState;
// use rasch::common::Protocol;
use rasch::common::Protocol::*;
use rasch::common::StrategyState;
use rasch::common::Variation;
use rasch::fen;
// use rasch::computing as C;
// use rasch::fieldset::*;
use rasch::mdb;
use rasch::position;
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
