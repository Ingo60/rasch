#![allow(non_snake_case)]

use std::sync::mpsc;

// use rasch::common;
use rasch::common::GameState;
use rasch::common::Protocol;
use rasch::common::Protocol::*;
use rasch::common::StrategyState;
// use rasch::computing as C;
// use rasch::fieldset::*;
use rasch::mdb;
// use rasch::position as P;
// use rasch::zobrist as Z;

fn main() {
    mdb::initStatic();
    let mut gs = GameState::new();
    gs.mainLoop(strategy_resign);
}

pub fn strategy_resign(mut state: StrategyState) {
    state.sender.send(NoMore(state.sid)).unwrap();
    state.sid = 0;
}
