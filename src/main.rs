#![allow(non_snake_case)]

// use rasch::common;
use rasch::common::GameState;
// use rasch::computing as C;
// use rasch::fieldset::*;
use rasch::mdb;
// use rasch::position as P;
// use rasch::zobrist as Z;

fn main() {
    mdb::initStatic();
    let mut gs = GameState::new();
    gs.mainLoop();
}
