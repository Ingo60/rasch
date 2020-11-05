#![allow(non_snake_case)]

use std::cmp::{max, min, Ordering};
use std::collections::{HashMap, HashSet};
use std::env::args;
use std::io;
use std::io::Write;
use std::sync::MutexGuard;
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
    match argv[1].as_str() {
        "flamegraph" => {
            let n = if argv.len() >= 3 {
                match argv[2].parse::<u32>() {
                    Ok(k) => k,
                    Err(_) => 7,
                }
            } else {
                7
            };
            let gs = GameState::new(String::from("flamegraph"));
            flamegraph(gs, n)
        }
        "negamin" => {
            let mut gs = GameState::new(String::from("negamin"));
            gs.mainLoop(strategy_negamin)
        }
        "negamax" => {
            let mut gs = GameState::new(String::from("negamax"));
            gs.mainLoop(strategy_negamax)
        }
        other => {
            eprintln!("Illegal command line argument: `{}´", other);
            eprintln!("usage: {} [flamegraph N|negamin|negamax]", argv[0]);
        }
    };
}

fn flamegraph(gs: GameState, depth: u32) {
    let mut hist = vec![P::initialBoard()];
    let hash = gs.trtable.lock().unwrap();
    let before = Instant::now();
    let mut killers = HashSet::with_capacity(64);
    let (pv, _hash) = negaMax(
        &mut hist,
        hash,
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
        pv.showMoves()
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
    let badCapturing = if vt > 0                                  {  1 } else { 0 };
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
    killers: &mut KillerSet,
    ext: bool,
    depth: u32,
    alpha0: i32,
    beta: i32,
    moves: &Vec<Move>,
) -> (Variation, TransTable<'tt>) {
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
        let (pv, h) = negaMax(hist, hash, killers, capture, d, -beta, -alpha);
        hash = h;
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
            return (killerpv, hash);
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
            pvMoves: pv.moves(),
            posMoves: moves,
        };
        hash.insert(pos, tr);
    }
    hash
}

const NONE: [Move; VariationMoves] = [P::noMove; VariationMoves];

/// Move searching with NegaMax
pub fn negaMax<'tt>(
    hist: &mut Positions,
    hash: TransTable<'tt>,
    killers: &mut KillerSet,
    ext: bool,
    depth: u32,
    alpha: i32,
    beta: i32,
) -> (Variation, TransTable<'tt>) {
    let pos = *hist.last().unwrap(); // the history must not be empty
                                     // let none = [P::noMove; VariationMoves];
    #[rustfmt::skip]
    let draw = Variation {
        score: 0, nodes: 1, depth, length: 0, moves: NONE,
    };
    let mate = Variation {
        score: P::whiteIsMate,
        ..draw
    };
    if depth > 2 && computing::thinkingFinished() {
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
    // the follwoing is needed because else there is an immutable reference
    // to the hash
    let maybe: Option<common::Transp> = match hash.get(&pos) {
        Some(te) => Some(te.clone()),
        None => None,
    };
    let (rpv, rhash) = match maybe {
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
                    (found, hash)
                }

                Ordering::Less if te.depth >= depth => {
                    let alpha2 = max(alpha, (te.score - 1) >> 2);
                    if alpha2 > beta {
                        (
                            Variation {
                                length: 0,
                                moves: NONE,
                                score: alpha2,
                                nodes: 1,
                                depth,
                            }
                            .from_iter(&mut te.pvMoves.into_iter()),
                            hash,
                        )
                    } else {
                        let (pv, hash2) = negaMaxGo(hist, hash, killers, ext, depth, alpha2, beta, &ordered);
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
                                moves: NONE,
                                score: beta2,
                                nodes: 1,
                                depth,
                            }
                            .from_iter(&mut te.pvMoves.into_iter()),
                            hash,
                        )
                    } else {
                        let (pv, hash2) = negaMaxGo(hist, hash, killers, ext, depth, alpha, beta2, &ordered);
                        if depth >= te.depth && !ext {
                            let hash3 = insertPV(hash2, pos, pv, ordered, depth, alpha, beta2);
                            (pv, hash3)
                        } else {
                            (pv, hash2)
                        }
                    }
                }

                _other => {
                    let (pv, hash2) = negaMaxGo(hist, hash, killers, ext, depth, alpha, beta, &ordered);
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
                orderMoves(&pos, killers, &moves[..])
            } else {
                moves
            };
            if ordered.len() == 0 {
                if pos.inCheck(pos.turn()) {
                    (mate, hash)
                } else {
                    (draw, hash)
                }
            } else {
                let (pv, hash2) = negaMaxGo(hist, hash, killers, ext, depth, alpha, beta, &ordered);
                if !ext {
                    let hash3 = insertPV(hash2, pos, pv, ordered, depth, alpha, beta);
                    (pv, hash3)
                } else {
                    (pv, hash2)
                }
            }
        }
    };
    // overwrite the evaluated score with zero if it is repetition or 50
    // moves rule
    if pos.getPlyCounter() >= 100 || hist[0..hist.len() - 1].contains(&pos) {
        (Variation { score: 0, ..rpv }, rhash)
    } else {
        (rpv, rhash)
    }
}

/// Iterative deepening for strategy negamin
pub fn negaSimple(state: StrategyState, killers: &mut KillerSet, depth: u32, alpha: i32, beta: i32) {
    let mut depth = depth;
    // let mut killers = HashSet::with_capacity(64);
    println!("# negaSimple depth {}", depth);
    loop {
        let mut hist = state.history.clone();
        let (pv0, _) = {
            // acquire mutable access to the transposition table
            // the hash is locked during search
            let hash: TransTable = state.trtable.lock().unwrap();
            // println!("# hash size: {}", hash.len());
            // let kvec: Vec<Move> = killers.iter().copied().collect();
            // println!(
            //     "# killer size: {} {}",
            //     killers.len(),
            //     P::showMoves(&kvec[0..min(8, killers.len())])
            // );
            negaMax(&mut hist, hash, killers, false, depth, -beta, -alpha)
        };
        if computing::thinkingFinished() {
            break;
        }
        let mut pv = correctMateDistance(&pv0);
        pv.score *= state.player().factor();
        pv.depth = depth;
        if state.talkPV(pv) {
            if pv.length + 1 < depth || pv.score >= P::blackIsMate - 2 || pv.score == 0 {
                state.tellNoMore();
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

/// Iterative deepening for negaMax
pub fn negaDeep(state: StrategyState, killers: &mut KillerSet, depth: u32) {
    let mut depth = depth;
    let myPos = state.current();
    let myMoves = myPos.moves();
    let mut pvs: Variations = Vec::with_capacity(myMoves.len());
    let mut nodes = 0;
    // for increasing depth
    loop
    /* forever! */
    {
        println!("# negaDeep{} depth {}", state.sid, depth);
        let myOrderedMoves = if pvs.len() == 0 {
            // first iteration
            orderMoves(&myPos, killers, &myMoves[..])
        } else {
            // subsequent iterations, order the PVs by decreasing score and extract
            // our moves
            pvs.sort_unstable_by(|p1, p2| p2.score.cmp(&p1.score));
            // the unwrap should be safe as we pushed our move into the PV before
            pvs.iter().copied().map(|pv| pv.last().unwrap()).collect()
        };
        pvs.clear();
        println!(
            "# negaDeep{} we have {} ordered moves {}",
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
            let (pv0, _) = {
                // acquire mutable access to the transposition table
                // the hash is locked during search
                let hash: TransTable = state.trtable.lock().unwrap();
                // println!("# hash size: {}", hash.len());
                // let kvec: Vec<Move> = killers.iter().copied().take(8).collect();
                // println!("# killer size: {} {}", killers.len(),
                // P::showMoves(&kvec[..]));
                let dur = locking.elapsed().as_millis();
                if dur > 1 {
                    println!("# negaDeep{}: it took only {}ms to lock the hash.", state.sid, dur);
                }
                negaMax(&mut hist, hash, killers, false, depth, P::whiteIsMate, -alpha + 6)
            };
            if computing::thinkingFinished() {
                state.tellNoMore();
                return;
            }
            let pv1 = correctMateDistance(&pv0);

            // show the result of negamax
            // println!(
            //     "# after {}, negaMax answers with depth:{} score:{} nodes:{} {}",
            //     m,
            //     pv0.depth,
            //     pv0.score,
            //     pv0.nodes,
            //     pv0.showMoves()
            // );

            // nodes += pv1.nodes;
            // the final version with our move pushed onto the end
            let pv = Variation {
                score: -pv1.score, //
                //
                // * myPos.turn().factor(),
                depth: depth + 1,
                nodes: pv1.nodes + nodes + 1,
                ..pv1
            }
            .push(m);
            // make sure counter moves are treated as killers
            if let Some(killer) = pv1.last() {
                killers.insert(killer);
            }
            if pv.score >= alpha - 5 {
                if !state.talkPV(pv) {
                    println!("# negaDeep{} is not allowed to continue.", state.sid);
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

/// search with the *negamax* algorithm
pub fn strategy_negamax(state: StrategyState) {
    let mut killers = HashSet::with_capacity(2048);
    let allMoves = orderMoves(&state.current(), &mut killers, &state.current().moves());
    println!(
        "# Started strategy negamax {}, we have {} moves to consider.",
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
        negaDeep(state, &mut killers, 3);
    }
}
