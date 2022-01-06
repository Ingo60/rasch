//! Move generation for CPos
//!

use super::basic::{CPosState, Move, Piece, Player};
use super::cpos::{CPos, Signature};
use super::cposio::{cpos_file_size, mk_egtb_path, CPosReader};
use super::fieldset::{BitSet, Field};
use super::mdb;
use super::position::{bit, pieceTargets, showMoves, showMovesSAN};
use crate::fen::encodeFEN;
use crate::util::formatted64;

use CPosState::*;
use Field::*;
use Piece::*;
use Player::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CPosMoveIterator {
    cpos: CPos,
    player: Player,
    promote_to: Piece,
    index: u64,
    whites: BitSet,
    occupied: BitSet,
    targets: BitSet,
}

impl CPosMoveIterator {
    pub fn new(p: CPos, player: Player) -> CPosMoveIterator {
        let mut this = CPosMoveIterator {
            cpos: p,
            player,
            promote_to: QUEEN,
            whites: BitSet::empty(),
            occupied: BitSet::empty(),
            index: 4,
            targets: BitSet::empty(),
        };
        let wkbit = bit(p.white_king());
        this.occupied = this.occupied + wkbit;
        this.whites = this.whites + wkbit;
        this.occupied = this.occupied + bit(p.black_king());
        for i in 0..4 {
            if p.piece_at(i) != EMPTY {
                let f = bit(p.field_at(i));
                this.occupied = this.occupied + f;
                if p.player_at(i) == WHITE {
                    this.whites = this.whites + f;
                    if player == WHITE && i < this.index {
                        this.index = i;
                    }
                } else {
                    if player == BLACK && i < this.index {
                        this.index = i;
                    }
                }
            }
        }
        this.targets = if this.index < 4 {
            pieceTargets(p.piece_at(this.index), player, p.field_at(this.index))
        } else {
            pieceTargets(KING, player, p.players_king(player))
        };
        this
    }

    /// set of fields occupied by player
    pub fn occupied_by_me(&self) -> BitSet {
        if self.player == WHITE {
            self.whites
        } else {
            self.occupied - self.whites
        }
    }

    /// set of fields occupied by opponent
    pub fn occupied_by_him(&self) -> BitSet {
        if self.player == WHITE {
            self.occupied - self.whites
        } else {
            self.whites
        }
    }

    /// set of empty fields in this position
    pub fn empty(&self) -> BitSet {
        !self.occupied
    }

    /// tell if all fields in given bitset are empty
    pub fn all_empty(&self, fields: BitSet) -> bool {
        self.occupied.intersection(fields).null()
    }

    /// return true iff the opponent is not in check
    ///
    /// See also `CPos::valid`
    pub fn valid(&self) -> bool {
        let his_king = self.cpos.players_king(self.player.opponent());
        let my_king = self.cpos.players_king(self.player);
        if mdb::kingTargets(my_king).member(his_king) {
            false
        } else {
            for u in 0..4 {
                if self.cpos.player_at(u) == self.player {
                    let from = self.cpos.field_at(u);
                    match self.cpos.piece_at(u) {
                        EMPTY => return true, /* no attacker can be found further left */
                        QUEEN => {
                            if self.all_empty(mdb::canBishop(from, his_king))
                                || self.all_empty(mdb::canRook(from, his_king))
                            {
                                return false;
                            }
                        }
                        ROOK => {
                            if self.all_empty(mdb::canRook(from, his_king)) {
                                return false;
                            }
                        }
                        BISHOP => {
                            if self.all_empty(mdb::canBishop(from, his_king)) {
                                return false;
                            }
                        }
                        KNIGHT => {
                            if mdb::knightTargets(from).member(his_king) {
                                return false;
                            }
                        }
                        PAWN => {
                            if self.player == BLACK && mdb::targetOfBlackPawns(his_king).member(from)
                                || self.player == WHITE && mdb::targetOfWhitePawns(his_king).member(from)
                            {
                                return false;
                            }
                        }
                        _ => panic!(""),
                    }
                }
            }
            true
        }
    }

    /// Apply a move to the contained CPos and return a new one.
    ///
    /// May be a bit clumsy. See also `CPos::apply()`.
    pub fn apply(&self, mv: Move) -> CPos {
        let mut new = self.cpos;
        if self.occupied_by_him().member(mv.to()) {
            match new.piece_index_by_field(mv.to()) {
                Ok(u) => new = new.clear_piece(u),
                Err(KING) => panic!("attempt to capture KING"),
                Err(x) => panic!("occupied field {} is {:?}", mv.to(), x),
            }
        }
        match self.cpos.piece_index_by_field(mv.from()) {
            Ok(u) => {
                if mv.promote() != EMPTY {
                    new = new.change_piece(u, mv.promote())
                }
                new = new.move_piece(u, mv.to());
            }
            Err(KING) => {
                new = new.move_king(mv.player(), mv.to());
            }
            Err(x) => panic!("attempt to move from {:?} field {}", x, mv.from()),
        }
        new.ordered()
    }
}

impl Iterator for CPosMoveIterator {
    type Item = Move;
    /// Give us the next **raw** move. Must be checked for validity.
    fn next(&mut self) -> Option<Self::Item> {
        while self.index < 5 {
            match self.targets.next() {
                None => {
                    // continue with next piece, if any, or the king
                    self.index += 1;
                    if self.index < 4 {
                        let pc = self.cpos.piece_at(self.index);
                        if pc != EMPTY && self.cpos.player_at(self.index) == self.player {
                            // found next piece
                            self.targets = pieceTargets(pc, self.player, self.cpos.field_at(self.index));
                        };
                    } else if self.index == 4 {
                        self.targets = pieceTargets(KING, self.player, self.cpos.players_king(self.player));
                    } else
                    /* if self.index > 4 */
                    {
                        return None;
                    }
                }
                Some(to) if self.occupied_by_me().member(to) => {}
                Some(to) => {
                    let from = if self.index < 4 {
                        self.cpos.field_at(self.index)
                    } else {
                        self.cpos.players_king(self.player)
                    };
                    let piece = if self.index < 4 { self.cpos.piece_at(self.index) } else { KING };
                    let mv = Move::new(self.player, piece, EMPTY, from, to);
                    match piece {
                        PAWN => {
                            let es = match self.player {
                                WHITE => mdb::canWhitePawn(from, to),
                                BLACK => mdb::canBlackPawn(from, to),
                            };
                            let promotion = self.player == WHITE && to.rank() == 8
                                || self.player == BLACK && to.rank() == 1;
                            let valid = es.some() && self.all_empty(es)
                                || es.null() // it's a capturing move
                                        &&     self.occupied_by_him().member(to);
                            if valid {
                                if promotion {
                                    let promotion_mv =
                                        Move::new(self.player, PAWN, self.promote_to, from, to);

                                    // arrange for next next(), which may hit this exact target field
                                    self.promote_to = match self.promote_to {
                                        QUEEN => ROOK,
                                        ROOK => BISHOP,
                                        BISHOP => KNIGHT,
                                        _ => PAWN,
                                    };
                                    if self.promote_to != PAWN {
                                        // we need to come back here!
                                        self.targets = self.targets + bit(to);
                                    } else {
                                        // we are done with emitting promotion moves
                                        self.promote_to = QUEEN;
                                    }
                                    return Some(promotion_mv);
                                } else {
                                    return Some(mv);
                                }
                            };
                        }
                        BISHOP => {
                            if self.all_empty(mdb::canBishop(from, to)) {
                                return Some(mv);
                            }
                        }
                        ROOK => {
                            if self.all_empty(mdb::canRook(from, to)) {
                                return Some(mv);
                            }
                        }
                        QUEEN => {
                            if self.all_empty(mdb::canBishop(from, to))
                                || self.all_empty(mdb::canRook(from, to))
                            {
                                return Some(mv);
                            }
                        }
                        _other => return Some(mv),
                    };
                }
            }
        }
        None
    }
}

pub fn test1(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let egtb_path = mk_egtb_path(signature, "egtb");
    let unsorted_path = mk_egtb_path(signature, "unsorted");
    let mut path: &str = &egtb_path;
    let rdr = CPosReader::new(&egtb_path).or_else(|_| {
        path = &unsorted_path;
        CPosReader::new(&unsorted_path)
    })?;
    let expected = cpos_file_size(path)?;
    let mut done = 0;
    eprint!("{} Pass  1 - test1 CPos {}    0‰ ", signature, path);
    for cpos in rdr {
        done += 1;
        if done % (1024 * 1024) == 0 || done + 1 == expected {
            eprint!("\x08\x08\x08\x08\x08\x08{:4}‰ ", (done + 1) * 1000 / expected);
        }

        for player in [BLACK, WHITE] {
            // let cpm = CPosMoveIterator::new(cpos, pl);
            // let v = cpm.valid();
            let v = cpos.valid(player);
            let s = cpos.state(player);
            if v && s == INVALID_POS || !v && s != INVALID_POS {
                eprintln!("failed.");
                println!("valid={}, state={:?}", v, s);
                // println!("CPMG={:?}", cpm);
                println!("CPOS={:?}", cpos);
                return Err("failed".to_string());
            }

            if v {
                let pos = cpos.uncompressed(player);
                let mut pmoves = pos.moves();
                pmoves.sort_unstable();
                let mut cmoves = Vec::with_capacity(pmoves.len());
                for m in cpos.move_iterator(player) {
                    if cpos.apply(m).valid(player.opponent()) {
                        cmoves.push(m);
                    }
                }
                cmoves.sort_unstable();
                let mut pi = 0;
                let mut ci = 0;
                while pi < pmoves.len() || ci < cmoves.len() {
                    if pi >= pmoves.len() {
                        eprintln!("failed.");
                        println!(
                            "extra cpos moves  {}  {} ",
                            encodeFEN(&pos),
                            showMoves(&cmoves[ci..cmoves.len()])
                        );
                        return Err("extra CPos moves".to_string());
                    }
                    if ci >= cmoves.len() {
                        eprintln!("failed.");
                        println!(
                            "missing cpos moves  {}  {} ",
                            encodeFEN(&pos),
                            showMovesSAN(&pmoves[pi..pmoves.len()], pos)
                        );
                        return Err("missing CPos moves".to_string());
                    }
                    if pmoves[pi] < cmoves[pi] {
                        eprintln!("failed.");
                        println!(
                            "missing cpos move  {}  {} ",
                            encodeFEN(&pos),
                            pmoves[pi].showSAN(pos)
                        );
                        println!("p-moves: {}", showMoves(&pmoves));
                        println!("c-moves: {}", showMoves(&cmoves));
                        println!(
                            "targets of black pawn standing on a3: {}",
                            pieceTargets(PAWN, BLACK, A3)
                        );

                        for m in cpos.move_iterator(BLACK) {
                            println!(
                                "iterator yields {}, result {:?}, valid={}",
                                m,
                                cpos.apply(m),
                                cpos.apply(m).valid(WHITE)
                            );
                        }
                        println!("move-iterator: {:#?}", cpos.move_iterator(BLACK));
                        let mut miter = cpos.move_iterator(BLACK);
                        match miter.next() {
                            None => {}
                            Some(_) => {}
                        }
                        return Err("missing CPos move".to_string());
                    }
                    if pmoves[pi] > cmoves[pi] {
                        eprintln!("failed.");
                        println!(
                            "extra cpos move  {}  {} ",
                            encodeFEN(&pos),
                            cmoves[ci].algebraic()
                        );
                        return Err("extra CPos move".to_string());
                    }
                    pi += 1;
                    ci += 1;
                }
            }
        }
    }
    eprintln!("done");
    Ok(())
}

pub fn test1p(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let egtb_path = mk_egtb_path(signature, "egtb");
    let unsorted_path = mk_egtb_path(signature, "unsorted");
    let mut path: &str = &egtb_path;
    let rdr = CPosReader::new(&egtb_path).or_else(|_| {
        path = &unsorted_path;
        CPosReader::new(&unsorted_path)
    })?;
    let expected = cpos_file_size(path)?;
    let mut done = 0;
    let mut moves = 0u64;
    eprint!("{} Pass  1 - test1 Position {}    0‰ ", signature, path);
    for cpos in rdr {
        done += 1;
        if done % (1024 * 1024) == 0 || done + 1 == expected {
            eprint!("\x08\x08\x08\x08\x08\x08{:4}‰ ", (done + 1) * 1000 / expected);
        }

        for pl in [BLACK, WHITE] {
            let pos = cpos.uncompressed(pl);
            let v = pos.valid();
            let s = cpos.state(pl);
            if v && s == INVALID_POS || !v && s != INVALID_POS {
                eprintln!("failed.");
                println!("valid={}, state={:?}", v, s);
                println!("POS={}", pos);
                println!("CPOS={:?}", cpos);
                return Err("failed".to_string());
            }
            if v {
                moves += pos.moves().len() as u64;
            }
        }
    }
    eprintln!("done, {} moves", formatted64(moves));
    Ok(())
}
