//! Move generation for CPos
//!

use super::basic::{CPosState, Move, Piece, Player};
use super::cpos::{CPos, Signature};
use super::cposio::{cpos_file_size, mk_egtb_path, CPosReader};
use super::fieldset::{BitSet, Field};
use super::mdb;
use super::position::{bit, pieceTargets};

use CPosState::*;
use Field::*;
use Piece::*;
use Player::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CPosMoveGenerator {
    cpos: CPos,
    player: Player,
    index: u64,
    whites: BitSet,
    occupied: BitSet,
    targets: BitSet,
}

impl CPosMoveGenerator {
    pub fn new(p: CPos, player: Player) -> CPosMoveGenerator {
        let mut this = CPosMoveGenerator {
            cpos: p,
            player,
            whites: BitSet::empty(),
            occupied: BitSet::empty(),
            index: 5,
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

    /// set of empty fields in this position
    pub fn empty(&self) -> BitSet {
        !self.occupied
    }

    /// tell if all fields in given bitset are empty
    pub fn all_empty(&self, fields: BitSet) -> bool {
        self.occupied.intersection(fields).null()
    }

    /// return true iff the opponent is not in check
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
    eprint!("{} Pass  1 - checking {}    0‰ ", signature, path);
    for cpos in rdr {
        done += 1;
        if done % (1024 * 1024) == 0 || done + 1 == expected {
            eprint!("\x08\x08\x08\x08\x08\x08{:4}‰ ", (done + 1) * 1000 / expected);
        }

        for pl in [BLACK, WHITE] {
            let cpm = CPosMoveGenerator::new(cpos, pl);
            let v = cpm.valid();
            let s = cpos.state(pl);
            if v && s == INVALID_POS || !v && s != INVALID_POS {
                eprintln!("failed.");
                println!("valid={}, state={:?}", v, s);
                println!("CPMG={:?}", cpm);
                println!("CPOS={:?}", cpos);
                return Err("failed".to_string());
            }
        }
    }
    eprintln!("done.");
    Ok(())
}
