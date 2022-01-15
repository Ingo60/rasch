//! Move generation for CPos
//!

use std::collections::HashSet;

use super::basic::{CPosState, Move, Piece, Player};
use super::cpos::{CPos, Signature};
use super::cposio::{cpos_file_size, mk_egtb_path, CPosReader};
use super::fen::encodeFEN;
use super::fieldset::{BitSet, Field};
use super::mdb;
use super::position::{bit, pieceTargets, showMoves, showMovesSAN, Position};
use super::util::formatted_sz;

use CPosState::*;
use Field::*;
use Piece::*;
use Player::*;

/// the set of fields from whence a piece can come to a given field.
/// ```
/// use rasch::fieldset::{BitSet, Field};
/// use rasch::position::bit;
/// use rasch::basic::{Piece, Player};
/// use Field::*;
/// use Piece::*;
/// use Player::*;
/// use rasch::cposmove::*;
/// use rasch::mdb::initStatic;
/// initStatic();
/// assert!(piece_sources(PAWN, WHITE, E4) == bit(E3)+bit(E2)+bit(D3)+bit(F3));
/// ```
pub fn piece_sources(piece: Piece, player: Player, to: Field) -> BitSet {
    match piece {
        PAWN => match player {
            WHITE => mdb::whitePawnSources(to),
            BLACK => mdb::blackPawnSources(to),
        },
        KNIGHT => mdb::knightTargets(to),
        BISHOP => mdb::bishopTargets(to),
        ROOK => mdb::rookTargets(to),
        QUEEN => mdb::rookTargets(to) + mdb::bishopTargets(to),
        KING => mdb::kingTargets(to),
        EMPTY => BitSet::empty(),
    }
}

/// Find raw `Move`s for position in `cpos` attribute assuming it is `player`s turn.
/// The `CPos` must be `CPos::valid` for the player, otherwise applying the generated moves may panic.
///
/// Generated `Move`s must also be checked for validity after application. General pattern is like
///
/// ```
/// if cpos.valid(player) {
///     for mv in cpos.move_iterator() {
///         if cpos.apply(mv).valid(player.opponent()) {
///             // the mv was valid
///         }
///     }
/// }
/// ```
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

/// Find raw `Move`s of the opponent of `player` that could have resulted in the given `cpos`.
/// The `CPos` should be `CPos::valid` for the player, else it doesn't make sense.
///
/// By `CPos::unapply`ing the generated moves to the `CPos` one obtains possible positions (not necessarily
/// valid ones, one has to check for this) from where
/// applying the same move will result in the original `CPos`.
///
/// Note that unapplying a capturing pawn move **requires** a piece to be placed on the vacated `to` field.
/// The piece may not be a `KING`, of course, and if it is also a pawn promotion, it may not be a `PAWN`.
/// For example:
/// ```
/// cpos.unapply(Move:new(WHITE, PAWN, QUEEN, A7, B8), ROOK)
/// ```
/// will result in  a position where A7 is a white pawn and the white queen on B8 is replaced with a BLACK ROOK.
///
/// General pattern looks like:
///
/// ```
/// if cpos.valid(player) {
///     for mv in cpos.reverse_move_iterator() {
///         let prev = if mv.is_capture_by_pawn() {
///             cpos.unapply(mv, piece); // where piece is not EMPTY or KING
///                                      // and PAWN only if no promotion
///         }
///         else { cpos.unapply(mv, EMPTY) } // or any piece except KING and PAWN on promotion rank
///         if prev.valid(player.opponent()) {
///             assert!(prev.apply(mv) == cpos);
///         }
///     }
/// }
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CPosReverseMoveIterator {
    cpos: CPos,
    player: Player,
    promotion: bool,
    index: u64,
    whites: BitSet,
    occupied: BitSet,
    sources: BitSet,
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

impl CPosReverseMoveIterator {
    pub fn new(p: CPos, player: Player) -> CPosReverseMoveIterator {
        let mut this = CPosReverseMoveIterator {
            cpos: p,
            player,
            promotion: true, // e.g. give a pawn move e7e8q for a queen on e8
            whites: BitSet::empty(),
            occupied: BitSet::empty(),
            index: 4,
            sources: BitSet::empty(),
        };
        let wkbit = bit(p.white_king());
        this.occupied = this.occupied + wkbit;
        this.whites = this.whites + wkbit;
        this.occupied = this.occupied + bit(p.black_king());
        for u in 0..4 {
            let piece = p.piece_at(u);
            if piece != EMPTY {
                let f = p.field_at(u);
                let fbs = bit(f);
                this.occupied = this.occupied + fbs;
                if p.player_at(u) == WHITE {
                    this.whites = this.whites + fbs;
                    if player == BLACK && f.rank() == 8 && u < this.index && piece < KING && piece > PAWN {
                        this.index = u;
                    }
                } else
                /* piece is black */
                {
                    if player == WHITE && f.rank() == 1 && u < this.index && piece < KING && piece > PAWN {
                        this.index = u;
                    }
                }
            }
        }
        if this.index < 4 {
            // there is some piece that could be the result of a promotion
            let f = p.field_at(this.index);
            this.sources = if this.player == BLACK {
                mdb::whitePawnSources(f)
            } else {
                mdb::blackPawnSources(f)
            };
        } else {
            this.promotion = false;
            this.index = 4;
            for u in 0..4 {
                if p.piece_at(u) != EMPTY && p.player_at(u) != this.player {
                    this.index = u;
                    break;
                }
            }
            this.sources = if this.index < 4 {
                piece_sources(
                    p.piece_at(this.index),
                    this.player.opponent(),
                    p.field_at(this.index),
                )
            } else {
                piece_sources(
                    KING,
                    this.player.opponent(),
                    p.players_king(this.player.opponent()),
                )
            };
        }

        this
    }

    /// set of fields occupied by player
    pub fn occupied_by_player(&self) -> BitSet {
        if self.player == WHITE {
            self.whites
        } else {
            self.occupied - self.whites
        }
    }

    /// set of fields occupied by opponent
    pub fn occupied_by_opponent(&self) -> BitSet {
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
}

impl Iterator for CPosReverseMoveIterator {
    type Item = Move;
    /// Give us the next **raw** backwards move. Must be checked for validity.
    fn next(&mut self) -> Option<Self::Item> {
        let me = self.player.opponent();
        while self.index < 5 {
            match self.sources.next() {
                None if self.promotion => {
                    let rank = if me == WHITE { 8 } else { 1 };
                    self.index += 1;
                    if self.index < 4 {
                        let pc = self.cpos.piece_at(self.index);
                        let f = self.cpos.field_at(self.index);
                        if pc < KING && pc > PAWN && self.cpos.player_at(self.index) == me && f.rank() == rank
                        {
                            // found next piece
                            self.sources =
                                if me == WHITE { mdb::whitePawnSources(f) } else { mdb::blackPawnSources(f) };
                        };
                        // if this was not a piece eligible for promotion,
                        // continue with still empty self.sources, this'll try the next one
                    } else {
                        self.promotion = false;
                        for u in 0..4 {
                            if self.cpos.piece_at(u) != EMPTY && self.cpos.player_at(u) == me {
                                self.index = u;
                                break;
                            }
                        }
                        self.sources = if self.index < 4 {
                            piece_sources(self.cpos.piece_at(self.index), me, self.cpos.field_at(self.index))
                        } else {
                            piece_sources(KING, me, self.cpos.players_king(me))
                        };
                    };
                }
                None => {
                    // continue with next piece, if any, or the king
                    self.index += 1;
                    if self.index < 4 {
                        let pc = self.cpos.piece_at(self.index);
                        if pc != EMPTY && self.cpos.player_at(self.index) == me {
                            // found next piece
                            self.sources = piece_sources(pc, me, self.cpos.field_at(self.index));
                        };
                    } else if self.index == 4 {
                        self.sources = piece_sources(KING, me, self.cpos.players_king(me));
                    } else
                    /* if self.index > 4 */
                    {
                        return None;
                    };
                }
                Some(from) if self.occupied.member(from) => {}
                Some(from) => {
                    let to = if self.index < 4 {
                        self.cpos.field_at(self.index)
                    } else {
                        self.cpos.players_king(me)
                    };
                    let piece = if self.index < 4 { self.cpos.piece_at(self.index) } else { KING };
                    let mv = Move::new(me, piece, EMPTY, from, to);
                    match piece {
                        PAWN => {
                            let es = match me {
                                WHITE => mdb::canWhitePawn(from, to) - bit(to),
                                BLACK => mdb::canBlackPawn(from, to) - bit(to),
                            };
                            // since to is a PAWN, this was not a promotion
                            if self.all_empty(es) {
                                return Some(mv);
                            };
                        }
                        KNIGHT | BISHOP | ROOK | QUEEN if self.promotion => {
                            // promotional move
                            return Some(Move::new(me, PAWN, piece, from, to));
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

pub fn test2(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    println!("{} predecessors:", signature);
    let relatives = signature.get_relatives();
    for r in relatives {
        println!("{:?}", r);
    }

    Ok(())
}

pub fn test3(pos: &Position) -> Result<(), String> {
    let cpos = pos.compressed();
    print!("{:?} is result of one of:", cpos);
    for mv in CPosReverseMoveIterator::new(cpos, pos.turn()) {
        print!(" {}", mv);
    }
    println!();
    for mv in cpos.reverse_move_iterator(pos.turn()) {
        let unc = if mv.is_capture_by_pawn() { QUEEN } else { EMPTY };
        let prev = cpos.unapply(mv, unc);
        let v = prev.valid(pos.turn().opponent());
        println!("unapplying {} yields {:?} (valid:{})", mv, prev, v);
        if v {
            let rpos = prev.apply(mv);
            if rpos == cpos {
                println!("re-applying {} yields original.", mv)
            } else {
                println!("re-applying {} yields different {:?}", mv, rpos);
            }
        }
    }
    Ok(())
}

/// Does it ever happen that SigA and canonic(SigA) are both predecessors of SigB?
/// ```
/// rasch::cposmove::test4().unwrap();
/// ```
pub fn test4() -> Result<(), String> {
    let qrbnp = vec![QUEEN, ROOK, BISHOP, KNIGHT, PAWN];
    let mut sigs: HashSet<Signature> = HashSet::new();
    let mut vsig = Vec::new();
    for a in qrbnp.clone() {
        vsig.clear();
        vsig.push((WHITE, a));
        sigs.insert(Signature::from_vec(&vsig).mk_canonic());

        for b in qrbnp.clone() {
            vsig.clear();
            vsig.push((WHITE, a));
            vsig.push((WHITE, b));
            sigs.insert(Signature::from_vec(&vsig).mk_canonic());
            vsig.clear();
            vsig.push((WHITE, a));
            vsig.push((BLACK, b));
            sigs.insert(Signature::from_vec(&vsig).mk_canonic());

            for c in qrbnp.clone() {
                vsig.clear();
                vsig.push((WHITE, a));
                vsig.push((WHITE, b));
                vsig.push((WHITE, c));
                sigs.insert(Signature::from_vec(&vsig).mk_canonic());
                vsig.clear();
                vsig.push((WHITE, a));
                vsig.push((WHITE, b));
                vsig.push((BLACK, c));
                sigs.insert(Signature::from_vec(&vsig).mk_canonic());
                vsig.clear();
                vsig.push((WHITE, a));
                vsig.push((BLACK, b));
                vsig.push((BLACK, c));
                sigs.insert(Signature::from_vec(&vsig).mk_canonic());
                for d in qrbnp.clone() {
                    vsig.clear();
                    vsig.push((WHITE, a));
                    vsig.push((WHITE, b));
                    vsig.push((WHITE, c));
                    vsig.push((WHITE, d));
                    sigs.insert(Signature::from_vec(&vsig).mk_canonic());
                    vsig.clear();
                    vsig.push((WHITE, a));
                    vsig.push((WHITE, b));
                    vsig.push((WHITE, c));
                    vsig.push((BLACK, d));
                    sigs.insert(Signature::from_vec(&vsig).mk_canonic());
                    vsig.clear();
                    vsig.push((WHITE, a));
                    vsig.push((WHITE, b));
                    vsig.push((BLACK, c));
                    vsig.push((BLACK, d));
                    sigs.insert(Signature::from_vec(&vsig).mk_canonic());
                    vsig.clear();
                    vsig.push((WHITE, a));
                    vsig.push((BLACK, b));
                    vsig.push((BLACK, c));
                    vsig.push((BLACK, d));
                    sigs.insert(Signature::from_vec(&vsig).mk_canonic());
                }
            }
        }
    }
    for sig in sigs {
        let pmap = sig.predecessors();
        for p in pmap.keys() {
            if !p.is_canonic() && pmap.contains_key(&p.mk_canonic()) {
                let a1 = pmap.get(&p).unwrap();
                let a2 = pmap.get(&p.mk_canonic()).unwrap();
                println!(
                    "{} has {} (by {:?}) and {} (by {:?}) as predecessors.",
                    sig,
                    p,
                    a1,
                    p.mk_canonic(),
                    a2
                );
                // return Err("Mist".to_string());
            }
        }
    }
    Ok(())
}

/// look for MATE positions in sorted file
pub fn test5(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let egtb_path = mk_egtb_path(signature, "egtb");
    let sorted_path = mk_egtb_path(signature, "sorted");
    let mut path: &str = &egtb_path;
    let rdr = CPosReader::new(&egtb_path).or_else(|_| {
        path = &sorted_path;
        CPosReader::new(&sorted_path)
    })?;
    let mut mate = vec![0, 0];
    let mut canm = vec![0, 0];
    // let mut egtbmap = HashMap::new();
    for cpos in rdr {
        for player in [BLACK, WHITE] {
            if cpos.state(player) != INVALID_POS && cpos.state(player.opponent()) == INVALID_POS {
                // player's KING in check
                if let None = cpos
                    .move_iterator(player)
                    .filter(|&mv| cpos.apply(mv).valid(player.opponent()))
                    .next()
                {
                    mate[player as usize] += 1;
                    assert!(cpos.state(player) == MATE);
                    for mv in cpos.reverse_move_iterator(player) {
                        if !mv.is_capture_by_pawn() && !mv.is_promotion() {
                            let can_mate = cpos.unapply(mv, EMPTY);
                            if can_mate.valid(player.opponent()) {
                                let canonic_can_mate = can_mate.mk_canonical(); // find(&mut egtbmap)?;
                                let winner = if canonic_can_mate.canonic_has_bw_switched() {
                                    player
                                } else {
                                    player.opponent()
                                };
                                // assert!(canonic_can_mate.state(winner) == CAN_MATE);
                                canm[winner as usize] += 1;
                            }
                        }
                    }
                } else {
                    assert!(cpos.state(player) != MATE);
                }
            }
        }
    }
    for player in [BLACK, WHITE] {
        println!("{:?} is mate: {}", player, formatted_sz(mate[player as usize]));
        println!("{:?} can mate: {}", player, formatted_sz(canm[player as usize]));
    }
    Ok(())
}
