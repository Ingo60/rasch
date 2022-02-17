//! Move generation for CPos
//!

use std::{
    alloc::{alloc, dealloc, Layout},
    collections::HashSet,
    slice,
};

use super::{
    basic::{Move, Piece, Player},
    cpos::{CPos, Signature},
    // cposio::{cpos_file_size, mk_egtb_path, CPosReader},
    // fen::encodeFEN,
    fieldset::{BitSet, Field},
    mdb,
    position::{bit, pieceTargets},
    util::formatted_h,
};

// use CPosState::*;
// use Field::*;
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
///     for mv in cpos.move_iterator(player) {
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

/// Like [CPosReverseMoveIterator], but doesn't generate [PAWN] promotions or captures.
///
/// Put differently, when results are used with [CPos::unapply_nc], the positions obtained
/// have the same signature as the iterated over position.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CPosSimpleReverseMoveIterator {
    cpos: CPos,
    player: Player,
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

impl CPosSimpleReverseMoveIterator {
    pub fn new(p: CPos, player: Player) -> CPosSimpleReverseMoveIterator {
        let mut this = CPosSimpleReverseMoveIterator {
            cpos: p,
            player,
            whites: BitSet::empty(),
            occupied: BitSet::empty(),
            index: 4,
            sources: BitSet::empty(),
        };
        let wkbit = bit(p.white_king());
        this.occupied = this.occupied + wkbit;
        this.whites = this.whites + wkbit;
        this.occupied = this.occupied + bit(p.black_king());

        // Fill whites and occupieds
        for u in 0..4 {
            let piece = p.piece_at(u);
            if piece != EMPTY {
                let f = p.field_at(u);
                let fbs = bit(f);
                this.occupied = this.occupied + fbs;
                if p.player_at(u) == WHITE {
                    this.whites = this.whites + fbs;
                }
            }
        }

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

impl Iterator for CPosSimpleReverseMoveIterator {
    type Item = Move;
    /// Give us the next **raw** backwards move. Must be checked for validity.
    fn next(&mut self) -> Option<Self::Item> {
        let me = self.player.opponent();
        while self.index < 5 {
            match self.sources.next() {
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
                            if !mv.is_capture_by_pawn() {
                                let es = match me {
                                    WHITE => mdb::canWhitePawn(from, to) - bit(to),
                                    BLACK => mdb::canBlackPawn(from, to) - bit(to),
                                };

                                if self.all_empty(es) {
                                    return Some(mv);
                                }
                            }
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

pub fn test2(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    println!("{} predecessors:", signature);
    let relatives = signature.get_relatives();
    for r in relatives {
        println!("{:?}", r);
    }

    Ok(())
}

pub fn test3(sig: &str) -> Result<(), String> {
    let signature = Signature::new_from_str_canonic(sig)?;
    let mut n_items = 0;
    for cpos in signature.first() {
        for p in [BLACK, WHITE] {
            n_items += 1;
            for (prev, mv) in cpos.predecessors(p) {
                let rpos = prev.apply(mv).light_canonical();
                if rpos != cpos {
                    println!("{}  cpos {:5?} {:?}", signature, p, cpos);
                    println!("{}  prev {:4}  {:?}", signature, mv, prev);
                    println!("{}  result     {:?}", signature, rpos);
                    return Err("failed".to_uppercase());
                }
            }
        }
    }
    println!("{}  items tried: {}", signature, n_items);
    Ok(())
}

/// Does it ever happen that SigA and canonic(SigA) are both predecessors of SigB?
/// ```
/// rasch::cposmove::test4().unwrap();
/// ```
pub fn test4() -> Result<(), String> {
    let sigs = all_signatures();
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

pub fn all_signatures() -> HashSet<Signature> {
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
    sigs
}

/// check out space requirements
pub fn test5(sig: &str) -> Result<(), String> {
    let mut all = all_signatures();
    if sig != "all" {
        let signature = Signature::new_from_str_canonic(sig)?;
        all.clear();
        all.insert(signature);
    }
    for signature in all {
        let fst = signature.first();
        let lst = signature.last();
        let n_bytes = lst.canonic_addr().bytes();
        println!(
            "{}    {:?}..{:?}  {}bytes",
            signature,
            // fst,
            // lst,
            fst.canonic_addr(),
            lst.canonic_addr(),
            formatted_h(n_bytes, '1')
        );
        let layout = Layout::array::<u8>(n_bytes).map_err(|e| format!("{}", e))?;
        let ptr = unsafe { alloc(layout) };
        let array = unsafe { slice::from_raw_parts_mut(ptr, n_bytes) };
        array.fill(0xff);
        for curr in signature.first() {
            let addr = curr.canonic_addr();
            let nibble = addr.get_nibble(array);
            if nibble != 0xf {
                eprintln!("{:?}   {:?} was already used {:#02x}", curr, addr, nibble);
                for dbl in signature.first().filter(|s| *s < curr) {
                    if dbl.canonic_addr() == addr {
                        eprintln!("{:?}   used it first", dbl);
                        return Err("DOUBLES".to_string());
                    }
                }
            }
            addr.set_nibble(array, 0);
        }
        let mut zeroed = 0usize;
        for u in &array[..] {
            if *u & 0xf0 == 0 {
                zeroed += 1;
            }
            if *u & 0x0f == 0 {
                zeroed += 1;
            }
        }
        println!("{}   {}‰ nibbles used", signature, zeroed * 1000 / (n_bytes * 2));
        unsafe {
            dealloc(ptr, layout);
        }
    }
    Ok(())
}
