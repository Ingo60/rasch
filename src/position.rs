//!   # The central data structure, the `Position`
//!
//! A `Position` encodes the state of the chess board.
//!
//! * which pieces do exist and where are they placed
//! * who's turn it is
//! * the castling rights of the two players
//! * whether and where a pawn can capture en-passant
//! * the number of half moves since the last pawn move or capture

#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code
#![allow(non_upper_case_globals)] // as well as this

// use standard packages
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
// use std::iter::FromIterator;

use super::fieldset::BitSet;
use super::fieldset::Field;
use super::fieldset::Field::*;
use super::mdb;
use super::zobrist;

/// short form of BitSet::singleton
pub const fn bit(f: Field) -> BitSet { BitSet::singleton(f) }

/// short form of BitSet::bitIndex
pub fn fld(b: BitSet) -> Field { b.bitIndex() }

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
#[repr(u32)]
pub enum Player {
    BLACK,
    WHITE,
}

impl From<bool> for Player {
    /// if true then it's WHITE, otherwise FALSE
    fn from(b: bool) -> Player {
        if b {
            WHITE
        } else {
            BLACK
        }
    }
}

impl Player {
    /// the color of the opponent
    pub fn opponent(self) -> Player {
        match self {
            Player::BLACK => Player::WHITE,
            Player::WHITE => Player::BLACK,
        }
    }

    /// compute -1 or 1 without conditional branch
    ///
    /// ```
    /// use rasch::position as P;
    /// assert_eq!(P::WHITE.factor(), 1);
    /// assert_eq!(P::BLACK.factor(), -1);
    /// ```
    pub fn factor(self) -> i32 { 2 * (self as i32) - 1 }
}

// pub const BLACK: Player = Player::BLACK;
// pub const WHITE: Player = Player::WHITE;
pub use Player::*;

/// Enumeration of the chess pieces
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
#[repr(u32)]
pub enum Piece {
    EMPTY,
    PAWN,
    KNIGHT,
    BISHOP,
    ROOK,
    QUEEN,
    KING,
}

impl Display for Piece {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result { write!(f, "{}", self.show()) }
}

impl From<u32> for Piece {
    /// The inverse of `piece as u32`.
    /// Will panic! if not in range 0..6
    ///
    /// ```
    /// use rasch::position::Piece;
    ///
    /// assert!((0..6).all(|u| Piece::from(u) as u32 == u));
    /// ```
    fn from(u: u32) -> Piece {
        match u {
            0 => EMPTY,
            1 => PAWN,
            2 => KNIGHT,
            3 => BISHOP,
            4 => ROOK,
            5 => QUEEN,
            6 => KING,
            _ => panic!(format!("can't cast {} to Piece", u)),
        }
    }
}

impl Piece {
    /// Produce a single letter String for a Piece: -PBNRQK
    pub fn show(self) -> String {
        String::from(match self {
            EMPTY => "-",
            PAWN => "P",
            BISHOP => "B",
            KNIGHT => "N",
            ROOK => "R",
            QUEEN => "Q",
            KING => "K",
        })
    }

    /// Encode the 3 bit information (4 in pawnSet, 2 in bishopSet, 1 in
    /// rookSet) into a Piece. Any value >6 results in EMPTY, as well as
    /// 0 (which is proper)
    pub fn encodePBR(pbr: u8) -> Piece {
        match pbr {
            0b001 => ROOK,
            0b010 => BISHOP,
            0b011 => QUEEN,
            0b100 => PAWN,
            0b101 => KING,
            0b110 => KNIGHT,
            _ => EMPTY,
        }
    }
}

// pub const EMPTY: Piece = Piece::EMPTY;
// pub const PAWN: Piece = Piece::PAWN;
// pub const BISHOP: Piece = Piece::BISHOP;
// pub const KNIGHT: Piece = Piece::KNIGHT;
// pub const ROOK: Piece = Piece::ROOK;
// pub const QUEEN: Piece = Piece::QUEEN;
// pub const KING: Piece = Piece::KING;
use Piece::*;

#[rustfmt::skip]
//                  Board Geometry
//      8        7        6       5         4        3       2        1
//  hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba
//  10101010 01010101 11001100 00110011 01100110 11001100 00110011 01100110

/// All the information that is needed for board positions.
///
/// Note that illegal states **are** representable, but
/// - there will be the initial position
/// - and the possibility to apply a `Move`
/// If the initial position and the application of moves is correct
/// and the move generator does indeed generate
/// valid moves only, then it should be impossible to reach an illegal
/// state.

#[derive(Clone, Copy, Debug)]

pub struct Position {
    /// The flags bitset informs about castling rights, en passant
    /// position and who's turn it is. In addition, whether the
    /// kings actually did castle, the 50-moves-rule ply counter and
    /// the distance to root ply counter.
    ///
    /// - If A1 is in the set, it is 'WHITE's turn, otherwise 'BLACK's.
    /// - If en passant is possible, one of  A3..H3 or A6..H6 is in the
    ///   set
    /// - If castling is possible, the corresponding target fields of
    ///   the Kings are in the set. This would be G1 (white kingside),
    ///   C1 (white queenside), G8 (black kingside) or C8 (black
    ///   queenside).
    /// - If castling actually was performed, F1, D1, F8 or G8 are set
    ///   (the field that the 'KING' skipped).
    /// - The bits 'A4' .. 'H4' contain the unsigned half-move counter.
    ///   It is reset to 0 on a 'PAWN' move or a capturing move. For all
    ///   other moves, it is incremented.
    /// - The bits 'A5' .. 'H5' contain the unsigned root-move counter.
    ///   It is incremented with every move, but reset to 0 after a
    ///   usermove was carried out. Hence, in searches, one can see how
    ///   many levels deep in the search we are (distance to root),
    ///   while the "depth" parameter of said search functions gives the
    ///   distance to the horizon.
    ///
    /// Note: the counter bits and the F1, D1, F8 and G8 bits are not
    /// reflected in the hash key.
    pub flags: BitSet,

    /// the fields occupied by WHITE pieces
    pub whites: BitSet,

    /// For encoding of figures, we use only 3 sets instead of 6, namely
    /// 'Position.pawnSet', Position.bishopSet' and
    /// 'Position.rookSet', with the following convention:
    ///
    /// ```text
    /// Set       P      B     R
    /// PAWN      x      -     -
    /// KNIGHT    x      x     -
    /// BISHOP    -      x     -
    /// ROOK      -      -     x
    /// QUEEN     -      x     x
    /// KING      x      -     x
    /// ```
    ///
    /// For example, if the bit E5 is set in the pawnSet and the
    /// rookSet, but not in the bishopSet, then there is a KING on
    /// E5. If the E5 flag is set in whites, then it is a white
    /// king, else a black king.
    pub pawnSet:   BitSet,
    pub bishopSet: BitSet,
    pub rookSet:   BitSet,

    /// the Zobrist hash key, a special hashing method usually employed
    /// in chess programming
    ///
    /// The following is guaranteed:
    ///
    /// ```text
    /// x.hash != y.hash → x != y
    /// x == y           → x.hash == y.hash
    /// ```
    pub hash: u64,
}

/// Bitmask for selection of the ply (half-move) counter, which is used to support the 50 moves rule.
/// Note that we have room for 256 plies, but only 100 are needed.
///
/// The evaluator will return 0 for a position where the ply counter is 100 or more.
/// Also, the protocol handler will claim a draw when the move played results in a position
/// with ply counter 100 or more.
/// 
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// assert!(P::plyCounterBits.member(F::Field::A4));
/// assert!(P::plyCounterBits.member(F::Field::B4));
/// assert!(P::plyCounterBits.member(F::Field::C4));
/// assert!(P::plyCounterBits.member(F::Field::D4));
/// assert!(P::plyCounterBits.member(F::Field::E4));
/// assert!(P::plyCounterBits.member(F::Field::F4));
/// assert!(P::plyCounterBits.member(F::Field::G4));
/// assert!(P::plyCounterBits.member(F::Field::H4));
/// assert_eq!(P::plyCounterBits.card(), 8);
/// ```
#[rustfmt::skip]
pub const plyCounterBits: BitSet = BitSet { bits: 0xFF00_0000u64 }; // A4..H4

/// how many bits to shift right to get the ply counter
/// ```
/// use rasch::position as P;
/// assert_eq!(P::plyCounterBits.bits >> P::plyCounterShift, 255);
/// ```
pub const plyCounterShift: u32 = 24;

/// Bitmask for selection of the distance to root ply counter
/// This counter is reset to 0 before a seach starts and incremented with every move
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// assert!(P::rootCounterBits.member(F::Field::A5));
/// assert!(P::rootCounterBits.member(F::Field::B5));
/// assert!(P::rootCounterBits.member(F::Field::C5));
/// assert!(P::rootCounterBits.member(F::Field::D5));
/// assert!(P::rootCounterBits.member(F::Field::E5));
/// assert!(P::rootCounterBits.member(F::Field::F5));
/// assert!(P::rootCounterBits.member(F::Field::G5));
/// assert!(P::rootCounterBits.member(F::Field::H5));
/// assert_eq!(P::rootCounterBits.card(), 8);
/// ```
#[rustfmt::skip]
pub const rootCounterBits: BitSet = BitSet { bits: 0xFF_0000_0000 }; // A5..H5

/// number of bits to shift right to get the ply counter
/// ```
/// use rasch::position as P;
/// assert_eq!(P::rootCounterBits.bits >> P::rootCounterShift, 255);
/// ```
pub const rootCounterShift: u32 = 32;

/// A bitmask used to turn all the counter bits off
///
/// ```
/// use rasch::position as P;
/// assert_eq!(P::counterBits, P::rootCounterBits + P::plyCounterBits);
/// ```
pub const counterBits: BitSet = BitSet {
    bits: 0x0000_00FF_FF00_0000,
};

#[rustfmt::skip]
/// Bitmask for selection of the bits that indicate the rights to castle
/// 
/// ```
/// use rasch::position as P;
/// assert_eq!(P::castlingBits, P::whiteCastlingRights + P::blackCastlingRights)
/// ```
pub const castlingBits: BitSet = BitSet { bits: 0x4400_0000_0000_0044 }; // G1, C1, G8, C8

#[rustfmt::skip]
/// Bitmask for selection of the bits that indicate the rights to castle for WHITE
/// 
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// assert!   (P::whiteCastlingRights.member(F::Field::G1));
/// assert!   (P::whiteCastlingRights.member(F::Field::C1));
/// assert_eq!(P::whiteCastlingRights.card(), 2);
/// ```
pub const whiteCastlingRights: BitSet = BitSet { bits: 0x0000_0000_0000_0044 }; // G1, C1

#[rustfmt::skip]
/// Bitmask for selection of the bits that indicate the rights to castle for BLACK
/// 
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// assert!   (P::blackCastlingRights.member(F::Field::G8));
/// assert!   (P::blackCastlingRights.member(F::Field::C8));
/// assert_eq!(P::blackCastlingRights.card(), 2);
/// ```
pub const blackCastlingRights: BitSet = BitSet { bits: 0x4400_0000_0000_0000 }; // G8, C8

#[rustfmt::skip]
/// Bitmask for selection of the bits that indicate the fields that were skipped when the king castled
/// 
/// ```
/// use rasch::position as P;
/// assert_eq!(P::castlingDoneBits, P::whiteHasCastledBits + P::blackHasCastledBits)
/// ```
pub const castlingDoneBits: BitSet = BitSet { bits: 0x2800_0000_0000_0028 }; // F1, D1, F8, D8

#[rustfmt::skip]
/// Bitmask for selection of the bits that tell us whether the white king has castled and whereto.
///
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// assert!   (P::whiteHasCastledBits.member(F::Field::F1));
/// assert!   (P::whiteHasCastledBits.member(F::Field::D1));
/// assert_eq!(P::whiteHasCastledBits.card(), 2);
/// ```
pub const whiteHasCastledBits: BitSet = BitSet { bits: 0x0000_0000_0000_0028 }; // F1, D1

#[rustfmt::skip]
/// Bitmask for selection of the bits that tell us whether the black king has castled and whereto.
/// 
/// ```
/// use rasch::position as P;
/// use rasch::fieldset as F;
/// assert!   (P::blackHasCastledBits.member(F::Field::F8));
/// assert!   (P::blackHasCastledBits.member(F::Field::D8));
/// assert_eq!(P::blackHasCastledBits.card(), 2);
/// ```
pub const blackHasCastledBits: BitSet = BitSet { bits: 0x2800_0000_0000_0000 }; // F8, D8

/// give the bitmask that can be used to find out whether a given player
/// has castled
pub const fn playerCastledBits(p: Player) -> BitSet {
    match p {
        Player::BLACK => blackHasCastledBits,
        Player::WHITE => whiteHasCastledBits,
    }
}

#[rustfmt::skip]
/// Bitmask to select the Bits that can be targets of an en-passant capture (rank 3 and rank 6)
/// If any of those is set in the position flags, an en passant capture is possible
pub const enPassantBits: BitSet = BitSet { bits: 0x0000_FF00_00FF_0000 }; // A3-H3, A6-H6

/// constant to add or subtract 1 from both counters in one go
/// It goes without saying that we must never decrement beyond zero, nor
/// increment beyond 255.
pub const onePly: u64 = 0x1_0100_0000; // A4 and A5

/// This is used when only the root counter must be incremented (on
/// 'PAWN' moves and captures)
pub const onePlyRootOnly: u64 = 0x1_0000_0000; // A5

/// Bit mask that indicates WHITE is to move
pub const whiteToMove: BitSet = bit(A1);
/// Bit mask that indicates BLACK is to move
pub const blackToMove: BitSet = BitSet::empty();

/// Zobrist hash value commonly used: A1
static flagZobristA1: u64 = zobrist::flagZobrist(A1 as u32);
/// Zobrist hash value commonly used: C1
static flagZobristC1: u64 = zobrist::flagZobrist(C1 as u32);
/// Zobrist hash value commonly used: G1
static flagZobristG1: u64 = zobrist::flagZobrist(G1 as u32);
/// Zobrist hash value commonly used: C8
static flagZobristC8: u64 = zobrist::flagZobrist(C8 as u32);
/// Zobrist hash value commonly used: G8
static flagZobristG8: u64 = zobrist::flagZobrist(G8 as u32);

/// Move sequences for castlings consist of 3 moves to get the "who is
/// to move" bit right. Castling rights are taken care of by the
/// ordinary move logic. Since this happens maybe once in a game,
/// efficiency is less important.
const whiteShortCastlingMove1: Move = Move::new(WHITE, KING, EMPTY, E1, F1);
const whiteShortCastlingMove2: Move = Move::new(WHITE, KING, EMPTY, F1, G1);
const whiteShortCastlingMove3: Move = Move::new(WHITE, ROOK, EMPTY, H1, F1);
const whiteLongCastlingMove1: Move = Move::new(WHITE, KING, EMPTY, E1, D1);
const whiteLongCastlingMove2: Move = Move::new(WHITE, KING, EMPTY, D1, C1);
const whiteLongCastlingMove3: Move = Move::new(WHITE, ROOK, EMPTY, A1, D1);
const blackShortCastlingMove1: Move = Move::new(BLACK, KING, EMPTY, E8, F8);
const blackShortCastlingMove2: Move = Move::new(BLACK, KING, EMPTY, F8, G8);
const blackShortCastlingMove3: Move = Move::new(BLACK, ROOK, EMPTY, H8, F8);
const blackLongCastlingMove1: Move = Move::new(BLACK, KING, EMPTY, E8, D8);
const blackLongCastlingMove2: Move = Move::new(BLACK, KING, EMPTY, D8, C8);
const blackLongCastlingMove3: Move = Move::new(BLACK, ROOK, EMPTY, A8, D8);

/// Helper function that turns `false` into `0u64` and `true` into
/// `0xffff_ffff_ffff_ffffu64`
#[inline]
pub const fn boolMask(b: bool) -> u64 {
    match b {
        false => 0,
        true => 0xffff_ffff_ffff_ffffu64,
    }
}

impl Position {
    /// the set of fields that are occupied by PAWNS
    pub fn pawns(&self) -> BitSet { (self.pawnSet - self.bishopSet) - self.rookSet }

    /// the set of fields that are occupied by KNIGHTS
    pub fn knights(&self) -> BitSet { (self.pawnSet * self.bishopSet) - self.rookSet }

    /// the set of fields that are occupied by BISHOPS
    pub fn bishops(&self) -> BitSet { (self.bishopSet - self.pawnSet) - self.rookSet }

    /// the set of fields that are occupied by ROOKS
    pub fn rooks(&self) -> BitSet { (self.rookSet - self.bishopSet) - self.pawnSet }

    /// the set of fields that are occupied by QUEENS
    pub fn queens(&self) -> BitSet { (self.rookSet * self.bishopSet) - self.pawnSet }

    /// the set of fields that are occupied by KINGS
    pub fn kings(&self) -> BitSet { (self.pawnSet * self.rookSet) - self.bishopSet }

    /// get the number of `Move`s applied since the last pawn move or
    /// capture (Castling, despite technically doing 3 moves,
    /// corrects the counter acordingly)
    pub fn getPlyCounter(&self) -> u64 { (self.flags * plyCounterBits).bits >> plyCounterShift }

    /// get the number of `Move`s applied since the last root counter
    /// reset
    pub fn getRootDistance(&self) -> u64 { (self.flags * rootCounterBits).bits >> rootCounterShift }

    /// clear the 50-move ply counter
    pub fn clearPlyCounter(&self) -> Position {
        Position {
            flags: self.flags - plyCounterBits,
            ..*self
        }
    }

    /// clear the root ply counter
    pub fn clearRootPlyCounter(&self) -> Position {
        Position {
            flags: self.flags - rootCounterBits,
            ..*self
        }
    }

    /// Increment the ply counter(s) using either `onePly` or
    /// `onePlyRootOnly`
    pub fn incrPlyCounters(&self, mask: u64) -> Position {
        Position {
            flags: BitSet {
                bits: self.flags.bits + mask,
            },
            ..*self
        }
    }

    /// Decrement both counters
    /// Note: it will never be necessary to decrement only one counter.
    /// Only used after castling, where we actually do 3 ordinary (but
    /// illegal) moves.
    pub fn decrPlyCounters(&self) -> Position {
        Position {
            flags: BitSet {
                bits: self.flags.bits - onePly,
            },
            ..*self
        }
    }

    /// subtract 2 from the plyCounters (conveniece for castlings)
    pub fn correctPlyCounterForCastling(&self) -> Position { self.decrPlyCounters().decrPlyCounters() }

    /// the set of occupied fields
    pub fn occupied(&self) -> BitSet { self.pawnSet + self.bishopSet + self.rookSet }

    /// `true` if and only if the given `Field` is not occupied by some
    /// piece
    pub fn isEmpty(&self, f: Field) -> bool { !self.occupied().member(f) }

    /// `true` if and only if no member of the given set is an occupied
    /// field
    pub fn areEmpty(&self, fs: BitSet) -> bool { (self.occupied() * fs).null() }

    /// `true` if and only if the given `Field` is occupied by WHITE
    ///
    /// Note that `false` does **not** imply it's occupied by BLACK, it
    /// could just be empty. But if you already know that the field
    /// **is** occupied, this'll save a few cycles consulting the
    /// pawn, rook and bishop sets.
    pub fn isWhite(&self, f: Field) -> bool { self.whites.member(f) }

    /// tell who's turn it is
    pub fn turn(&self) -> Player {
        if self.flags.member(A1) {
            WHITE
        } else {
            BLACK
        }
    }

    /// fields occupied by WHITE
    pub fn occupiedByWhite(&self) -> BitSet { self.occupied() * self.whites }

    /// fields occupied by BLACK
    pub fn occupiedByBlack(&self) -> BitSet { self.occupied() - self.whites }

    /// fields occupied by Player
    pub fn occupiedBy(&self, p: Player) -> BitSet {
        match p {
            Player::WHITE => self.occupiedByWhite(),
            Player::BLACK => self.occupiedByBlack(),
        }
    }

    /// fields occupied by the player who's turn it is
    pub fn occupiedByActive(&self) -> BitSet { self.occupiedBy(self.turn()) }

    /// Tell us the piece that occupies some Field.
    #[allow(clippy::many_single_char_names)] // because we are no snowflakes
    pub fn pieceOn(&self, f: Field) -> Piece {
        let s = bit(f);
        let p = if (s * self.pawnSet).null() { 0 } else { 4 };
        let b = if (s * self.bishopSet).null() { 0 } else { 2 };
        let r = if (s * self.rookSet).null() { 0 } else { 1 };
        Piece::encodePBR(p + b + r)
    }

    /// Tell us the player occupying some Field.
    pub fn playerOn(&self, f: Field) -> Option<Player> {
        if self.occupied().member(f) {
            Some(Player::from(self.isWhite(f)))
        } else {
            None
        }
    }

    /// compute the hash
    pub fn computeZobrist(&self) -> u64 {
        let flagz = ((self.flags - counterBits) - castlingDoneBits)
            // .into_iter()
            .fold(0u64, |acc, f| acc ^ super::zobrist::flagZobrist(f as u32));
        self.occupied()
            // .into_iter()
            .fold(flagz, |acc, f| {
                let player = Player::from(self.isWhite(f));
                let piece = self.pieceOn(f);
                acc ^ zobrist::ppfZobrist(player as u32, piece as u32, f as u32)
            })
    }

    /// rehash the Position, will be done once with each move
    ///
    /// ```
    /// use rasch::mdb;
    /// use rasch::position as P;
    /// use rasch::fieldset::Field::*;
    /// use rasch::position::Player::*;
    /// use rasch::position::Piece::*;
    /// mdb::initStatic();
    /// let p = P::initialBoard();
    /// assert_eq!(0xc49244f1909c1fabu64, p.hash);
    /// let m = P::Move::new(WHITE, PAWN, EMPTY, E2, E4);
    /// let p2 = p.apply(m);
    /// let p3 = p2.rehash();
    /// assert_eq!(p2.hash, p3.hash);
    /// ```
    pub fn rehash(&self) -> Position {
        Position {
            hash: self.computeZobrist(),
            ..*self
        }
    }

    /// Like `attacked`, but reports attacks by `PAWN`s only. O(1)
    pub fn attackedByPawns(&self, wo: Field, durch: Player) -> BitSet {
        let attackers = match durch {
            WHITE => mdb::targetOfWhitePawns(wo),
            BLACK => mdb::targetOfBlackPawns(wo),
        };
        match durch {
            WHITE => (self.pawns() * attackers) * self.whites,
            BLACK => (self.pawns() * attackers) - self.whites,
        }
    }

    /// Like `attacked`, but reports attacks by `KNIGHT`s only. O(1)
    pub fn attackedByKnights(&self, wo: Field, durch: Player) -> BitSet {
        let attackers = self.knights() * mdb::knightTargets(wo);
        match durch {
            WHITE => (self.knights() * attackers) * self.whites,
            BLACK => (self.knights() * attackers) - self.whites,
        }
    }

    /// Like `attacked`, but reports attacks by `KINGS`s only. O(1)
    pub fn attackedByKings(&self, wo: Field, durch: Player) -> BitSet {
        let attackers = self.kings() * mdb::kingTargets(wo);
        match durch {
            WHITE => (self.kings() * attackers) * self.whites,
            BLACK => (self.kings() * attackers) - self.whites,
        }
    }

    /// Like `attacked`, but reports attacks by `BISHOP`s only. Needs 1
    /// iteration through attacker's bishops
    pub fn attackedByBishops(&self, wo: Field, durch: Player) -> BitSet {
        let bishops = match durch {
            // find the attacker's bishops
            WHITE => self.bishops() * self.whites,
            BLACK => self.bishops() - self.whites,
        };
        self.validBishopTargets(wo, bishops)
    }

    /// Like `attacked`, but reports attacks by `ROOK`s only. Needs 1
    /// iteration through attacker's rooks.
    pub fn attackedByRooks(&self, wo: Field, durch: Player) -> BitSet {
        let rooks = match durch {
            // find the attacker's rooks
            WHITE => self.rooks() * self.whites,
            BLACK => self.rooks() - self.whites,
        };
        self.validRookTargets(wo, rooks)
    }

    /// Like `attacked`, but reports attacks by `QUEEN`s only. Needs 2
    /// iterations through attacker's queens.
    pub fn attackedByQueens(&self, wo: Field, durch: Player) -> BitSet {
        let queens = match durch {
            // find the attacker's queens
            WHITE => self.queens() * self.whites,
            BLACK => self.queens() - self.whites,
        };
        self.validRookTargets(wo, queens) + self.validBishopTargets(wo, queens)
    }

    /// Like `attackedByBishops` but reports also QUEENs  attacking with
    /// BISHOP moves. Needs 1 iteration through attacker's queens and
    /// bishops.
    pub fn attackedByBishopsOrQueens(&self, wo: Field, durch: Player) -> BitSet {
        let pieces = match durch {
            // find the attacker's bishops and queens
            WHITE => (self.bishops() + self.queens()) * self.whites,
            BLACK => (self.bishops() + self.queens()) - self.whites,
        };
        self.validBishopTargets(wo, pieces)
    }

    /// Like `attackedByRooks` but reports also QUEENs  attacking with
    /// ROOK moves. Needs 1 iteration through attacker's queens and
    /// rooks.
    pub fn attackedByRooksOrQueens(&self, wo: Field, durch: Player) -> BitSet {
        let pieces = match durch {
            // find the attacker's rooks and queens
            WHITE => (self.rooks() + self.queens()) * self.whites,
            BLACK => (self.rooks() + self.queens()) - self.whites,
        };
        self.validRookTargets(wo, pieces)
    }

    /// Tell if the indicated field is attacked by a piece of the
    /// indicated player. This is faster than
    ///
    /// ```text
    ///     !position.attacked(f,p).null()
    /// ```
    ///
    /// because it can short circuit whenever an attacker is found. And
    /// of course it tries the pieces first that have an O(1) time
    /// complexity. In the worst case, when the attack is from a ROOK or
    /// QUEEN it'll need 2 iterations.
    pub fn isAttacked(&self, wo: Field, durch: Player) -> bool { !self.isNotAttacked(wo, durch) }

    /// the opposite of isAttacked
    pub fn isNotAttacked(&self, wo: Field, durch: Player) -> bool {
        self.attackedByPawns(wo, durch).null()
            && self.attackedByKnights(wo, durch).null()
            && self.attackedByKings(wo, durch).null()
            && self.attackedByBishopsOrQueens(wo, durch).null()
            && self.attackedByRooksOrQueens(wo, durch).null()
    }

    /// Given a `Position`, a `Field` and a `Player` computes the set of
    /// fields that are occupied  by a `Piece` of `Player`
    /// that could move to the given `Field` (and capture).
    ///
    /// Note that computation for `QUEEN`, `ROOK` and `BISHOP` attackers
    /// require two iterations. Therefore, whenever one just needs
    /// to know *whether* the `Field` is attacked or not, use
    /// `isAttacked`
    ///
    /// ```
    /// use rasch::mdb;
    /// use rasch::position as P;
    /// use rasch::fieldset as F;
    /// use rasch::fieldset::Field::*;
    /// use rasch::position::Player::*;
    /// use rasch::position::Piece::*;
    /// mdb::initStatic();
    /// let p = P::initialBoard();
    /// eprintln!("should be {}", p.attacked(C3, WHITE));
    /// assert_eq!(p.attacked(C3, WHITE), [B1, B2, D2].iter().collect());
    /// ```

    pub fn attacked(&self, wo: Field, durch: Player) -> BitSet {
        self.attackedByPawns(wo, durch)
            + self.attackedByKnights(wo, durch)
            + self.attackedByKings(wo, durch)
            + self.attackedByBishopsOrQueens(wo, durch)
            + self.attackedByRooksOrQueens(wo, durch)
    }

    /// Tell if `Players` KING stands in check.
    pub fn inCheck(&self, wer: Player) -> bool {
        let king = match wer {
            WHITE => self.kings() * self.whites,
            BLACK => self.kings() - self.whites,
        };
        // an invalid position without kings will crash fld()
        self.isAttacked(fld(king), wer.opponent())
    }

    /// Tell if current position is ok in that the **passive** player is
    /// not in check. Can be used after having applied a user's move
    /// or a speculative move to see if it was possible at all.
    ///
    /// ```
    /// use rasch::mdb;
    /// use rasch::position as P;
    /// mdb::initStatic();
    /// assert!(P::initialBoard().notInCheck());
    /// ```
    pub fn notInCheck(&self) -> bool {
        let player = self.turn();
        let otherking = match player {
            WHITE => self.kings() - self.whites,
            BLACK => self.kings() * self.whites,
        };
        // an invalid position without kings will crash fld()
        self.isNotAttacked(fld(otherking), player)
    }

    /// Where does a BISHOP on a given field attack in the current
    /// position, taking actual board status in account? Result can
    /// be restricted to certain set of fields to save iteration
    /// cycles or all results can be kept using BitSet::all()
    ///
    /// To find this out, we must loop through the set of general
    /// targets (restricted by the mask) and check if the
    /// intermediate fields, if any, are all empty.
    ///
    /// Note that the current position must not actually have a BISHOP
    /// on the from field. Indeed, this function can be used in
    /// determining whether a certain field is attacked by a BISHOP.
    /// This is so because BISHOP moves are symmetric. That is, if a
    /// BISHOP standing on the from field could attack a BISHOP
    /// somewhere this means the from field is also attacked by that
    /// BISHOP.
    pub fn validBishopTargets(&self, from: Field, mask: BitSet) -> BitSet {
        let targets = mdb::bishopTargets(from) * mask;
        // chances are we can avoid all the iteration overhead
        if targets.null() {
            targets
        } else {
            targets.filter(|to| self.areEmpty(mdb::canBishop(from, *to))).collect()
        }
    }

    /// like `validBishopTargets` but for ROOKs
    pub fn validRookTargets(&self, from: Field, mask: BitSet) -> BitSet {
        let targets = mdb::rookTargets(from) * mask;
        if targets.null() {
            targets
        } else {
            targets.filter(|to| self.areEmpty(mdb::canRook(from, *to))).collect()
        }
    }

    /// Give the least attacking piece, if any
    pub fn leastAttacker(&self, wo: Field, durch: Player) -> Piece {
        match () {
            () if self.attackedByPawns(wo, durch).some() => PAWN,
            () if self.attackedByKnights(wo, durch).some() => KNIGHT,
            () if self.attackedByBishops(wo, durch).some() => BISHOP,
            () if self.attackedByRooks(wo, durch).some() => ROOK,
            () if self.attackedByQueens(wo, durch).some() => QUEEN,
            () if self.attackedByKings(wo, durch).some() => KING,
            () => EMPTY,
        }
    }

    /// Apply a move in a position and return the resulting position.
    /// The move should be one of the moves generated for this position.
    pub fn apply(&self, mv: Move) -> Position {
        eprintln!(
            "applying {:?} {:?} {:?} {} {}",
            mv.player(),
            mv.piece(),
            mv.promote(),
            mv.from(),
            mv.to()
        );
        eprintln!("to {}", self);
        if mv.piece() == KING && mv.promote() != EMPTY {
            self.applyCastling(mv)
        } else {
            self.applyOrdinary(mv)
        }
    }

    /// Helper function to place pieces on a board, removing any that
    /// occupy the indicated fields. Can be given EMPTY to remove.
    ///
    /// Not for public use as it leaves the hash in an
    /// illegal state.
    fn place(&self, player: Player, piece: Piece, mask: BitSet) -> Position {
        let whites = match player {
            WHITE if piece != EMPTY => self.whites + mask,
            _other => self.whites - mask,
        };
        let pawnSet = match piece {
            PAWN | KING | KNIGHT => self.pawnSet + mask,
            _other => self.pawnSet - mask,
        };
        let bishopSet = match piece {
            BISHOP | QUEEN | KNIGHT => self.bishopSet + mask,
            _other => self.bishopSet - mask,
        };
        let rookSet = match piece {
            ROOK | QUEEN | KING => self.rookSet + mask,
            _other => self.rookSet - mask,
        };
        Position {
            whites,
            pawnSet,
            bishopSet,
            rookSet,
            ..*self
        }
    }

    /// Helper to set a certain bit in flags
    fn setFlag(&self, f: Field) -> Position {
        Position {
            flags: self.flags + bit(f),
            ..*self
        }
    }

    /// Apply a castling move. Used only by `apply`
    fn applyCastling(&self, mv: Move) -> Position {
        let castled = match mv.player() {
            WHITE => match mv.promote() {
                KING => self
                    .applyOrdinary(whiteShortCastlingMove1)
                    .applyOrdinary(whiteShortCastlingMove2)
                    .applyOrdinary(whiteShortCastlingMove3)
                    .setFlag(F1),
                _other => self
                    .applyOrdinary(whiteLongCastlingMove1)
                    .applyOrdinary(whiteLongCastlingMove2)
                    .applyOrdinary(whiteLongCastlingMove3)
                    .setFlag(D1),
            },
            BLACK => match mv.promote() {
                KING => self
                    .applyOrdinary(blackShortCastlingMove1)
                    .applyOrdinary(blackShortCastlingMove2)
                    .applyOrdinary(blackShortCastlingMove3)
                    .setFlag(F8),
                _other => self
                    .applyOrdinary(blackLongCastlingMove1)
                    .applyOrdinary(blackLongCastlingMove2)
                    .applyOrdinary(blackLongCastlingMove3)
                    .setFlag(D8),
            },
        };
        castled.correctPlyCounterForCastling()
    }

    /// Apply an ordinary move. Used only by `apply`
    fn applyOrdinary(&self, mv: Move) -> Position {
        // construct the part of the flag that tells who's move it is
        let tomove = BitSet {
            bits: (self.flags * whiteToMove).bits ^ whiteToMove.bits,
        };
        // construct the counter part of the flags
        let plies = match mv.piece() {
            PAWN if self.isEmpty(mv.to()) => BitSet {
                bits: (self.flags * rootCounterBits).bits + onePlyRootOnly,
            },
            _otherwise => BitSet {
                bits: (self.flags * rootCounterBits).bits + onePly,
            },
        };
        // LCR means "lost castling rights"
        // the castling rights that are lost if this was a KING move
        let kingMoveLCR = match mv.piece() {
            KING => match mv.player() {
                WHITE => whiteCastlingRights,
                BLACK => blackCastlingRights,
            },
            _otherwise => BitSet::empty(),
        };
        // the castling rights that are lost if this was a ROOK move
        // it suffices to check for the from positions, since the same rights
        // are also long lost if there is something else on the ROOK fields
        let rookMoveLCR = match mv.from() {
            A8 => bit(C8),
            H8 => bit(G8),
            A1 => bit(C1),
            H1 => bit(G1),
            _other => BitSet::empty(),
        };
        // the castling rights lost if the target of the move if an original
        // ROOK field Again, it matters not if the original ROOK is
        // standing there or not, the rights are lost anyway.
        let rookCaptureLCR = match mv.to() {
            A8 => bit(C8),
            H8 => bit(G8),
            A1 => bit(C1),
            H1 => bit(G1),
            _other => BitSet::empty(),
        };
        // All lost castling rights in this move.
        // For example, moving the rook from a8 to a1 would loose C8 and C1
        let lostCastlingRights = kingMoveLCR + rookMoveLCR + rookCaptureLCR;
        // The new castling rights are the difference between the maybe still
        // existing castling rights and the lost castling rights
        let castlingRights = (self.flags * castlingBits) - lostCastlingRights;
        // en passant bit to set, if any
        let enPassantBit = match mv.piece() {
            PAWN if mv.to() as u8 == mv.from() as u8 + 16 => bit(Field::from(mv.from() as u8 + 8)),
            PAWN if mv.to() as u8 == mv.from() as u8 - 16 => bit(Field::from(mv.from() as u8 - 8)),
            _otherwise => BitSet::empty(),
        };
        // the new castled flag to set, if any
        // not needed as we do castling in applyCastling
        // let gainedCastledBit = match mv.piece() {
        //     KING => match mv.from() {
        //         E1 => match mv.to() {
        //             G1 => bit(F1),
        //             C1 => bit(D1),
        //             _otherwise => BitSet::empty(),
        //         },
        //         E8 => match mv.to() {
        //             G8 => bit(F8),
        //             C8 => bit(D8),
        //             _otherwise => BitSet::empty(),
        //         },
        //         _otherwise => BitSet::empty(),
        //     },
        //     _otherwise => BitSet::empty(),
        // };

        // The new part of flags that tells who has castled already.
        let hasCastledFlags = self.flags * castlingDoneBits; // + gainedCastledBit;
                                                             // Fields that will be empty after this move.
        let fromMask = bit(mv.from())
            + match mv.promote() {
                // compute field of piece captured by en passant
                PAWN if mv.player() == WHITE => bit(Field::from(mv.to() as u8 - 8)),
                PAWN if mv.player() == BLACK => bit(Field::from(mv.to() as u8 + 8)),
                _otherwise => BitSet::empty(),
            };
        // The following must be xor'ed out from the hash
        let fromHash = zobrist::ppfZobrist(mv.player() as u32, mv.piece() as u32, mv.from() as u32)
            ^ match mv.promote() {
                // compute zobrist of captured piece
                PAWN if mv.player() == WHITE => {
                    zobrist::ppfZobrist(mv.player().opponent() as u32, PAWN as u32, mv.to() as u32 - 8)
                }
                PAWN if mv.player() == BLACK => {
                    zobrist::ppfZobrist(mv.player().opponent() as u32, PAWN as u32, mv.to() as u32 + 8)
                }
                _otherwise => 0,
            };
        // field that will be set after the move
        let toMask = bit(mv.to());
        // the value that must be xor'ed out of the hash
        let toHash = match self.pieceOn(mv.to()) {
            EMPTY => 0,
            p => zobrist::ppfZobrist(mv.player().opponent() as u32, p as u32, mv.to() as u32),
        };
        // the piece to place on to
        let piece = match mv.piece() {
            PAWN if mv.promote() >= KNIGHT => mv.promote(),
            _otherwise => mv.piece(),
        };
        // the fields occupied by WHITE after the move
        let whites = match mv.player() {
            WHITE => (self.whites - fromMask) + toMask,
            BLACK => self.whites - (fromMask + toMask),
        };
        // updated pawn set
        let pawnSet = match piece {
            PAWN | KNIGHT | KING => self.pawnSet - fromMask + toMask,
            _otherwise => self.pawnSet - fromMask - toMask,
        };
        // updated bishop set
        let bishopSet = match piece {
            BISHOP | KNIGHT | QUEEN => self.bishopSet - fromMask + toMask,
            _otherwise => self.bishopSet - fromMask - toMask,
        };
        // updated bishop set
        let rookSet = match piece {
            ROOK | KING | QUEEN => self.rookSet - fromMask + toMask,
            _otherwise => self.rookSet - fromMask - toMask,
        };
        // current en passant hash value, if any, needs to get xor'ed out
        let currentEPHash = match self.flags * enPassantBits {
            currentEP if currentEP.some() => zobrist::flagZobrist(fld(currentEP) as u32),
            _otherwise => 0,
        };
        // hash for new en passant bit, if any, needs to get xor'ed in
        let enPassantHash = match enPassantBit {
            bits if bits.some() => zobrist::flagZobrist(fld(enPassantBit) as u32),
            _otherwise => 0,
        };
        // new hash
        let hash = self.hash
            ^ flagZobristA1   // flip the A1 hash
            ^ (boolMask((self.flags * lostCastlingRights).member(C1)) & flagZobristC1)
            ^ (boolMask((self.flags * lostCastlingRights).member(G1)) & flagZobristG1)
            ^ (boolMask((self.flags * lostCastlingRights).member(C8)) & flagZobristC8)
            ^ (boolMask((self.flags * lostCastlingRights).member(G8)) & flagZobristG8)
            ^ currentEPHash
            ^ enPassantHash
            ^ fromHash
            ^ zobrist::ppfZobrist(mv.player() as u32, piece as u32, mv.to() as u32)
            ^ toHash;
        Position {
            flags: tomove + castlingRights + hasCastledFlags + enPassantBit + plies,
            whites,
            pawnSet,
            bishopSet,
            rookSet,
            hash,
        }
    }
}

impl PartialEq for Position {
    fn eq(&self, other: &Position) -> bool {
        self.flags - counterBits == other.flags - counterBits
            && self.whites == other.whites
            && self.pawnSet == other.pawnSet
            && self.bishopSet == other.bishopSet
            && self.rookSet == other.rookSet
    }
}

impl Eq for Position {}

impl Hash for Position {
    fn hash<H: Hasher>(&self, state: &mut H) { self.hash.hash(state); }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "P(hash={:x}, flags={}, whites={}, pawnSet={}, bishopSet={}, rookSet={})",
            self.hash, self.flags, self.whites, self.pawnSet, self.bishopSet, self.rookSet
        )
    }
}

/// Representation of a move
///    
/// Normally, the promote bits are EMPTY, e.g. 0b000.
/// If moving piece is KING, promote may be QUEEN or KING to indicate
/// queenside or kingside castling. If it's a promoting PAWN move,
/// promote may be KNIGHT, BISHOP, ROOK or QUEEN. If it's an en passant
/// capturing, promote will be PAWN.
///
/// We need 20 bits:
///
/// ```text
/// KPfffpppttttttssssss
/// ```
///
/// - [K] This bit indicates whether this move caused a cut and is thus
///   a so called killer move.
/// - [P] 1 bit for player where 0 means BLACK, 1 means WHITE
/// - [fff] 3 bit encoding for moving Piece
/// - [ppp] 3 bit for promotion piece
/// - [tttttt] 6 bit target index
/// - [ssssss] 6 bit source index
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
#[repr(transparent)]
pub struct Move {
    mv: u32,
}

#[allow(clippy::inconsistent_digit_grouping)]
// we use the unconventional digit grouping to demonstrate the structure
// of the bitfield
impl Move {
    /// Tell if this is a killer move
    #[inline]
    pub fn killer(self) -> bool { self.mv & 0b10_000_000_000000_000000u32 != 0 }

    /// Which player is moving?
    #[inline]
    pub fn player(self) -> Player { Player::from(self.mv & 0b01_000_000_000000_000000u32 != 0) }

    /// What piece is moving?
    #[inline]
    pub fn piece(self) -> Piece { Piece::from((self.mv & 0b00_111_000_000000_000000u32) >> 15) }

    /// What is it promoting into, if any?
    #[inline]
    pub fn promote(self) -> Piece { Piece::from((self.mv & 0b00_000_111_000000_000000u32) >> 12) }

    /// Whereto are we moving?
    #[inline]
    pub fn to(self) -> Field { Field::from(((self.mv & 0b00_000_000_111111_000000u32) >> 6) as u8) }

    /// From whence are we moving?
    #[inline]
    pub fn from(self) -> Field { Field::from((self.mv & 0b111111u32) as u8) }

    /// construct a move from player, piece to move, piece to promote,
    /// from field and to field
    pub const fn new(pl: Player, pc: Piece, pr: Piece, from: Field, to: Field) -> Move {
        Move {
            mv: ((pl as u32) << 18) | ((pc as u32) << 15) | ((pr as u32) << 12) | ((to as u32) << 6) | (from as u32),
        }
    }

    /// Show this Move in algebraic notation
    ///
    /// ```
    /// use rasch::position::Move;
    /// use rasch::position::Piece::*;
    /// use rasch::position::Player::*;
    /// use rasch::fieldset::Field::*;
    ///
    /// assert_eq!(Move::new(WHITE, PAWN, QUEEN, B7, C8).algebraic(), "b7c8q");
    /// ```
    pub fn algebraic(self) -> String {
        let p = if self.promote() >= KNIGHT && self.piece() == PAWN {
            self.promote().show().to_lowercase()
        } else {
            String::from("")
        };
        self.from().show() + &self.to().show() + &p
    }

    /// Find a move in a list that corresponds to a given string or post
    /// an Err. This will be used when accepting a move to make from
    /// the user.
    ///
    /// This is so that we'll use the moves generated by our move
    /// generator only. Assuming the move generator generates all
    /// possible moves for any position, the move entered **must
    /// be** in the list, or else it is illegal.
    pub fn unAlgebraic(list: &[Move], src: &str) -> Result<Move, String> {
        for mv in list.iter() {
            if mv.algebraic() == src {
                return Ok(*mv);
            }
        }
        let vs: Vec<_> = list.iter().map(|x| x.algebraic()).collect();
        Err(format!("Move {} does not appear in [{}]", src, &vs[..].join(", ")))
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result { write!(f, "{}", self.algebraic()) }
}

pub fn initialBoard() -> Position {
    let empty = Position {
        hash:      0,
        flags:     bit(A1) + castlingBits,
        whites:    BitSet::empty(),
        pawnSet:   BitSet::empty(),
        bishopSet: BitSet::empty(),
        rookSet:   BitSet::empty(),
    };
    empty
        .place(BLACK, PAWN, [A7, B7, C7, D7, E7, F7, G7, H7].iter().collect())
        .place(WHITE, PAWN, [A2, B2, C2, D2, E2, F2, G2, H2].iter().collect())
        .place(BLACK, BISHOP, [C8, F8].iter().collect())
        .place(WHITE, BISHOP, [C1, F1].iter().collect())
        .place(BLACK, KNIGHT, [B8, G8].iter().collect())
        .place(WHITE, KNIGHT, [B1, G1].iter().collect())
        .place(BLACK, ROOK, [A8, H8].iter().collect())
        .place(WHITE, ROOK, [A1, H1].iter().collect())
        .place(BLACK, QUEEN, bit(D8))
        .place(WHITE, QUEEN, bit(D1))
        .place(BLACK, KING, bit(E8))
        .place(WHITE, KING, bit(E1))
        .rehash()
}
