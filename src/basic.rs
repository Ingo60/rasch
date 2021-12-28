//! Basic data types `Player`, `Piece` and `Move`

#![allow(non_snake_case)]

use super::fieldset::Field;
use super::position::Position;
use std::fmt::{Display, Formatter};
pub use Piece::*;
pub use Player::*;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum Player {
    BLACK,
    WHITE,
}

/*
We can't to this because unstable library feature 'step_trait'
impl Step for Player {
    fn forward(start: Self, count: usize) -> Self {
        Step::forward_checked(start, count).expect("overflow in `Step::forward`")
    }

    unsafe fn forward_unchecked(start: Self, count: usize) -> Self {
        Step::forward(start, count)
    }

    fn backward(start: Self, count: usize) -> Self {
        Step::backward_checked(start, count).expect("overflow in `Step::backward`")
    }

    unsafe fn backward_unchecked(start: Self, count: usize) -> Self {
        Step::backward(start, count)
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        if start == BLACK { Some(WHITE) } else { None }
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        if start == WHITE { Some(BLACK) } else { None }
    }
}

*/

impl From<bool> for Player {
    /// if true then it's WHITE, otherwise BLACK
    fn from(b: bool) -> Player {
        if b {
            WHITE
        } else {
            BLACK
        }
    }
}

impl Display for Player {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", if *self == BLACK { "b" } else { "w" })
    }
}

impl Player {
    /// the color of the opponent
    #[inline]
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
    #[inline]
    pub fn factor(self) -> i32 {
        2 * (self as i32) - 1
    }

    /// a value in favor of this player
    ///
    /// ```
    /// use rasch::position as P;
    /// assert_eq!(P::WHITE.forP(42), 42);
    /// assert_eq!(P::BLACK.forP(42), -42);
    /// ```
    #[inline]
    pub fn forP(self, v: i32) -> i32 {
        self.factor() * v
    }

    /// penalize player on condition by n
    /// If the condition holds, count n in favor of players opponent, else 0
    pub fn penalize(self, cond: bool, n: i32) -> i32 {
        match cond {
            true => self.opponent().forP(n),
            false => 0,
        }
    }
}

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
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.show())
    }
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
            _ => panic!("can't cast {} to Piece", u),
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

    /// Base score for a piece in centipawns, used in evaluation
    pub fn score(self) -> i32 {
        match self {
            EMPTY => 0,
            PAWN => 100,
            KNIGHT => 300,
            BISHOP => 305,
            ROOK => 550,
            QUEEN => 875,
            KING => 1000,
        }
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
/// We need 19 bits:
///
/// ```text
/// Pfffpppttttttssssss
/// ```
///
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

pub const NO_MOVE: Move = Move { mv: 0xffff_ffff };

#[allow(clippy::inconsistent_digit_grouping)]
// we use the unconventional digit grouping to demonstrate the structure
// of the bitfield
impl Move {
    /// `true` if and only if this is not `noMove`
    #[inline]
    pub fn isMove(self) -> bool {
        self != NO_MOVE
    }

    /// Which player is moving?
    #[inline]
    pub fn player(self) -> Player {
        Player::from(self.mv & 0b01_000_000_000000_000000u32 != 0)
    }

    /// What piece is moving?
    #[inline]
    pub fn piece(self) -> Piece {
        Piece::from((self.mv & 0b00_111_000_000000_000000u32) >> 15)
    }

    /// What is it promoting into, if any?
    #[inline]
    pub fn promote(self) -> Piece {
        Piece::from((self.mv & 0b00_000_111_000000_000000u32) >> 12)
    }

    /// Whereto are we moving?
    #[inline]
    pub fn to(self) -> Field {
        Field::from(((self.mv & 0b00_000_000_111111_000000u32) >> 6) as u8)
    }

    /// From whence are we moving?
    #[inline]
    pub fn from(self) -> Field {
        Field::from((self.mv & 0b111111u32) as u8)
    }

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
        if self == NO_MOVE {
            return "????".to_string();
        }
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

    /// Show a move in standard algebraic notation (SAN)
    pub fn showSAN(self, pos: Position) -> String {
        // handle castling first
        if self.piece() == KING && self.promote() != EMPTY {
            if self.promote() == KING {
                return "O-O".into();
            } else {
                return "O-O-O".into();
            }
        }
        // no castling
        let moves = pos.moves();
        let mut san = if self.piece() != PAWN { self.piece().show() } else { "".into() };
        let capture = !pos.isEmpty(self.to()) || self.promote() == PAWN;
        let file = moves.iter().any(|m| {
            m.from() != self.from()
                && m.to() == self.to()
                && m.player() == self.player()
                && m.piece() == self.piece()
                && m.from().rank() == self.from().rank()
        }) || (self.piece() == PAWN && capture);
        let rank = moves.iter().any(|m| {
            m.from() != self.from()
                && m.to() == self.to()
                && m.player() == self.player()
                && m.piece() == self.piece()
                && m.from().file() == self.from().file()
        });
        let prom = if self.promote() > PAWN {
            "=".to_string() + &self.promote().show()
        } else {
            "".to_string()
        };
        if file {
            san.push(self.from().file());
        }
        if rank {
            san += &format!("{}", self.from().rank());
        }
        if capture {
            san.push('x');
        }
        san += &self.to().show();
        san += &prom;
        san
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.algebraic())
    }
}

// show a bunch of moves in algebraic form
pub fn showMoves(moves: &[Move]) -> String {
    moves.iter().map(|x| x.algebraic()).collect::<Vec<_>>().join(" ")
}

// show a bunch of moves in standard algebraic notation, where `start` is the position the moves are valid for.
pub fn showMovesSAN(moves: &[Move], start: Position) -> String {
    let mut sans = vec![];
    let mut pos = start;
    for m in moves {
        sans.push(m.showSAN(pos));
        pos = pos.apply(*m);
    }
    sans.join(" ")
}
