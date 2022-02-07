#![allow(non_snake_case)]
//! A `CPos` is a compressed representation of a `Position`.
//! A `Signature` is the kind of an end game, for example "KQB-KNB".
//! A canonic end game is one where `WHITE` has the better pieces.
//! The non-canonic ones are not worth extra storage space,
//! as they can be reduced to canonic ones by simply changing the colour of the pieces
//! and mirroring the board horizontally if pawns are in the game.

use memmap2::Mmap;
use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt,
    fmt::{Display, Formatter},
    fs::{File, OpenOptions},
    hash::{Hash, Hasher},
    io::{BufReader, ErrorKind::UnexpectedEof, Read, Seek, SeekFrom, Write},
    // iter::{Filter, Map},
    slice,
};

use super::{
    basic::{decode_str_sig, CPosState, Move, Piece, Player, PlayerPiece},
    cposio::mk_egtb_path,
    cposmove::{CPosMoveIterator, CPosReverseMoveIterator},
    fen::encodeFEN,
    fieldset::{BitSet, Field},
    mdb,
    position::{
        bit, Position, EN_PASSANT_BITS, LEFT_HALF, LOWER_HALF, LOWER_LEFT_QUARTER, PAWN_FIELDS, WHITE_TO_MOVE,
    },
};
use CPosState::*;
use Field::*;
use Piece::*;
use Player::*;

pub type MovesMap<'a> = HashMap<Signature, (Mmap, &'a [CPos])>;
pub type EgtbMap<'a> = HashMap<Signature, (Mmap, &'a [u8])>;

/// Signature of a CPos
///
/// This is needed to employ the symmetries on the chess board in order to save lots of disk space.
/// We define a "canonical" position as follows:
/// - if there are no PAWNs in the game, the white KING is in the lower left corner (rank 1..4, file A..D)
/// - if there are PAWNs in the game, the white KING is in the left half (rank 1..8, files A..D)
///
/// Only canonical positions will be in the database.
/// All other positions with the same sets of pieces can be reduced to a canonical one by
/// - mirroring the board on the vertical axis if the king is in the right half
/// - and then, if there are no PAWNs in the game, mirroring the board on the horizontal axis
///
/// But this is not all. For example, it turns out that it doesn't matter if we have a WHITE KING on A1,
/// a BLACK KING on C2 and a BLACK ROOK on C8 or if we change the BLACK pieces to WHITE ones and the WHITE ones to BLACK
/// ones: Whatever the player with ROOK and KING is, will be able to mate if it is his turn while the other player
/// is doomed to move his KING to A2 where it is mated in the next move.
/// To get "the same" position with different colors, it is necessary to mirror the board on the horizontal axis
/// whenever pawns are present. In addition, when querying the database, care must be taken to
/// exchange the states for BLACK and WHITE whenever it was necessary to exchange colors.
///
/// Every canonical position has thus up to 3 equivalent positions where the pieces have the same colors and up to
/// 4 equivalent positions where the colours are chhanged. Taking into account that every position in the EGTB has
/// two states for the different players to move, we map up to 16 positions to one and the same 64 bit word.
/// (If there are pawns, it is only 4, though).
///
/// The question is only: which of the two positions described earlier will be in the database? We will extend our
/// definition by these rules:
/// - WHITE has the more valuable pieces, where for the sake of simplicity,
/// a queen beats any number of rooks, bishops, knights and pawns, a rook beats any number of bishops, knights and pawns,
/// and so forth. But, of course, 2 rooks are better than 1, as long as no queens are present.
/// Thus, the following are canonic signatures: `KQP-KRR` `KQ-KRRR`, whereas `KBB-KQ` is not.
/// - If both BLACK and WHITE have the same number and kinds of pieces there will be one table where both colour symmetric
/// positions are contained, so no savings here (for now).
/// To decide the question, whether a position must get colour changed on searching, we have this `Signature` type,
/// that can be created fast from a `CPos`, can be compared fast and can be used to derive a unique file name for the
/// EGTB file.
///

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Signature {
    /// encoding is 6 bits for each piece type, where the most significant bits hold the number of queens and the least
    /// significant ones the number of pawns.
    white: u32,
    /// encoding is 6 bits for each piece type, where the most significant bits hold the number of queens and the least
    /// significant ones the number of pawns.
    black: u32,
}

impl Signature {
    /// the signature for K-K
    pub const SIGNATURE_KK: Signature = Signature { white: 0, black: 0 };
    /// 1 nibble per piece
    pub const BITS_PER_PIECE: u32 = 4;
    pub const PIECE_BITS: u32 = (1 << Signature::BITS_PER_PIECE) - 1;

    pub const PAWN_SHIFT: u32 = 0;
    pub const KNIGHT_SHIFT: u32 = Signature::PAWN_SHIFT + Signature::BITS_PER_PIECE;
    pub const BISHOP_SHIFT: u32 = Signature::KNIGHT_SHIFT + Signature::BITS_PER_PIECE;
    pub const ROOK_SHIFT: u32 = Signature::BISHOP_SHIFT + Signature::BITS_PER_PIECE;
    pub const QUEEN_SHIFT: u32 = Signature::ROOK_SHIFT + Signature::BITS_PER_PIECE;

    pub const PAWN_BITS: u32 = Signature::PIECE_BITS << Signature::PAWN_SHIFT;
    pub const KNIGHT_BITS: u32 = Signature::PIECE_BITS << Signature::KNIGHT_SHIFT;
    pub const BISHOP_BITS: u32 = Signature::PIECE_BITS << Signature::BISHOP_SHIFT;
    pub const ROOK_BITS: u32 = Signature::PIECE_BITS << Signature::ROOK_SHIFT;
    pub const QUEEN_BITS: u32 = Signature::PIECE_BITS << Signature::QUEEN_SHIFT;

    const fn one(p: Piece) -> u32 {
        match p {
            PAWN => 1 << Signature::PAWN_SHIFT,
            KNIGHT => 1 << Signature::KNIGHT_SHIFT,
            BISHOP => 1 << Signature::BISHOP_SHIFT,
            ROOK => 1 << Signature::ROOK_SHIFT,
            QUEEN => 1 << Signature::QUEEN_SHIFT,
            _ => 0,
        }
    }

    /// A canonic [CPos] will result in a canonic [Signature], but not every [CPos] that has a canonic signature
    /// is itself canonic (the white [KING] could not be in the left half, for instance) or the pieces
    /// could be in a wrong order.
    pub fn new(cpos: CPos) -> Signature {
        let mut white = 0u32;
        let mut black = 0u32;

        for u in 0..4 {
            match cpos.www_at(u) {
                WWW(_, KING, _) | WWW(_, EMPTY, _) => continue,
                WWW(WHITE, piece, _) => {
                    white += Signature::one(piece);
                }
                WWW(BLACK, piece, _) => {
                    black += Signature::one(piece);
                }
            }
        }
        Signature { white, black }
    }
    pub fn from_vec(vec: &Vec<PlayerPiece>) -> Signature {
        let mut white = 0u32;
        let mut black = 0u32;
        for pp in vec {
            match pp {
                (WHITE, p) => {
                    white += Signature::one(*p);
                }
                (BLACK, p) => {
                    black += Signature::one(*p);
                }
            }
        }
        Signature { white, black }
    }
    /// Make a vector of `PlayerPosition`s (`KING`s not included)
    pub fn to_vec(&self) -> Vec<PlayerPiece> {
        let mut result: Vec<PlayerPiece> = Vec::new();
        //result.push('K');
        for _ in 0..self.white_queens() {
            result.push((WHITE, QUEEN));
        }
        for _ in 0..self.white_rooks() {
            result.push((WHITE, ROOK));
        }
        for _ in 0..self.white_bishops() {
            result.push((WHITE, BISHOP));
        }
        for _ in 0..self.white_knights() {
            result.push((WHITE, KNIGHT));
        }
        for _ in 0..self.white_pawns() {
            result.push((WHITE, PAWN));
        }
        for _ in 0..self.black_queens() {
            result.push((BLACK, QUEEN));
        }
        for _ in 0..self.black_rooks() {
            result.push((BLACK, ROOK));
        }
        for _ in 0..self.black_bishops() {
            result.push((BLACK, BISHOP));
        }
        for _ in 0..self.black_knights() {
            result.push((BLACK, KNIGHT));
        }
        for _ in 0..self.black_pawns() {
            result.push((BLACK, PAWN));
        }
        result
    }

    /// decode a signature from a string, return the canonic one, if any
    pub fn new_from_str_canonic(desc: &str) -> Result<Signature, String> {
        decode_str_sig(desc)
            .and_then(|v| Ok(Signature::from_vec(&v)))
            .map(|s| s.mk_canonic())
    }
    /// If the `Signature` of a `CPos` is **not** canonic, then the corresponding CPos will have the
    /// colours of the pieces changed and a possible search result needs the flags switched.
    pub const fn is_canonic(&self) -> bool {
        self.white >= self.black
    }
    /// A `Signature` is **symmetric** if both players have the same set of pieces.
    /// A symmetric `Signature` is also canonic.
    pub const fn is_symmetric(&self) -> bool {
        self.white == self.black
    }

    /// The canonic form of this `Signature`
    pub const fn mk_canonic(&self) -> Signature {
        if self.is_canonic() {
            *self
        } else {
            Signature { white: self.black, black: self.white }
        }
    }

    /// A Signature with the values for black/white flipped
    pub const fn opposite(&self) -> Signature {
        Signature { white: self.black, black: self.white }
    }

    pub const fn white_queens(&self) -> u32 {
        (self.white & Signature::QUEEN_BITS) >> Signature::QUEEN_SHIFT
    }
    pub const fn white_rooks(&self) -> u32 {
        (self.white & Signature::ROOK_BITS) >> Signature::ROOK_SHIFT
    }
    pub const fn white_bishops(&self) -> u32 {
        (self.white & Signature::BISHOP_BITS) >> Signature::BISHOP_SHIFT
    }
    pub const fn white_knights(&self) -> u32 {
        (self.white & Signature::KNIGHT_BITS) >> Signature::KNIGHT_SHIFT
    }
    pub const fn white_pawns(&self) -> u32 {
        self.white & Signature::PAWN_BITS
    }
    pub const fn black_queens(&self) -> u32 {
        (self.black & Signature::QUEEN_BITS) >> Signature::QUEEN_SHIFT
    }
    pub const fn black_rooks(&self) -> u32 {
        (self.black & Signature::ROOK_BITS) >> Signature::ROOK_SHIFT
    }
    pub const fn black_bishops(&self) -> u32 {
        (self.black & Signature::BISHOP_BITS) >> Signature::BISHOP_SHIFT
    }
    pub const fn black_knights(&self) -> u32 {
        (self.black & Signature::KNIGHT_BITS) >> Signature::KNIGHT_SHIFT
    }
    pub const fn black_pawns(&self) -> u32 {
        self.black & Signature::PAWN_BITS
    }
    /// A `Signature` has pawns if it has white pawns or black pawns.
    pub const fn has_pawns(&self) -> bool {
        self.white_pawns() > 0 || self.black_pawns() > 0
    }

    /// make a string like "KQ-KR" from the signature
    pub fn display(&self) -> String {
        let mut result = String::with_capacity(10);
        result.push('K');
        for _ in 0..self.white_queens() {
            result.push('Q');
        }
        for _ in 0..self.white_rooks() {
            result.push('R');
        }
        for _ in 0..self.white_bishops() {
            result.push('B');
        }
        for _ in 0..self.white_knights() {
            result.push('N');
        }
        for _ in 0..self.white_pawns() {
            result.push('P');
        }
        result.push('-');
        result.push('K');
        for _ in 0..self.black_queens() {
            result.push('Q');
        }
        for _ in 0..self.black_rooks() {
            result.push('R');
        }
        for _ in 0..self.black_bishops() {
            result.push('B');
        }
        for _ in 0..self.black_knights() {
            result.push('N');
        }
        for _ in 0..self.black_pawns() {
            result.push('P');
        }
        result
    }

    /// helper function for predecessor, computes the predecessors obtained by capturing one of `player`s officers.
    fn capture(
        &self,
        player: Player,
        map: &mut HashMap<Signature, Alienation>,
        cap: impl Fn(Piece) -> Alienation,
    ) {
        for (p, u) in [
            (QUEEN, Signature::QUEEN_SHIFT),
            (ROOK, Signature::ROOK_SHIFT),
            (BISHOP, Signature::BISHOP_SHIFT),
            (KNIGHT, Signature::KNIGHT_SHIFT),
        ] {
            if player == WHITE {
                if self.white & (Signature::PIECE_BITS << u) != 0 {
                    map.insert(
                        Signature { black: self.black, white: self.white - (1 << u) },
                        cap(p),
                    );
                }
            } else {
                if self.black & (Signature::PIECE_BITS << u) != 0 {
                    map.insert(
                        Signature { white: self.white, black: self.black - (1 << u) },
                        cap(p),
                    );
                }
            }
        }
    }

    /// A signature defines a set of chess positions.
    /// However, by capturing pieces and promoting pawns
    /// one obtains positions that are members of a different set.
    /// This function computes the sets that are reachable with a
    /// single move from a given one.
    ///
    /// We assume that for any two Signatures A and B the following holds:
    /// 1. if B is a predecessor of A, A is not a predecessor of B
    /// 2. from 1) it follows immediatly that A is not its own predecessor.
    /// 3. if B is a predecessor of A, then there is one and only one alienation transformation that turns A into B.
    ///
    /// To prove 1.
    /// - If B is a predecessor of A, there is a transformation X that turns A into B.
    /// - If X is Capture(p), then B has one piece less than A. But there is no transformation that increases
    /// the number of pieces, hence there can be no transformation that turns B into A.
    /// - If X is Promotion(p), then B has one PAWN less than A. But there is no transformation that increases
    /// then number of pawns, hence there can be no transformation that turns B into A.
    /// - If X is PromoteAndCapture(p1, p2), then again B has one PAWN less than A. As before,
    /// there is no transformation that turns B into A.
    /// - If A were a predecessor of B, there would have to be a transformation Y that turns B into A.
    /// But we have shown that there can be no such transformation if B is a predecessor of A.
    /// Therefore, A cannot be a predecessor of B if B is a predecessor of A. q.e.d
    pub fn predecessors(&self) -> HashMap<Signature, Alienation> {
        let mut map = HashMap::new();

        let capw = |p| Capture(WHITE, p);
        let capb = |p| Capture(BLACK, p);

        // for each kind, assume one of the pieces got captured
        self.capture(WHITE, &mut map, &capw);
        self.capture(BLACK, &mut map, &capb);

        // PAWNs are a bit more difficult
        if self.white_pawns() > 0 {
            let one_pawn_less = Signature { black: self.black, white: self.white - Signature::one(PAWN) };
            // the case where one of the pawns was simply captured
            map.insert(one_pawn_less, Capture(WHITE, PAWN));
            // now the cases where a pawn promotes to some other piece
            for piece in [QUEEN, ROOK, BISHOP, KNIGHT] {
                let promoted = Signature {
                    black: one_pawn_less.black,
                    white: one_pawn_less.white + Signature::one(piece),
                };
                map.insert(promoted, Promote(WHITE, piece));
                // but with the promotion, the pawn could have captured an opponents' piece
                // e.g. KP-KQ could become KN-KQ but also KN-K
                // (by capturing the black queen on the 8th rank and promoting to a knight)
                // Note that we could never capture a PAWN this way, though.
                let pcap = |p| PromoteAndCapture(WHITE, piece, p);
                promoted.capture(BLACK, &mut map, &pcap);
            }
        };

        if self.black_pawns() > 0 {
            let one_pawn_less = Signature { white: self.white, black: self.black - Signature::one(PAWN) };
            // the case where one of the pawns was simply captured
            map.insert(one_pawn_less, Capture(BLACK, PAWN));
            // now the cases where a pawn promotes to some other piece
            for piece in [QUEEN, ROOK, BISHOP, KNIGHT] {
                let promoted = Signature {
                    white: one_pawn_less.white,
                    black: one_pawn_less.black + Signature::one(piece),
                };
                map.insert(promoted, Promote(BLACK, piece));
                // but with the promotion, the pawn could have captured an opponents' piece
                // e.g. KP-KQ could become KN-KQ but also KN-K
                // (by capturing the black queen on the 8th rank and promoting to a knight)
                // Note that we could never capture a PAWN this way, though.
                let pcap = |p| PromoteAndCapture(BLACK, piece, p);
                promoted.capture(WHITE, &mut map, &pcap);
            }
        };
        map
    }

    /// Get all the predecessor `Signatures` with canonic/non-canonic ones grouped properly
    pub fn get_relatives(&self) -> Vec<RelationMode> {
        let mut result = Vec::with_capacity(16);
        let map = self.predecessors();
        for (&s1, &a1) in map.iter() {
            let s2 = s1.opposite();

            match map.get(&s2) {
                Some(&a2) if s1 != s2 => {
                    if s1.is_canonic() {
                        result.push(Both(s1, a1, s2, a2));
                    };
                    // we either had (s2,s1) before, or it will come later
                }
                _none => {
                    if s1.is_canonic() {
                        result.push(Canonic(s1, a1))
                    } else {
                        result.push(NonCanonic(s1, a1))
                    }
                }
            }
        }
        result
    }

    /// Construct first [CPos] of this signature
    /// ```
    /// use rasch::cpos::Signature;
    /// use rasch::cpos::NibbleAddr;
    /// use rasch::mdb::initStatic;
    /// initStatic();
    /// let sig = Signature::new_from_str_canonic("KQRPP-K").unwrap();
    /// assert_eq!(sig.first().canonic_addr(), NibbleAddr(0));
    /// ```
    pub fn first(&self) -> CPos {
        let v = self.to_vec();
        let mut cpos = CPos { bits: 0 }.move_king(WHITE, A1).move_king(BLACK, C1);
        let mut occupied = bit(A1) + bit(C1);
        let mut inx = 0;
        for (player, piece) in v {
            let free = (if piece == PAWN { PAWN_FIELDS - occupied } else { !occupied }).bitIndex();
            occupied = occupied + bit(free);
            cpos = cpos
                .change_player(inx, player)
                .change_piece(inx, piece)
                .move_piece(inx, free);
            inx += 1;
        }
        cpos = cpos.ordered();

        if cpos.canonic_addr() != NibbleAddr(0) {
            panic!(
                "{}    {:?}  expected NibbleAddr(0) found {:?}",
                self,
                cpos,
                cpos.canonic_addr()
            );
        }

        cpos
    }

    /// Construct last [CPos] of this signature
    pub fn last(&self) -> CPos {
        let v = self.to_vec();
        let last_wk = if self.has_pawns() { D8 } else { D4 };
        let mut cpos = CPos { bits: 0 }.move_king(WHITE, last_wk).move_king(BLACK, H8);
        let mut occupied = bit(last_wk) + bit(H8);
        let mut inx = 0;
        let mut f = G8;
        let mut pf = H7;
        for (player, piece) in v {
            if piece != PAWN {
                while occupied.member(f) {
                    f = Field::from(f as u8 - 1);
                }
                occupied = occupied + bit(f);
                cpos = cpos
                    .change_player(inx, player)
                    .change_piece(inx, piece)
                    .move_piece(inx, f);
            } else {
                while occupied.member(pf) {
                    pf = Field::from(pf as u8 - 1);
                }
                occupied = occupied + bit(pf);
                cpos = cpos
                    .change_player(inx, player)
                    .change_piece(inx, piece)
                    .move_piece(inx, pf);
            }
            inx += 1;
        }
        let stw = if cpos.valid(WHITE) { DRAW } else { INVP };
        let stb = if cpos.valid(BLACK) { DRAW } else { INVP };
        cpos.ordered().with_state(stw, stb)
    }
}

impl Display for Signature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(&self.display())
    }
}

impl fmt::Debug for Signature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(&self.display())
    }
}

/// `Alienation` is a transformation that turns a `Signature` into one of their predecessors.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Alienation {
    /// - by capturing a piece of a certain kind, e.g. KQ-KN → KN-K with Capture(WHITE, QUEEN)
    /// One has to look for opponent's moves here.
    Capture(Player, Piece),
    /// - by promoting a PAWN, e.g. KR-KP → KQ-KR wih Promote(BLACK, QUEEN)
    /// Look for `Player`s `PAWN` moves.
    Promote(Player, Piece),
    /// - by promoting a PAWN with a capturing move, e.g. KR-KP → KB-K with PromoteAndCapture(BLACK, BISHOP, ROOK)
    /// (It goes without saying that the captured piece must be one of the opponent's pieces.)
    /// Look for `Player`s `PAWN` moves.
    PromoteAndCapture(Player, Piece, Piece),
}
pub use Alienation::*;

/// `RelationMode` describes how a `Signature` is related to one of its predecessors.
/// It is possible that a non-canonic `Signature` is a predecessor of some `Signature` and the canonic
/// counterpart is not. It is also possible that both are predecessors.
///
/// We need this to avoid scanning predecessor EGTBs multiple times.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RelationMode {
    /// only the canonic one is predecessor
    Canonic(Signature, Alienation),
    /// only the non-canonic one is predecessor
    NonCanonic(Signature, Alienation),
    /// both canonic and non-canonic ones are predecessors
    Both(Signature, Alienation, Signature, Alienation),
}
pub use RelationMode::*;

/// **W**ho, **W**hat, **W**here
///
/// Triple of [Player], [Piece], [Field]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct WWW(Player, Piece, Field);

impl fmt::Debug for WWW {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.0, self.1, self.2)
    }
}

impl fmt::Display for WWW {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.0 != BLACK || self.1 != EMPTY || self.2 != A1 {
            write!(f, "{}{}{}", self.0, self.1, self.2)
        } else {
            f.write_str("----")
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NibbleAddr(pub usize);

impl NibbleAddr {
    /// Get a nibble from a byte array. If the address is even, the left half of the
    /// byte is used, else the right half. The extracted information is returned in the
    /// right 4 bits.
    pub fn get_nibble(&self, array: &[u8]) -> u8 {
        let baddr = self.0 >> 1;
        if self.0 & 1 == 0 {
            // even
            array[baddr] >> 4
        } else {
            array[baddr] & 0x0f
        }
    }

    /// Store a nibble in a byte array.
    /// Even nibbles go in the left 4 bits, odd nibbles in the right 4 bits.
    ///
    /// The [nibble] passed as argument must have the information in its rightmost 4 bits.
    pub fn set_nibble(&self, array: &mut [u8], nibble: u8) -> () {
        let NibbleAddr(naddr) = *self;
        let baddr = naddr >> 1;
        let bits = if naddr & 1 == 0 { (nibble & 0x0f) << 4 } else { nibble & 0x0F };
        let mask = if naddr & 1 == 0 { 0x0f } else { 0xf0 };
        let byte = (array[baddr] & mask) | bits;
        array[baddr] = byte;
    }

    /// tell how many bytes are needed for reading and writing to this address
    pub fn bytes(&self) -> usize {
        (self.0 >> 1) + 1
    }
}

/// # Compressed position for the endgame tablebases.

/// This is good for up to 6 pieces (kings included) and 12 bits worth of flags.
/// The flags indicate the ability for a player to force a win or a draw, or the inability to avoid a loss or a draw.
/// In the version that is stored in the `moves` file, the flags are used to indicate whether this move applies to
/// black or white, the piece to move and the target field.
///
/// Note that the CPos does not indicate which player is to move. Hence the flags accomodate for both cases.
/// It is as if you enter a room and see a chessboard with a few pieces on it.
/// You don't normally know who is to move (except if some player is in check, then it must be that players move),
/// so you can figure out the odds for both sides.
///
/// Since a black and a white `KING` *must* be present, we simply note their fields in the lower 2x6 bits.
///
/// Each of the other 4 pieces is encoded in 10 bits: a code for player and piece type (4 bits) and the field (6 bits).
/// The encodings are:

/// - 0 - EMPTY
/// - 1/9  - BLACK/WHITE PAWN
/// - 2/10 - BLACK/WHITE KNIGHT
/// - 3/11 - BLACK/WHITE BISHOP
/// - 4/12 - BLACK/WHITE ROOK
/// - 5/13 - BLACK/WHITE QUEEN
///
///
/// In order to identify equal positions, the pieces (except kings, of course) occur in the same order as the signature string tells.
/// For example, in KQB-KQN, the leftmost piece is a white queen, followed by a white bishop,
/// followed by a black queen followed by a black knight.
///
/// When there are two or more pieces of the same color and quality, their field numbers must be in descending order.
///
/// This is guaranteed when compressing a [Position].
///
/// Care must be taken when reflecting a [CPos] to get the pieces in the correct order and to correct the move index, if any.
///
/// Not all u64 values make for a valid [CPos]. The following fatal errors will abort the program:
/// - trying to compress a [Position] that is not a valid endgame as checked by [Position::validEndgame]
/// - trying to uncompress a [CPos] that contains a wrong piece code 8, 6 or 14
/// - trying to uncompress a [CPos] where any two field numbers are equal.
///   That is, every piece and the kings must have their own unique fields.
/// - trying to uncompress a [CPos] indicating a player to move for whom this position is invalid.
///
/// ### Bit layout
///
/// ```text
///
/// 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111
/// -FFF -fff -shv AAAA BBBB CCCC DDDD KKKK KKkk kkkk aaaa aabb bbbb cccc ccdd dddd
/// ```
/// where
/// - FFF the position state ([DRAW], [LOSS], [WINS], [BAD]) for [WHITE]
/// - fff the position state for [BLACK]
/// - shv applied transformations relative to a non-canonic [CPos] (see below)
/// - AAAA player and piece kind of piece A
/// - BBBB player and piece kind of piece B
/// - CCCC player and piece kind of piece C
/// - DDDD player and piece kind of piece D
/// - KKKKKK field of [WHITE] [KING]
/// - kkkkkk field of [BLACK] [KING]
/// - aaaaaa field of piece A
/// - bbbbbb field of piece B
/// - cccccc field of piece C
/// - dddddd field of piece D
///
///
/// When a position is searched in an `egtb` file,
/// it may have to be changed to a canonic one through certain transformations.
/// The result of the search is always the canonic position with indications what the applied transformations were:
/// The `s` bit is set, when the colours have been switched. The `h` bit is set when the position has been mirrored
/// horizontally. The `v` bit is set when the position has been mirrored vertically.
///
/// In the `moves` files, the 12 flag bits are
///
/// ```text
/// pkxx yyff ffff
/// ```
///
/// with
/// - `p` 0 for [BLACK] 1 for [WHITE]
/// - `k` if set, the piece to move is the king of the player indicated with `p`
/// - `xx` if `k` is 0 tells which piece to move (0 means 1st piece .. 3 means 4th piece)
/// - `yy` if this move is a pawn promotion, indicates the piece promoted to:
///    0 for knight, 1 for bishop, 2 for rook and 3 for queen
/// - `ffffff` field number of the target field
///
/// ## Mapping of [CPos] to ordinal offset and calculation of needed storag
///
/// Given a canonical position, we calculate the address (offset in an array) where we keep the status
/// of this and only this positon. No two canonical positions with the same signature must be mapped to
/// the same address.
/// In a game without [PAWN], the [WHITE] [KING] can occupy the fields that are on files 1 .. 4 and ranks A .. D.
/// This gives 16 possibilities.
///
/// The [WHITE] [KING], once placed, cancels 4 possible positions for the [BLACK] [KING] when on [A1], it cancels 6 possible
/// positions when on [B1], [C1], [D1], [A2], [A3] and [A4] and on the remaining 9 fields it cancels 9 possible
/// positions. Thus we have 1×60 + 6×58 + 9×55 or 903 possible [KING] configurations. In a game with [PAWN]s it will be
/// twice as much. By way of a constant table we can compute the king configuration ordinal number in one step.
///
/// The subsequent pieces that are in play can occupy an easily computable number of fields. The first piece can occupy 62
/// positions. The next one 61 and so forth. [PAWN]s make a difference, as they can only occupy 56 positions. We need to
/// reserve 56 as all other pieces could stand on fields that cannot be occupied by a PAWN. However, the second [PAWN] will have
/// only 55 ways to get placed, the 3rd 54 and the 4th 53.
///
/// Hence in a game without [PAWN]s we need to reserve 62×61×60×59 or *13,388,280* storage places for each king configuration.
/// This totals to 12,089,616,840. But we need only 4 bit for each position (2 bit for [WHITE] and 2 for [BLACK]). Hence the
/// biggest 6-piece table base will be less than 6GB in size.
/// The worst case is a position with 1 [PAWN] and 3 non-[PAWN]s. The total is 1806×62×61×60×48 or 19671240960 or less than 10GB.
///
/// Here is, then, the formula to compute an address:
///
///      A = OrdK(wK, bK) × PieceMax(0..3) + P0 × PieceMax(1..3) + P1 × PieceMax(2..3) + P2 × PieceMax(3..3) + P3
///
/// where
///
/// - OrdK(wK, bK) the ordinal number of the [KING] configuration 0..902 (or 1805 with [PAWN]s)
/// - PieceMax(i..j) product of PMax(i), PMax(i+1), ... PMax(j)
/// - PMax(i) = 62-i or 1 if piece i is [EMPTY] or 48-p for [PAWN]s preceeded by p other [PAWN]s
/// - Pi = field value where piece i stands - value of first possible field - number of occupied fields from the first possible field
///   up to piece i' field
///   0 if piece i is [EMPTY]
///
/// For example, in KQ-KP, the first possible position is: [WHITE] Ka1, Qb1, [BLACK] Kc1 a2
/// - OrdK(wK, bK) = 0 (it is the first possible [KING] configuration)
/// - PieceMax(0..3) = 62 × 48 × 1 × 1 = 2976
/// - P0 = 1 (b1) - 0 (a1) - 1 (because a1 is occupied) = 0
/// - PieceMax(1..3) = 48×1×1 = 48
/// - P1 = 0 (a2) first/lowest possible field for a [PAWN]
/// - P2 and P3 = 0, because [EMPTY]
/// Hence all the terms evalluate to 0 and the address is zero.
///
/// Now the last possible position: [WHITE] Kd8, Qg8, [BLACK] Kh8, h7
/// - OrdK(wk,bk) = 1805 (the last one)
/// - The PieceMax values are constant as before
/// - P0 = 62 (g8) - 0 (a1) - 2 (becasue d8 and h7 are occupied) = 60
/// - P1 = 55 (h7) - 8 (a2) = 47
/// - PieceMax(2..3) = 1
///
///     A = 1805 × 2976 + 60 × 48 + 47 = 5374607
///     Number of entries:   1806*2976 = 5374656
///
/// (There is a bit of waste just for the possibility that P0 reaches its theoretical maximum 62 values)
///

#[derive(Clone, Copy)]
pub struct CPos {
    pub bits: u64,
}

// 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111
// -FFF -fff -shv AAAA BBBB CCCC DDDD KKKK KKkk kkkk aaaa aabb bbbb cccc ccdd dddd

impl CPos {
    /// Mask the black flag bits in a CPos
    pub const WHITE_STATUS_BITS: u64 = 0x7000_0000_0000_0000u64;
    pub const WHITE_STATUS_SHIFT: u32 = CPos::WHITE_STATUS_BITS.trailing_zeros();
    /// Mask the white flag bits in a CPos
    pub const BLACK_STATUS_BITS: u64 = 0x0700_0000_0000_0000u64;
    pub const BLACK_STATUS_SHIFT: u32 = CPos::BLACK_STATUS_BITS.trailing_zeros();
    /// Mask the flag bits in a CPos
    pub const STATUS_BITS: u64 = CPos::BLACK_STATUS_BITS | CPos::WHITE_STATUS_BITS;
    /// Shift for flag bits in CPos
    pub const STATUS_SHIFT: u32 = CPos::STATUS_BITS.trailing_zeros();

    /// The `s` bit indicates when black and white have been swapped.
    pub const S_BIT: u64 = 0x0040_0000_0000_0000u64;
    /// The `h` bit indicates when the position has been mirrored horizontally.
    pub const H_BIT: u64 = 0x0020_0000_0000_0000u64;
    /// The `v` bit indicates when the position has been mirrored vertically.
    pub const V_BIT: u64 = 0x0010_0000_0000_0000u64;
    /// Bitmask for the transformer bits.
    pub const TRANS_BITS: u64 = CPos::S_BIT | CPos::H_BIT | CPos::V_BIT;

    /// Indicate a move for `WHITE` in a move CPos
    pub const WHITE_BIT: u64 = 0x8000_0000_0000_0000u64;
    /// Indicate a king move in a move CPos
    pub const KING_MOVE_BIT: u64 = 0x4000_0000_0000_0000u64;
    /// The index of the piece to move if it is not a `KING` move (move CPos)
    pub const PIECE_INDEX_BITS: u64 = 0x3000_0000_0000_0000u64;
    pub const PIECE_INDEX_SHIFT: u32 = CPos::PIECE_INDEX_BITS.trailing_zeros();
    /// The piece a pawn promotes to for promotion moves (move CPos)
    pub const PROMOTE_TO_BITS: u64 = 0x0C00_0000_0000_0000u64;
    pub const PROMOTE_TO_SHIFT: u32 = CPos::PROMOTE_TO_BITS.trailing_zeros();
    /// The number of the target field in a move CPos.
    pub const TARGET_BITS: u64 = 0x03f0_0000_0000_0000u64;
    pub const TARGET_SHIFT: u32 = CPos::TARGET_BITS.trailing_zeros();

    /// Mask the bits that count in comparisions
    pub const COMP_BITS: u64 = !0x7ff0_0000_0000_0000;
    /// Mask the code for piece 4
    pub const CODE4_BITS: u64 = CPos::CODE3_BITS >> 4;
    /// Mask the code for piece 3
    pub const CODE3_BITS: u64 = CPos::CODE2_BITS >> 4;
    /// Mask the code for piece 2
    pub const CODE2_BITS: u64 = CPos::CODE1_BITS >> 4;
    /// Mask the code for piece 1
    pub const CODE1_BITS: u64 = 0x0f << 48;
    /// shifts to get the numbers
    pub const CODE1_SHIFT: u32 = CPos::CODE1_BITS.trailing_zeros();
    pub const CODE2_SHIFT: u32 = CPos::CODE2_BITS.trailing_zeros();
    pub const CODE3_SHIFT: u32 = CPos::CODE3_BITS.trailing_zeros();
    /// This should be 6*6 = 36
    /// ```
    /// use rasch::cpos::CPos;
    /// assert_eq!(CPos::CODE4_SHIFT, 36)
    /// ```
    pub const CODE4_SHIFT: u32 = CPos::CODE4_BITS.trailing_zeros();
    /// Mask the field number of piece 4
    pub const FIELD4_BITS: u64 = 0x3f;
    pub const FIELD4_SHIFT: u32 = 0;
    /// Maskt the field number of piece 3
    pub const FIELD3_BITS: u64 = CPos::FIELD4_BITS << 6;
    pub const FIELD3_SHIFT: u32 = CPos::FIELD3_BITS.trailing_zeros();
    /// Maskt the field number of piece 2
    pub const FIELD2_BITS: u64 = CPos::FIELD3_BITS << 6;
    pub const FIELD2_SHIFT: u32 = CPos::FIELD2_BITS.trailing_zeros();
    /// Maskt the field number of piece 1
    pub const FIELD1_BITS: u64 = CPos::FIELD2_BITS << 6;
    pub const FIELD1_SHIFT: u32 = CPos::FIELD1_BITS.trailing_zeros();
    /// Mask the field of the white `KING`
    pub const WHITE_KING_BITS: u64 = CPos::BLACK_KING_BITS << 6;
    pub const WHITE_KING_SHIFT: u32 = CPos::WHITE_KING_BITS.trailing_zeros();
    /// Mask the field of the black `KING`
    pub const BLACK_KING_BITS: u64 = CPos::FIELD1_BITS << 6;
    pub const BLACK_KING_SHIFT: u32 = CPos::BLACK_KING_BITS.trailing_zeros();

    /// Mask for both kings, useful when we need to swap them
    pub const KING_BITS: u64 = CPos::WHITE_KING_BITS | CPos::BLACK_KING_BITS;

    /// Compress an ordinary position, which must be a valid endgame position.
    /// Sets either INVP or DRAW flags for both players.
    pub fn new(pos: &Position) -> CPos {
        // this makes sure that we never use more than 40 bits for encoding of pieces
        if !pos.validEndgame() {
            // it is possible that we reach en-passant positions, but we ignore them anyway
            let mut xpos = *pos;
            xpos.flags = xpos.flags - EN_PASSANT_BITS;
            if !xpos.validEndgame() {
                panic!("not an endgame position {}", encodeFEN(&xpos));
            }
        }

        let mut pieces = 0;
        let mut kings = 0;
        let mut codes = 0;

        // figure out validity
        let bstate = if pos.turn() == BLACK {
            if pos.valid() {
                DRAW
            } else {
                INVP
            }
        } else if pos.validForOpponent() {
            DRAW
        } else {
            INVP
        };
        let wstate = if pos.turn() == WHITE {
            if pos.valid() {
                DRAW
            } else {
                INVP
            }
        } else if pos.validForOpponent() {
            DRAW
        } else {
            INVP
        };
        let bflags = (bstate as u64) << CPos::BLACK_STATUS_SHIFT;
        let wflags = (wstate as u64) << CPos::WHITE_STATUS_SHIFT;

        // `BitSet` yields fields in ascending field order therefore we need to order them afterwards.
        for f in pos.occupied() {
            let p_on = pos.pieceOn(f);
            if p_on != KING {
                let color = if pos.whites.member(f) { 8 } else { 0 };
                let pcode = color | p_on as u64;
                codes <<= 4;
                codes |= pcode;
                pieces <<= 6;
                pieces |= f as u64;
            } else {
                if pos.whites.member(f) {
                    kings |= (f as u64) << CPos::WHITE_KING_SHIFT;
                } else {
                    kings |= (f as u64) << CPos::BLACK_KING_SHIFT;
                }
            }
        }
        CPos {
            bits: bflags | wflags | (pieces << CPos::FIELD4_SHIFT) | (codes << CPos::CODE4_SHIFT) | kings,
        }
        .ordered()
    }

    /// Re-construct position
    /// Before using it for anything, check validity and re-hash
    ///
    /// Beware that `cpos.uncompressed().compressed()` looses flags and piece index information.
    pub fn uncompressed(&self, player: Player) -> Position {
        let mut pos = Position {
            flags: if player == WHITE { WHITE_TO_MOVE } else { BitSet::empty() },
            whites: BitSet::empty(),
            pawnSet: BitSet::empty(),
            rookSet: BitSet::empty(),
            bishopSet: BitSet::empty(),
            hash: 0,
        };

        // place the kings
        pos = pos
            .place(WHITE, KING, bit(self.white_king()))
            .place(BLACK, KING, bit(self.black_king()));
        for u in 0u64..4 {
            let c = self.player_at(u);
            let p = self.piece_at(u);
            let f = self.field_at(u);

            if p != EMPTY {
                if pos.isEmpty(f) {
                    pos = pos.place(c, p, bit(f));
                } else {
                    panic!(
                        "Double occupation of field {} in compressed position {:?}.",
                        f, self
                    );
                }
            }
        }
        pos
    }

    /// compute the `Signature` of this position
    pub fn signature(&self) -> Signature {
        Signature::new(*self)
    }

    /// make a move iterator from this CPos
    pub fn move_iterator(&self, player: Player) -> CPosMoveIterator {
        CPosMoveIterator::new(*self, player)
    }

    /// make a reverse move iterator from this CPos
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
    pub fn reverse_move_iterator(&self, player: Player) -> CPosReverseMoveIterator {
        CPosReverseMoveIterator::new(*self, player)
    }

    /// Iterator for all the predecessors of a [CPos] where it is [player]s move, that have the same signature as [CPos].
    /// This excludes "undoing" [PAWN] promotions and captures as well as "uncapturing" pieces.
    pub fn predecessors(&'_ self, player: Player) -> impl Iterator<Item = (CPos, Move)> + '_ {
        let signature = self.signature();
        self.reverse_move_iterator(player)
            .filter(|mv| !mv.is_capture_by_pawn() && !mv.is_promotion())
            .map(|mv| (self.unapply(mv, EMPTY), mv))
            .filter(move |(c, _)| c.valid(player.opponent()) && c.signature() == signature)
    }

    /// Iterator for all the successors of a [CPos] assuming it is [player]s move.
    /// All sorts of moves are considered, so beware of successors with different signatures.
    pub fn successors(&'_ self, player: Player) -> impl Iterator<Item = (CPos, Move)> + '_ {
        self.move_iterator(player)
            .map(|mv| (self.apply(mv), mv))
            .filter(move |(c, _)| c.valid(player.opponent()))
    }

    /// Get the player for piece1, piece2, piece3 or piece4
    /// Wrong input values are masked away and if the index is not occupied, it will return `BLACK`
    pub fn player_at(&self, n: u64) -> Player {
        match n & 3 {
            0 => {
                if (self.bits & CPos::CODE1_BITS) >> CPos::CODE1_SHIFT >= 8 {
                    WHITE
                } else {
                    BLACK
                }
            }
            1 => {
                if (self.bits & CPos::CODE2_BITS) >> CPos::CODE2_SHIFT >= 8 {
                    WHITE
                } else {
                    BLACK
                }
            }
            2 => {
                if (self.bits & CPos::CODE3_BITS) >> CPos::CODE3_SHIFT >= 8 {
                    WHITE
                } else {
                    BLACK
                }
            }
            3 => {
                if (self.bits & CPos::CODE4_BITS) >> CPos::CODE4_SHIFT >= 8 {
                    WHITE
                } else {
                    BLACK
                }
            }
            _ => BLACK,
        }
    }

    /// Get the piece for piece1, piece2, piece3 or piece4
    /// Wrong input values are masked away and if the index is not occupied, it will return `EMPTY`
    pub fn piece_at(&self, n: u64) -> Piece {
        match n & 3 {
            0 => match ((self.bits & CPos::CODE1_BITS) >> CPos::CODE1_SHIFT) & 7 {
                p => Piece::from(p as u32),
            },
            1 => match ((self.bits & CPos::CODE2_BITS) >> CPos::CODE2_SHIFT) & 7 {
                p => Piece::from(p as u32),
            },
            2 => match ((self.bits & CPos::CODE3_BITS) >> CPos::CODE3_SHIFT) & 7 {
                p => Piece::from(p as u32),
            },
            3 => match ((self.bits & CPos::CODE4_BITS) >> CPos::CODE4_SHIFT) & 7 {
                p => Piece::from(p as u32),
            },
            _ => EMPTY,
        }
    }

    /// Get all information on piece [n].
    /// Valid index values are 0, 1, 2 or 3. Anything else yields [BLACK] [EMPTY] at [A1]
    pub fn www_at(&self, n: u64) -> WWW {
        match n {
            0 => {
                let code = (self.bits & CPos::CODE1_BITS) >> CPos::CODE1_SHIFT;
                let field = Field::from((self.bits & CPos::FIELD1_BITS) >> CPos::FIELD1_SHIFT);
                WWW(Player::from(code >= 8), Piece::from(code & 7), field)
            }
            1 => {
                let code = (self.bits & CPos::CODE2_BITS) >> CPos::CODE2_SHIFT;
                let field = Field::from((self.bits & CPos::FIELD2_BITS) >> CPos::FIELD2_SHIFT);
                WWW(Player::from(code >= 8), Piece::from(code & 7), field)
            }
            2 => {
                let code = (self.bits & CPos::CODE3_BITS) >> CPos::CODE3_SHIFT;
                let field = Field::from((self.bits & CPos::FIELD3_BITS) >> CPos::FIELD3_SHIFT);
                WWW(Player::from(code >= 8), Piece::from(code & 7), field)
            }
            3 => {
                let code = (self.bits & CPos::CODE4_BITS) >> CPos::CODE4_SHIFT;
                let field = Field::from((self.bits & CPos::FIELD4_BITS) >> CPos::FIELD4_SHIFT);
                WWW(Player::from(code >= 8), Piece::from(code & 7), field)
            }
            _ => WWW(BLACK, EMPTY, A1),
        }
    }

    /// Get the field for piece1, piece2, piece3 or piece4
    /// Wrong input values are masked away and if the index is not occupied, it will return `A1`
    pub fn field_at(&self, index: u64) -> Field {
        match index & 3 {
            0 => Field::from((self.bits & CPos::FIELD1_BITS) >> CPos::FIELD1_SHIFT),
            1 => Field::from((self.bits & CPos::FIELD2_BITS) >> CPos::FIELD2_SHIFT),
            2 => Field::from((self.bits & CPos::FIELD3_BITS) >> CPos::FIELD3_SHIFT),
            3 => Field::from((self.bits & CPos::FIELD4_BITS) >> CPos::FIELD4_SHIFT),
            _other => A1, // be happy, rustc
        }
    }

    /// get the state from the CPos
    pub fn state(&self, player: Player) -> CPosState {
        match player {
            WHITE => CPosState::from((self.bits & CPos::WHITE_STATUS_BITS) >> CPos::WHITE_STATUS_SHIFT),
            BLACK => CPosState::from((self.bits & CPos::BLACK_STATUS_BITS) >> CPos::BLACK_STATUS_SHIFT),
        }
    }

    /// make an identical CPos with a new state
    pub fn with_state(&self, ws: CPosState, bs: CPosState) -> CPos {
        CPos {
            bits: (self.bits & !CPos::STATUS_BITS)
                | ((ws as u64) << CPos::WHITE_STATUS_SHIFT)
                | ((bs as u64) << CPos::BLACK_STATUS_SHIFT),
        }
    }

    /// make an identical CPos with a new state for player
    pub fn with_state_for(&self, player: Player, ns: CPosState) -> CPos {
        if player == WHITE {
            CPos {
                bits: (self.bits & !CPos::WHITE_STATUS_BITS) | ((ns as u64) << CPos::WHITE_STATUS_SHIFT),
            }
        } else {
            CPos {
                bits: (self.bits & !CPos::BLACK_STATUS_BITS) | ((ns as u64) << CPos::BLACK_STATUS_SHIFT),
            }
        }
    }

    /// Check which piece of the CPos is on Field `field`
    ///
    /// Answer will be Ok(0..3) for piece1..piece4, or Err(KING) if it's a king or Err(EMPTY) if none.
    pub fn piece_index_by_field(&self, field: Field) -> Result<u64, Piece> {
        if self.white_king() == field || self.black_king() == field {
            Err(KING)
        } else if self.piece_at(0) != EMPTY && self.field_at(0) == field {
            Ok(0)
        } else if self.piece_at(1) != EMPTY && self.field_at(1) == field {
            Ok(1)
        } else if self.piece_at(2) != EMPTY && self.field_at(2) == field {
            Ok(2)
        } else if self.piece_at(3) != EMPTY && self.field_at(3) == field {
            Ok(3)
        } else {
            Err(EMPTY)
        }
    }

    /// Clear one of the pieces. The CPos must be `ordered()` afterwards.
    ///
    /// When the index is not 0,1,2 or 3, an unchanged CPos will be returned.
    pub fn clear_piece(&self, inx: u64) -> CPos {
        let mask = match inx {
            0 => CPos::CODE1_BITS | CPos::FIELD1_BITS,
            1 => CPos::CODE2_BITS | CPos::FIELD2_BITS,
            2 => CPos::CODE3_BITS | CPos::FIELD3_BITS,
            3 => CPos::CODE4_BITS | CPos::FIELD4_BITS,
            _ => 0,
        };
        CPos { bits: self.bits & !mask }
    }

    /// Move a piece to a different position. The CPos must be `ordered()` afterwards.
    ///
    /// When the index is not 0,1,2 or 3, an unchanged CPos will be returned.
    pub fn move_piece(&self, inx: u64, to: Field) -> CPos {
        let bits = self.bits;
        match inx {
            0 => CPos { bits: (bits & !CPos::FIELD1_BITS) | ((to as u64) << CPos::FIELD1_SHIFT) },
            1 => CPos { bits: (bits & !CPos::FIELD2_BITS) | ((to as u64) << CPos::FIELD2_SHIFT) },
            2 => CPos { bits: (bits & !CPos::FIELD3_BITS) | ((to as u64) << CPos::FIELD3_SHIFT) },
            3 => CPos { bits: (bits & !CPos::FIELD4_BITS) | ((to as u64) << CPos::FIELD4_SHIFT) },
            _ => CPos { bits },
        }
    }

    /// Move a king
    pub fn move_king(&self, player: Player, to: Field) -> CPos {
        match player {
            WHITE => CPos {
                bits: (self.bits & !CPos::WHITE_KING_BITS) | ((to as u64) << CPos::WHITE_KING_SHIFT),
            },
            BLACK => CPos {
                bits: (self.bits & !CPos::BLACK_KING_BITS) | ((to as u64) << CPos::BLACK_KING_SHIFT),
            },
        }
    }

    /// Change the piece at index. The colour of the piece will not change!
    ///
    /// When the index is not 0,1,2 or 3, an unchanged CPos will be returned.
    pub fn change_piece(&self, inx: u64, p: Piece) -> CPos {
        match inx {
            0 => CPos {
                bits: (self.bits & !(7 << CPos::CODE1_SHIFT)) | ((p as u64) << CPos::CODE1_SHIFT),
            },
            1 => CPos {
                bits: (self.bits & !(7 << CPos::CODE2_SHIFT)) | ((p as u64) << CPos::CODE2_SHIFT),
            },
            2 => CPos {
                bits: (self.bits & !(7 << CPos::CODE3_SHIFT)) | ((p as u64) << CPos::CODE3_SHIFT),
            },
            3 => CPos {
                bits: (self.bits & !(7 << CPos::CODE4_SHIFT)) | ((p as u64) << CPos::CODE4_SHIFT),
            },
            _ => CPos { bits: self.bits },
        }
    }

    /// Change the player at index. The piece itself will not change!
    /// Note that a WHITE EMPTY is a no go
    ///
    /// When the index is not 0,1,2 or 3, an unchanged CPos will be returned.
    pub fn change_player(&self, inx: u64, p: Player) -> CPos {
        let mask = if p == WHITE { 8 } else { 0 };
        match inx {
            0 => CPos { bits: (self.bits & !(8 << CPos::CODE1_SHIFT)) | (mask << CPos::CODE1_SHIFT) },
            1 => CPos { bits: (self.bits & !(8 << CPos::CODE2_SHIFT)) | (mask << CPos::CODE2_SHIFT) },
            2 => CPos { bits: (self.bits & !(8 << CPos::CODE3_SHIFT)) | (mask << CPos::CODE3_SHIFT) },
            3 => CPos { bits: (self.bits & !(8 << CPos::CODE4_SHIFT)) | (mask << CPos::CODE4_SHIFT) },
            _ => CPos { bits: self.bits },
        }
    }

    /// piece CPos::index for player and move
    pub fn piece_index_by_mv(&self, mv: Move) -> u64 {
        if self.piece_at(0) != EMPTY && self.player_at(0) == mv.player() && self.field_at(0) == mv.from() {
            0
        } else if self.piece_at(1) != EMPTY
            && self.player_at(1) == mv.player()
            && self.field_at(1) == mv.from()
        {
            1
        } else if self.piece_at(2) != EMPTY
            && self.player_at(2) == mv.player()
            && self.field_at(2) == mv.from()
        {
            2
        } else if self.piece_at(3) != EMPTY
            && self.player_at(3) == mv.player()
            && self.field_at(3) == mv.from()
        {
            3
        } else {
            0
        }
    }

    /// checks if [CPos] describes a possible chess board:
    /// - all pieces have unique fields
    /// - [PAWN]s are on ranks 2 to 7
    /// - the king configuration is valid
    /// (The position could still be invalid for both players for different reasons, but this is checked with [CPos::valid])
    pub fn is_possible(&self) -> bool {
        let occupied = (0..4).fold(bit(self.white_king()) + bit(self.black_king()), |a, e| {
            if self.piece_at(e) != EMPTY {
                a + bit(self.field_at(e))
            } else {
                a
            }
        });
        let pawns_ok = (0..4).fold(true, |a, e| {
            if self.piece_at(e) != PAWN {
                a
            } else {
                a && self.field_at(e) >= A2 && self.field_at(e) <= H7
            }
        });
        let pieces = (0..4).fold(2, |a, e| if self.piece_at(e) != EMPTY { a + 1 } else { a });
        occupied.card() == pieces && pawns_ok && mdb::kingConfig(self.white_king(), self.black_king()) < 1806
    }

    /// Return true iff, assuming it is `players` turn, the opponent's `KING` is **not** in check
    pub fn valid(&self, player: Player) -> bool {
        let his_king = self.players_king(player.opponent());
        let my_king = self.players_king(player);
        if mdb::kingTargets(my_king).member(his_king) {
            false
        } else {
            let occupied = bit(self.white_king())
                + bit(self.black_king())
                + if self.piece_at(0) != EMPTY { bit(self.field_at(0)) } else { BitSet::empty() }
                + if self.piece_at(1) != EMPTY { bit(self.field_at(1)) } else { BitSet::empty() }
                + if self.piece_at(2) != EMPTY { bit(self.field_at(2)) } else { BitSet::empty() }
                + if self.piece_at(3) != EMPTY { bit(self.field_at(3)) } else { BitSet::empty() };
            for u in 0..4 {
                if self.player_at(u) == player {
                    let from = self.field_at(u);
                    match self.piece_at(u) {
                        EMPTY => return true, /* no attacker can be found further left */
                        QUEEN => {
                            if occupied.intersection(mdb::canBishop(from, his_king)).null()
                                || occupied.intersection(mdb::canRook(from, his_king)).null()
                            {
                                return false;
                            }
                        }
                        ROOK => {
                            if occupied.intersection(mdb::canRook(from, his_king)).null() {
                                return false;
                            }
                        }
                        BISHOP => {
                            if occupied.intersection(mdb::canBishop(from, his_king)).null() {
                                return false;
                            }
                        }
                        KNIGHT => {
                            if mdb::knightTargets(from).member(his_king) {
                                return false;
                            }
                        }
                        PAWN => {
                            if player == BLACK && mdb::targetOfBlackPawns(his_king).member(from)
                                || player == WHITE && mdb::targetOfWhitePawns(his_king).member(from)
                            {
                                return false;
                            }
                        }
                        xp => panic!("bad piece {}", xp),
                    }
                }
            }
            true
        }
    }

    /// **true** if there are 4 pieces coded
    pub fn is_full(&self) -> bool {
        self.piece_at(3) != EMPTY
    }

    /// give the index of the first free place or panic
    pub fn index_of_free(&self) -> u64 {
        for u in 0..4 {
            if self.piece_at(u) == EMPTY {
                return u;
            }
        }
        panic!("no free piece in {:?}", self);
    }

    /// Return a `CPos` that yields this one when applying the move.
    pub fn unapply(&self, mv: Move, uncapture: Piece) -> CPos {
        let mut new = *self;

        if mv.is_capture_by_pawn() && (uncapture < PAWN || uncapture > QUEEN) {
            panic!(
                "unapplying capturing PAWN move {}, un-capturing {:?} not possible ",
                mv, uncapture
            );
        }
        if !mv.is_capture_by_pawn() && mv.piece() == PAWN && uncapture != EMPTY {
            panic!(
                "unapplying non-capturing PAWN move {}, un-capturing {:?} not possible ",
                mv, uncapture
            );
        }
        match new.piece_index_by_field(mv.to()) {
            Ok(u) => {
                if mv.promote() != EMPTY {
                    new = new.change_piece(u, PAWN);
                }
                new = new.move_piece(u, mv.from());
            }
            Err(KING) => new = new.move_king(mv.player(), mv.from()),
            Err(x) => {
                panic!("attempt to unapply {}, but {} is {:?}", mv, mv.to(), x);
            }
        }
        if uncapture != EMPTY {
            let u = new.index_of_free();
            new = new
                .change_piece(u, uncapture)
                .change_player(u, mv.player().opponent())
                .move_piece(u, mv.to());
        }
        new.ordered()
    }

    /// Return a `CPos` where the move has been applied.
    pub fn apply(&self, mv: Move) -> CPos {
        let mut new = *self;

        // clear target piece, if there is one
        match new.piece_index_by_field(mv.to()) {
            Ok(u) => new = new.clear_piece(u),
            Err(KING) => panic!("attempt to capture KING: {}  CPos {:?}", mv, new),
            Err(_) => { /* non capturing */ }
        }
        // move the piece or KING
        match new.piece_index_by_field(mv.from()) {
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

    /// make a `CPos` into a "move CPos" for a certain `Move`
    /// The move must have been computed for a `Position` that corresponds to this `CPos`,
    /// or else it will all be garbage.
    pub fn mpos_from_mv(&self, mv: Move) -> CPos {
        // check sanity
        if mv.piece() == KING && mv.from() != self.players_king(mv.player()) {
            panic!(
                "\nmpos_from_mv bad king move\n\
                mv {:?} {:?} {:?} {} {}    \
                self {:?}",
                mv.player(),
                mv.piece(),
                mv.promote(),
                mv.from(),
                mv.to(),
                self
            );
        }
        let inx = self.piece_index_by_mv(mv) & 3;
        if mv.piece() != KING
            && (self.piece_at(inx) != mv.piece()
                || self.player_at(inx) != mv.player()
                || self.field_at(inx) != mv.from())
        {
            panic!(
                "\nmpos_from_mv bad piece move\n\
                mv {:?} {:?} {:?} {} {}    \
                self {:?}",
                mv.player(),
                mv.piece(),
                mv.promote(),
                mv.from(),
                mv.to(),
                self
            );
        }
        CPos {
            bits: (self.bits & CPos::COMP_BITS)
                | if mv.player() == WHITE { CPos::WHITE_BIT } else { 0 }
                | if mv.piece() == KING { CPos::KING_MOVE_BIT } else { 0 }
                | ((self.piece_index_by_mv(mv) & 3) << CPos::PIECE_INDEX_SHIFT)
                | (match mv.promote() {
                    BISHOP => 1,
                    ROOK => 2,
                    QUEEN => 3,
                    _other => 0,
                } << CPos::PROMOTE_TO_SHIFT)
                | ((mv.to() as u64) << CPos::TARGET_SHIFT),
        }
    }

    /// Given some `CPos` with S_BIT, V_BIT or H_BIT set and a move `CPos` found for it, reconstruct the `Move`
    /// The `self` is only needed for the flags.
    pub fn mv_from_mpos(&self, mpos: CPos) -> Move {
        let player = if (mpos.bits & CPos::WHITE_BIT) != 0 { WHITE } else { BLACK };
        let piece = if (mpos.bits & CPos::KING_MOVE_BIT) != 0 {
            KING
        } else {
            mpos.piece_at((mpos.bits & CPos::PIECE_INDEX_BITS) >> CPos::PIECE_INDEX_SHIFT)
        };
        let from = if (mpos.bits & CPos::KING_MOVE_BIT) != 0 {
            if player == WHITE {
                mpos.white_king()
            } else {
                mpos.black_king()
            }
        } else {
            mpos.field_at((mpos.bits & CPos::PIECE_INDEX_BITS) >> CPos::PIECE_INDEX_SHIFT)
        };
        let to = Field::from((mpos.bits & CPos::TARGET_BITS) >> CPos::TARGET_SHIFT);
        let prom =
            if piece == PAWN && (player == WHITE && to.rank() == 8 || player == BLACK && to.rank() == 1) {
                match (mpos.bits & CPos::PROMOTE_TO_BITS) >> CPos::PROMOTE_TO_SHIFT {
                    0 => KNIGHT,
                    1 => BISHOP,
                    2 => ROOK,
                    3 => QUEEN,
                    _ => EMPTY,
                }
            } else {
                EMPTY
            };
        let mv = Move::new(player, piece, prom, from, to);
        let smv = if (self.bits & CPos::S_BIT) != 0 {
            Move::new(
                mv.player().opponent(),
                mv.piece(),
                mv.promote(),
                mv.from(),
                mv.to(),
            )
        } else {
            mv
        };
        let hmv = if (self.bits & CPos::H_BIT) != 0 { smv.mirror_h() } else { smv };
        let rmv = if (self.bits & CPos::V_BIT) != 0 { hmv.mirror_v() } else { hmv };
        rmv
    }

    /// get the field number of the white king
    pub fn white_king(&self) -> Field {
        Field::from((self.bits & CPos::WHITE_KING_BITS) >> CPos::WHITE_KING_SHIFT)
    }

    /// get the field number of the black king
    pub fn black_king(&self) -> Field {
        Field::from((self.bits & CPos::BLACK_KING_BITS) >> CPos::BLACK_KING_SHIFT)
    }

    /// get the field number of player's king
    pub fn players_king(&self, player: Player) -> Field {
        if player == WHITE {
            self.white_king()
        } else {
            self.black_king()
        }
    }

    /// clear the S, V and H bits
    pub fn clear_trans(&self) -> CPos {
        CPos { bits: self.bits & !CPos::TRANS_BITS }
    }

    /// exchange kings and switch the colours of all pieces - needs to get [CPos::ordered].
    fn switch_black_and_white(&self) -> CPos {
        let mut bits = self.bits;
        // we need to flip the WHITE/BLACK bits on occupied positions
        if bits & CPos::CODE1_BITS != 0 {
            bits ^= 8 << CPos::CODE1_SHIFT;
        }
        if bits & CPos::CODE2_BITS != 0 {
            bits ^= 8 << CPos::CODE2_SHIFT;
        }
        if bits & CPos::CODE3_BITS != 0 {
            bits ^= 8 << CPos::CODE3_SHIFT;
        }
        if bits & CPos::CODE4_BITS != 0 {
            bits ^= 8 << CPos::CODE4_SHIFT;
        }
        // we also need to exchange kings
        bits = (bits & !CPos::KING_BITS)                      // clear affected bits
                    | ((bits & CPos::BLACK_KING_BITS)>>CPos::BLACK_KING_SHIFT << CPos::WHITE_KING_SHIFT)     // move the black king
                    | ((bits & CPos::WHITE_KING_BITS)>>CPos::WHITE_KING_SHIFT << CPos::BLACK_KING_SHIFT)     // move the white king
                    ;
        CPos { bits: bits ^ CPos::S_BIT }
    }

    /// Make a canonical CPos for lookup in the DB
    /// A canonical CPos has a canonic signature and
    /// the white king is in the left half and,
    /// if there are now pawns, in the lower half.
    ///
    /// We pass the signature here explicitely to save calls to signature(), this way
    /// a client can store it when it is needed more than once.
    /// It is to be understood that only calls semantically equivalent to
    /// ```java
    /// x.canonical(x.signature)
    /// ```
    /// make any sense.

    pub fn canonical(&self, sig: Signature) -> CPos {
        let has_pawns = sig.has_pawns();
        let mut this = if sig.is_canonic() {
            *self
        } else {
            if has_pawns {
                // we need to mirror the board horizontally when pawns are present
                self.switch_black_and_white().mirror_h()
            } else {
                self.switch_black_and_white()
            }
        };
        let kf = this.white_king();
        if !LEFT_HALF.member(kf) {
            this = this.mirror_v();
        }
        debug_assert!(LEFT_HALF.member(this.white_king()));
        // the king is now in the left half
        if !has_pawns && !LOWER_HALF.member(kf) {
            this = this.mirror_h();
            debug_assert!(LOWER_LEFT_QUARTER.member(this.white_king()));
        }
        // make sure this **is** ordered.
        this.ordered()
    }

    /// Does this `CPos` have black/white switched?
    ///
    /// **Note**: the provided `CPos` must be canonical! Use for results of `find()` only.
    pub const fn canonic_has_bw_switched(&self) -> bool {
        (self.bits & CPos::S_BIT) != 0
    }

    /// Was this `CPos` mirrored horizontally?
    ///
    /// **Note**: the provided `CPos` must be canonical! Use for results of `find()` only.
    pub const fn canonic_was_mirrored_h(&self) -> bool {
        (self.bits & CPos::H_BIT) != 0
    }

    /// Was this `CPos` mirrored vertically?
    ///
    /// **Note**: the provided `CPos` must be canonical! Use for results of `find()` only.
    pub const fn canonic_was_mirrored_v(&self) -> bool {
        (self.bits & CPos::V_BIT) != 0
    }

    /// Was this `CPos` mirrored?
    ///
    /// **Note**: the provided `CPos` must be canonical! Use for results of `find()` only.
    pub const fn canonic_was_mirrored(&self) -> bool {
        (self.bits & (CPos::H_BIT | CPos::V_BIT)) != 0
    }

    /// convenience for `x.canonical(x.signature)`
    pub fn mk_canonical(&self) -> CPos {
        self.canonical(self.signature())
    }

    #[inline]
    /// Returns this CPos with the flags flipped
    /// This is needed on a search result of a position that needed color changes to become canonical
    pub const fn flipped_flags(&self) -> CPos {
        self.swap(CPos::BLACK_STATUS_BITS, CPos::WHITE_STATUS_BITS)
    }

    /// Swap the indicated bits. The masks must not overlap and have the same number of bits.
    #[inline]
    pub const fn swap(&self, mask1: u64, mask2: u64) -> Self {
        CPos { bits: CPos::swap_bits(self.bits, mask1, mask2) }
    }

    /// swaps the bits masked by m1 with the one masked by m2
    /// The masks must not overlap and have the same shape and number of bits.
    /// ```
    /// use rasch::cpos::CPos;
    /// assert_eq!(CPos::swap_bits(0x1234, 0xf0f0, 0x0f0f), 0x2143);
    /// ```
    pub const fn swap_bits(bits: u64, m1: u64, m2: u64) -> u64 {
        // now make the ALU hot
        (bits & !(m1|m2)) // clear affected bits
            | (((bits&m1) >> m1.trailing_zeros()) << m2.trailing_zeros())  // move m1 bits to m2
            | (((bits&m2) >> m2.trailing_zeros()) << m1.trailing_zeros()) // move m2 bits to m1
    }

    #[inline]
    /// gives CODE flags and field number combined into a number
    const fn ccval_A(bits: u64) -> u64 {
        (((bits & CPos::CODE1_BITS) >> CPos::CODE1_SHIFT) << 6)
            | ((bits & CPos::FIELD1_BITS) >> CPos::FIELD1_SHIFT)
    }
    #[inline]
    /// gives CODE flags and field number combined into a number
    const fn ccval_B(bits: u64) -> u64 {
        (((bits & CPos::CODE2_BITS) >> CPos::CODE2_SHIFT) << 6)
            | ((bits & CPos::FIELD2_BITS) >> CPos::FIELD2_SHIFT)
    }
    #[inline]
    /// gives CODE flags and field number combined into a number
    const fn ccval_C(bits: u64) -> u64 {
        (((bits & CPos::CODE3_BITS) >> CPos::CODE3_SHIFT) << 6)
            | ((bits & CPos::FIELD3_BITS) >> CPos::FIELD3_SHIFT)
    }
    #[inline]
    /// gives CODE flags and field number combined into a number
    const fn ccval_D(bits: u64) -> u64 {
        (((bits & CPos::CODE4_BITS) >> CPos::CODE4_SHIFT) << 6)
            | ((bits & CPos::FIELD4_BITS) >> CPos::FIELD4_SHIFT)
    }

    #[inline]
    /// 2 pieces must be swapped if left piece code is smaller or field is smaller for equal pieces
    fn need_swap(ccv1: u64, ccv2: u64) -> bool {
        ccv1 < ccv2
    }

    /// Return a CPos with the pieces ordered in such a way that white pieces come before black ones,
    /// better pieces before not so good ones, but among equal pieces of the same player the fields are ascending.
    pub fn ordered(&self) -> CPos {
        let mut bits = self.bits;
        if CPos::need_swap(CPos::ccval_A(bits), CPos::ccval_B(bits)) {
            bits = CPos::swap_bits(bits, CPos::CODE1_BITS, CPos::CODE2_BITS);
            bits = CPos::swap_bits(bits, CPos::FIELD1_BITS, CPos::FIELD2_BITS);
        }
        if CPos::need_swap(CPos::ccval_A(bits), CPos::ccval_C(bits)) {
            bits = CPos::swap_bits(bits, CPos::CODE1_BITS, CPos::CODE3_BITS);
            bits = CPos::swap_bits(bits, CPos::FIELD1_BITS, CPos::FIELD3_BITS);
        }
        if CPos::need_swap(CPos::ccval_A(bits), CPos::ccval_D(bits)) {
            bits = CPos::swap_bits(bits, CPos::CODE1_BITS, CPos::CODE4_BITS);
            bits = CPos::swap_bits(bits, CPos::FIELD1_BITS, CPos::FIELD4_BITS);
        }
        // A is now correct
        if CPos::need_swap(CPos::ccval_B(bits), CPos::ccval_C(bits)) {
            bits = CPos::swap_bits(bits, CPos::CODE2_BITS, CPos::CODE3_BITS);
            bits = CPos::swap_bits(bits, CPos::FIELD2_BITS, CPos::FIELD3_BITS);
        }
        if CPos::need_swap(CPos::ccval_B(bits), CPos::ccval_D(bits)) {
            bits = CPos::swap_bits(bits, CPos::CODE2_BITS, CPos::CODE4_BITS);
            bits = CPos::swap_bits(bits, CPos::FIELD2_BITS, CPos::FIELD4_BITS);
        }
        // B is now correct
        // swap C and D if needed
        if CPos::need_swap(CPos::ccval_C(bits), CPos::ccval_D(bits)) {
            bits = CPos::swap_bits(bits, CPos::CODE3_BITS, CPos::CODE4_BITS);
            bits = CPos::swap_bits(bits, CPos::FIELD3_BITS, CPos::FIELD4_BITS);
        }
        CPos { bits }
    }

    /// Compute the nibble-address of a **canonical**, **ordered** [CPos]
    ///
    ///  A = OrdK(wK, bK) × PieceMax(0..3) + P0 × PieceMax(1..3) + P1 × PieceMax(2..3) + P2 × PieceMax(3..3) + P3
    ///
    /// where
    ///
    /// - OrdK(wK, bK) the ordinal number of the [KING] configuration 0..902 (or 1805 with [PAWN]s)
    /// - PieceMax(i..j) product of PMax(i), PMax(i+1), ... PMax(j)
    /// - PMax(i) = 62-i or 1 if piece i is [EMPTY] or 48-p for [PAWN]s preceeded by p other [PAWN]s
    /// - Pi = field value where piece i stands - value of first possible field - number of occupied fields from the first possible field
    ///   up to piece i' field
    ///   0 if piece i is [EMPTY]
    pub fn canonic_addr(&self) -> NibbleAddr {
        let wK = self.white_king();
        let bK = self.black_king();
        let ord_k = mdb::kingConfig(wK, bK) as u64;
        let www0 = self.www_at(0);
        let www1 = self.www_at(1);
        let www2 = self.www_at(2);
        let www3 = self.www_at(3);
        // let n_pawns0 = 0;
        let n_pawns1 = if www0.1 == PAWN { 1 } else { 0 };
        let n_pawns2 = n_pawns1 + if www1.1 == PAWN { 1 } else { 0 };
        let n_pawns3 = n_pawns2 + if www2.1 == PAWN { 1 } else { 0 };
        let p_max0 = match www0.1 {
            EMPTY => 1,
            PAWN => 48,
            _ => 62,
        };
        let p_max1 = match www1.1 {
            EMPTY => 1,
            PAWN => 48 - n_pawns1,
            _ => 61,
        };
        let p_max2 = match www2.1 {
            EMPTY => 1,
            PAWN => 48 - n_pawns2,
            _ => 60,
        };
        let p_max3 = match www3.1 {
            EMPTY => 1,
            PAWN => 48 - n_pawns3,
            _ => 59,
        };
        let between = |lower, upper, f| if f >= lower && f < upper { 1 } else { 0 };
        let same = |a: WWW, b: WWW| if a.0 == b.0 && a.1 == b.1 { 1 } else { 0 };
        let p0 = match www0.1 {
            EMPTY => 0,
            PAWN => {
                www0.2 as u64
                    - (A2 as u64)
                    - between(A2, www0.2, wK)
                    - between(A2, www0.2, bK)
                    - same(www0, www1)
                    - same(www0, www2)
                    - same(www0, www3)
            }
            _ => {
                www0.2 as u64
                    - between(A1, www0.2, wK)
                    - between(A1, www0.2, bK)
                    - same(www0, www1)
                    - same(www0, www2)
                    - same(www0, www3)
            }
        };
        let p1 = match www1.1 {
            EMPTY => 0,
            PAWN => {
                www1.2 as u64
                    - (A2 as u64)
                    - between(A2, www1.2, wK)
                    - between(A2, www1.2, bK)
                    - between(A2, www1.2, www0.2)
                    - same(www1, www2)
                    - same(www1, www3)
            }
            _ => {
                www1.2 as u64
                    - between(A1, www1.2, wK)
                    - between(A1, www1.2, bK)
                    - between(A1, www1.2, www0.2)
                    - same(www1, www2)
                    - same(www1, www3)
            }
        };
        let p2 = match www2.1 {
            EMPTY => 0,
            PAWN => {
                www2.2 as u64
                    - (A2 as u64)
                    - between(A2, www2.2, wK)
                    - between(A2, www2.2, bK)
                    - between(A2, www2.2, www0.2)
                    - between(A2, www2.2, www1.2)
                    - same(www2, www3)
            }
            _ => {
                www2.2 as u64
                    - between(A1, www2.2, wK)
                    - between(A1, www2.2, bK)
                    - between(A1, www2.2, www0.2)
                    - between(A1, www2.2, www1.2)
                    - same(www2, www3)
            }
        };
        let p3 = match www3.1 {
            EMPTY => 0,
            PAWN => {
                www3.2 as u64
                    - (A2 as u64)
                    - between(A2, www3.2, wK)
                    - between(A2, www3.2, bK)
                    - between(A2, www3.2, www0.2)
                    - between(A2, www3.2, www1.2)
                    - between(A2, www3.2, www2.2)
            }
            _ => {
                www3.2 as u64
                    - between(A1, www3.2, wK)
                    - between(A1, www3.2, bK)
                    - between(A1, www3.2, www0.2)
                    - between(A1, www3.2, www1.2)
                    - between(A1, www3.2, www2.2)
            }
        };

        NibbleAddr(
            (ord_k * p_max0 * p_max1 * p_max2 * p_max3
                + p0 * p_max1 * p_max2 * p_max3
                + p1 * p_max2 * p_max3
                + p2 * p_max3
                + p3) as usize,
        )
    }

    fn next_pc(&self, pawns: bool, u: u64) -> Option<u64> {
        // any index other than 0,1,2,3 requests next king config
        if u > 3 {
            // make next king configuration
            let ord_k = mdb::kingConfig(self.white_king(), self.black_king());
            let max = if pawns { 1805 } else { 902 };
            if ord_k >= max {
                return None;
            }
            let mut kinx = ((self.white_king() as u64) << 6) + (self.black_king() as u64) + 1;
            while kinx < 64 * 64 {
                let w = Field::from(kinx >> 6);
                let b = Field::from(kinx & 0x3f);
                if mdb::kingConfig(w, b) <= max {
                    return Some(self.move_king(WHITE, w).move_king(BLACK, b).bits);
                }
                kinx += 1;
            }
            panic!("big oops for ord_k={} max={} {:?}", ord_k, max, self);
        }

        let prev_u = if u == 0 { 42 } else { u - 1 };
        // is it empty, perhaps?
        if self.piece_at(u) == EMPTY {
            return self.next_pc(pawns, prev_u);
        }

        // at this point, we have regular pieces only
        let mut next = CPos { bits: self.bits };

        loop {
            let WWW(player, piece, f) = next.www_at(u);
            let WWW(prev_pl, prev_piece, prev_f) =
                if u > 0 { next.www_at(u - 1) } else { WWW(BLACK, EMPTY, A1) };
            let limit = {
                // if previous piece is the same colour and kind
                // then the limit is the field before this one
                if prev_pl == player && prev_piece == piece {
                    if prev_f > A1 {
                        Field::from((prev_f as u64) - 1)
                    } else {
                        A1 // cause immediate overflow
                    }
                } else if piece == PAWN {
                    H7
                } else {
                    H8
                }
            };
            // do we have overflow
            if f >= limit {
                match next.clear_piece(u).next_pc(pawns, prev_u) {
                    None => return None,
                    Some(nxt) => {
                        // reconstruct CPos with our piece
                        next = CPos { bits: nxt }
                            .change_player(u, player)
                            .change_piece(u, piece)
                            .move_piece(u, if piece == PAWN { A2 } else { A1 });
                    }
                }
            } else {
                next = next.move_piece(u, Field::from((f as u64) + 1));
            }
            let f = next.field_at(u);
            let f0 = next.field_at(0);
            let f1 = next.field_at(1);
            let f2 = next.field_at(2);

            if f != next.white_king()
                && f != next.black_king()
                && (u == 0
                    || u == 1 && f != f0
                    || u == 2 && f != f0 && f != f1
                    || u == 3 && f != f0 && f != f1 && f != f2)
            {
                return Some(next.bits);
            }
        }
    }

    /// read a CPos at the current position
    pub fn read(file: &mut File) -> Result<CPos, std::io::Error> {
        let mut buf = [0u8; 8];
        file.read_exact(&mut buf)?;
        Ok(CPos { bits: u64::from_ne_bytes(buf) })
    }

    /// read a CPos at the current position from a buffered reader
    pub fn read_seq(file: &mut BufReader<File>) -> Result<CPos, std::io::Error> {
        let mut buf = [0u8; 8];
        file.read_exact(&mut buf)?;
        Ok(CPos { bits: u64::from_ne_bytes(buf) })
    }

    /// like `read_seq`, but maps Err(UnexpectedEOF) to OK(None) and
    /// Ok(v) to Ok(Some(v))
    pub fn read_seq_with_eof(file: &mut BufReader<File>) -> Result<Option<CPos>, std::io::Error> {
        let mut buf = [0u8; 8];
        match file.read_exact(&mut buf) {
            Ok(_) => Ok(Some(CPos { bits: u64::from_ne_bytes(buf) })),
            Err(x) if x.kind() == UnexpectedEof => Ok(None),
            Err(other) => Err(other),
        }
    }

    /// read a CPos at some seek position
    pub fn read_at(file: &mut File, wo: SeekFrom) -> Result<CPos, std::io::Error> {
        file.seek(wo)?;
        CPos::read(file)
    }

    /// write a CPos at the current file position
    pub fn write(&self, file: &mut File) -> Result<(), std::io::Error> {
        let buf = self.bits.to_ne_bytes();
        file.write_all(&buf)
    }

    /// write a CPos all sequentially
    pub fn write_seq(&self, file: &mut std::io::BufWriter<File>) -> Result<(), std::io::Error> {
        let buf = self.bits.to_ne_bytes();
        file.write_all(&buf)
    }

    /// write a CPos at some seek position
    pub fn write_at(&self, file: &mut File, wo: SeekFrom) -> Result<(), std::io::Error> {
        file.seek(wo)?;
        self.write(file)
    }

    /// Find a CPos in the database files.
    ///
    /// Absent I/O errors, the result will be the canonical `CPos` that is the same as the one searched for,
    /// with appropriately set flags.
    ///
    /// The databse consists of a number of files, each named after the signature of the `CPos`s it contains.
    /// To avoid frequent opening and closing of several files, `File` objects of files
    /// that have contributed in searching are remembered in the `hash` object. Only when this hash goes out
    /// of scope or entries getting removed will the corresponding file actually be closed.
    ///
    /// For a one time lookup, one can simply pass an empty hash. Subsequent searches may benefit a little from
    /// already opened files, but a real difference is seen with many thousand lookups only.
    pub fn find(&self, hash: &mut EgtbMap) -> Result<CPos, String> {
        let selfsig = self.signature();
        let canon = self.canonical(selfsig);
        canon
            .find_canonic(selfsig.mk_canonic(), hash)
            .map(|cp| // copy TRANS_BITS from canon to result 
                CPos { bits: (cp.bits & !CPos::TRANS_BITS) | (canon.bits & CPos::TRANS_BITS) })
    }

    pub fn egtb_mmap<'a>(canonsig: Signature, ext: &str, hash: &'a mut EgtbMap) -> Result<(), String> {
        if !hash.contains_key(&canonsig) {
            let path = mk_egtb_path(canonsig, ext);

            let mm = {
                let path: &str = &path;
                let file = OpenOptions::new()
                    .read(true)
                    .open(path)
                    .map_err(|e| format!("Can't read {} ({})", path, e))?;
                let map = unsafe { Mmap::map(&file) }.map_err(|e| format!("Can't mmap {} ({})", path, e))?;
                let array = unsafe { slice::from_raw_parts(map.as_ptr(), map.len()) };
                (map, array)
            };

            // let mm = byte_ro_map(&path)?;
            hash.insert(canonsig, mm);
        }
        Ok(())
    }

    pub fn moves_mmap<'a>(canonsig: Signature, hash: &'a mut MovesMap) -> Result<(), String> {
        if !hash.contains_key(&canonsig) {
            let path = mk_egtb_path(canonsig, "moves");
            let mm = {
                let path: &str = &path;
                let file = OpenOptions::new()
                    .read(true)
                    .open(path)
                    .map_err(|e| format!("Can't read {} ({})", path, e))?;
                let map = unsafe { Mmap::map(&file) }.map_err(|e| format!("Can't mmap {} ({})", path, e))?;
                let array = unsafe { slice::from_raw_parts(map.as_ptr().cast::<CPos>(), map.len() / 8) };
                (map, array)
            };

            // let mm = byte_ro_map(&path)?;
            hash.insert(canonsig, mm);
        }
        Ok(())
    }

    /// Look up a CPos in an u8 slice and sets the state flags accordingly in the result.
    ///
    /// Fails if the address is too big.
    pub fn lookup_canonic(&self, array: &[u8]) -> Result<CPos, String> {
        let addr = self.canonic_addr();
        if addr.bytes() > array.len() {
            Err(format!(
                "array length {} < {} for {:?}  {:?}",
                array.len(),
                addr.bytes(),
                addr,
                self
            ))
        } else {
            let nibble = addr.get_nibble(array);
            let ws = nibble >> 2;
            let bs = nibble & 3;
            Ok(self.with_state(CPosState::from(ws), CPosState::from(bs)))
        }
    }

    /// Constructs a nibble from the state information and puts it in an array.
    pub fn set_canonic(&self, array: &mut [u8]) {
        let addr = self.canonic_addr();
        if addr.bytes() > array.len() {
            panic!("array length {} < {} for {:?}", array.len(), addr.bytes(), self);
        }
        let ws = self.state(WHITE) as u8;
        let bs = self.state(BLACK) as u8;
        addr.set_nibble(array, (ws << 2) | bs);
    }

    /// like [CPos::with_state_for] but updates it also in the database
    /// The [CPos] must be canonic.
    pub fn with_db_state_for(&self, player: Player, state: CPosState, array: &mut [u8]) -> CPos {
        let addr = self.canonic_addr();
        if addr.bytes() > array.len() {
            panic!("array length {} < {} for {:?}", array.len(), addr.bytes(), self);
        }
        let nibble = addr.get_nibble(array);
        let ws = if player == WHITE { state as u8 } else { nibble >> 2 };
        let bs = if player == BLACK { state as u8 } else { nibble & 3 };
        addr.set_nibble(array, (ws << 2) | bs);
        self.with_state(CPosState::from(ws), CPosState::from(bs))
    }

    /// A variant of `find` where the searched `CPos` is guaranteed canonical.
    pub fn find_canonic(&self, canonsig: Signature, hash: &mut EgtbMap) -> Result<CPos, String> {
        CPos::egtb_mmap(canonsig, "egtb", hash)?;

        // the unwrap is justified because of the insert() above
        // we don't want hash.entry(canonsig).or_insert(...) because this would open the file every time
        // we don't want hash.entry(canonsig).or_insert_with(|| ...) either, because it destroys propagation
        // of errors upwards.
        let blubb = hash.get_mut(&canonsig).unwrap();
        self.lookup_canonic(blubb.1)
    }

    pub fn mpos_player(&self) -> Player {
        if (self.bits & CPos::WHITE_BIT) != 0 {
            WHITE
        } else {
            BLACK
        }
    }

    pub fn mpos_to_cpos(&self) -> CPos {
        CPos { bits: (self.bits & !CPos::WHITE_BIT) & CPos::COMP_BITS }
    }

    /// Find the winning move for [Player].
    ///
    /// If this is a winning position for [player], the result will be the [Move], applicable to this position, that leads to a win.
    ///
    /// If this is no winning position, the result will be
    ///
    ///     Err("CANT WIN")
    ///
    /// The database could be inconsistent and the move could be missing. In this
    /// case one gets
    ///
    ///     Err("NOT FOUND")
    ///
    /// Other errors indicate I/O errors.
    pub fn find_move(
        &self,
        player: Player,
        e_hash: &mut EgtbMap,
        m_hash: &mut MovesMap,
    ) -> Result<Move, String> {
        let canonic = self.clear_trans().find(e_hash)?;
        let cplayer = if canonic.canonic_has_bw_switched() { player.opponent() } else { player };
        let mpos = canonic.find_canonic_mpos(canonic.signature(), cplayer, m_hash)?;
        Ok(canonic.mv_from_mpos(mpos))
    }

    /// Find the db state for [Player]
    pub fn find_state(&self, player: Player, e_hash: &mut EgtbMap) -> Result<CPosState, String> {
        let canonic = self.clear_trans().find(e_hash)?;
        Ok(if canonic.canonic_has_bw_switched() {
            canonic.state(player.opponent())
        } else {
            canonic.state(player)
        })
    }

    /// Find a move position associated with a **canonic** [CPos].
    /// - it is an error if the status of the selected player is not [WINS]
    /// - it is an error if the move is not found.
    pub fn find_canonic_mpos(
        self,
        canonsig: Signature,
        player: Player,
        hash: &mut MovesMap,
    ) -> Result<CPos, String> {
        let this = if self.state(player) == WINS {
            Ok(CPos {
                bits: if player == WHITE { CPos::WHITE_BIT } else { 0 } | (self.bits & CPos::COMP_BITS),
            })
        } else {
            Err(String::from("CANT WIN"))
        }?;

        CPos::moves_mmap(canonsig, hash)?;

        // the unwrap is justified because of the egtb_open() above
        let blubb = hash.get_mut(&canonsig).unwrap();
        match blubb.1.binary_search(&this) {
            Ok(u) => Ok(blubb.1[u]),
            Err(_) => Err(String::from("NOT FOUND")),
        }
    }

    pub fn find_canonic_mpos_index(
        self,
        canonsig: Signature,
        player: Player,
        hash: &mut MovesMap,
    ) -> Result<usize, String> {
        let this = if self.state(player) == WINS {
            Ok(CPos {
                bits: if player == WHITE { CPos::WHITE_BIT } else { 0 } | (self.bits & CPos::COMP_BITS),
            })
        } else {
            Err(String::from("CANT WIN"))
        }?;

        CPos::moves_mmap(canonsig, hash)?;

        // the unwrap is justified because of the egtb_open() above
        let blubb = hash.get_mut(&canonsig).unwrap();
        blubb.1.binary_search(&this).map_err(|_| "NOT FOUND".to_string())
    }

    /// debugging output for a move `CPos`
    pub fn mpos_debug(&self) -> String {
        let inx = (self.bits & CPos::PIECE_INDEX_BITS) >> CPos::PIECE_INDEX_SHIFT;
        let to = Field::from((self.bits & CPos::TARGET_BITS) >> CPos::TARGET_SHIFT);
        let kingmv = (self.bits & CPos::KING_MOVE_BIT) != 0;
        format!(
            "{} {} {}  {}  {} {} {} {} {} {}",
            if (self.bits & CPos::WHITE_BIT) != 0 { WHITE } else { BLACK },
            if kingmv {
                "KING".to_string()
            } else {
                format!("P{}={}", 1 + inx, self.piece_at(inx))
            },
            if self.piece_at(inx) == PAWN && !kingmv && (to.rank() == 1 || to.rank() == 8) {
                format!(
                    "→{}",
                    match (self.bits & CPos::PROMOTE_TO_BITS) >> CPos::PROMOTE_TO_SHIFT {
                        0 => "K",
                        1 => "B",
                        2 => "R",
                        3 => "Q",
                        _ => "rustc, be happy",
                    }
                )
            } else {
                "--".to_string()
            },
            to,
            WWW(WHITE, KING, self.white_king()),
            WWW(BLACK, KING, self.black_king()),
            self.www_at(0),
            self.www_at(1),
            self.www_at(2),
            self.www_at(3)
        )
    }
}

impl Iterator for CPos {
    type Item = CPos;

    fn next(&mut self) -> Option<Self::Item> {
        let bits = self.bits;
        if bits == 0 {
            None
        } else {
            self.bits = self
                .next_pc(
                    self.piece_at(0) == PAWN
                        || self.piece_at(1) == PAWN
                        || self.piece_at(2) == PAWN
                        || self.piece_at(3) == PAWN,
                    3,
                )
                .unwrap_or(0);
            Some(CPos { bits })
        }
    }
}

impl fmt::Debug for CPos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&format!(
            "{:?}/{:?} {}{}{}  {} {} {} {} {} {}",
            self.state(WHITE),
            self.state(BLACK),
            if self.canonic_has_bw_switched() { "s" } else { "-" },
            if self.canonic_was_mirrored_h() { "h" } else { "-" },
            if self.canonic_was_mirrored_v() { "v" } else { "-" },
            WWW(WHITE, KING, self.white_king()),
            WWW(BLACK, KING, self.black_king()),
            self.www_at(0),
            self.www_at(1),
            self.www_at(2),
            self.www_at(3)
        ))
    }
}

impl PartialEq for CPos {
    fn eq(&self, other: &CPos) -> bool {
        self.bits & CPos::COMP_BITS == other.bits & CPos::COMP_BITS
    }
}

impl Eq for CPos {}

impl PartialOrd for CPos {
    fn partial_cmp(&self, other: &CPos) -> Option<Ordering> {
        (self.bits & CPos::COMP_BITS).partial_cmp(&(other.bits & CPos::COMP_BITS))
    }
    fn lt(&self, other: &CPos) -> bool {
        (self.bits & CPos::COMP_BITS) < (other.bits & CPos::COMP_BITS)
    }
    fn le(&self, other: &CPos) -> bool {
        (self.bits & CPos::COMP_BITS) <= (other.bits & CPos::COMP_BITS)
    }
    fn gt(&self, other: &CPos) -> bool {
        (self.bits & CPos::COMP_BITS) > (other.bits & CPos::COMP_BITS)
    }
    fn ge(&self, other: &CPos) -> bool {
        (self.bits & CPos::COMP_BITS) >= (other.bits & CPos::COMP_BITS)
    }
}

impl Ord for CPos {
    fn cmp(&self, other: &CPos) -> Ordering {
        (self.bits & CPos::COMP_BITS).cmp(&(other.bits & CPos::COMP_BITS))
    }
}

impl Hash for CPos {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.bits & CPos::COMP_BITS).hash(state);
    }
}

pub trait Mirrorable
where
    Self: Sized,
{
    fn mirror_h(&self) -> Self;
    fn mirror_v(&self) -> Self;
}

impl Mirrorable for Field {
    /// Mirror the field on horizontal middle axis.
    /// Changes to corresponding rank on other side, e.g. c6 <-> c3
    fn mirror_h(&self) -> Field {
        Field::fromFR(self.file(), 9 - self.rank())
    }
    /// Mirror the field on the vertical middle axis.
    /// Changes to corresponding file on the other side, e.g. c3 <-> f3
    fn mirror_v(&self) -> Field {
        Field::fromFR((b'h' as u8 - self.file() as u8 + b'a' as u8) as char, self.rank())
    }
}

impl Mirrorable for CPos {
    /// NOTE! After calling this, the fields must be ordered by ascending field numbers,
    /// (see `CPos::ordered`) **if** the position
    /// takes part in comparisions (including hashing!).
    /// ```
    /// use rasch::cpos::CPos;
    /// use rasch::cpos::Mirrorable;
    /// let cpos = CPos { bits: 0x0001_2345_6789_ABCDu64 };
    /// assert_eq!(cpos.ordered(), cpos.mirror_h().ordered().mirror_h().ordered());
    /// ```
    fn mirror_h(&self) -> CPos {
        let mut bits = self.bits;
        // for efficiency, we do this as an unrolled loop
        if bits & CPos::CODE1_BITS != 0 {
            let m = CPos::FIELD1_BITS;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_h();
            bits = (bits & !m) | ((f as u64) << s);
        }
        if bits & CPos::CODE2_BITS != 0 {
            let m = CPos::FIELD2_BITS;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_h();
            bits = (bits & !m) | ((f as u64) << s);
        }
        if bits & CPos::CODE3_BITS != 0 {
            let m = CPos::FIELD3_BITS;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_h();
            bits = (bits & !m) | ((f as u64) << s);
        }
        if bits & CPos::CODE4_BITS != 0 {
            let m = CPos::FIELD4_BITS;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_h();
            bits = (bits & !m) | ((f as u64) << s);
        }
        // the kings must be done unconditionally as they are always valid
        {
            let m = CPos::BLACK_KING_BITS;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_h();
            bits = (bits & !m) | ((f as u64) << s);
        }
        {
            let m = CPos::WHITE_KING_BITS;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_h();
            bits = (bits & !m) | ((f as u64) << s);
        }
        CPos { bits: bits ^ CPos::H_BIT }
    }

    /// NOTE! After calling this, the fields must be ordered by ascending field numbers,
    /// where the leftmost field (field4) has the lowest field number **if** the position
    /// takes part in comparisions (including hashing!).
    /// ```
    /// use rasch::cpos::CPos;
    /// use rasch::cpos::Mirrorable;
    /// let cpos = CPos { bits: 0x0001_2345_6789_ABCDu64 };
    /// assert_eq!(cpos.ordered(), cpos.mirror_v().ordered().mirror_v().ordered());
    /// ```
    fn mirror_v(&self) -> CPos {
        let mut bits = self.bits;
        // for efficiency, we do this as an unrolled loop
        if bits & CPos::CODE1_BITS != 0 {
            let m = CPos::FIELD1_BITS;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_v();
            bits = (bits & !m) | ((f as u64) << s);
        }
        if bits & CPos::CODE2_BITS != 0 {
            let m = CPos::FIELD2_BITS;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_v();
            bits = (bits & !m) | ((f as u64) << s);
        }
        if bits & CPos::CODE3_BITS != 0 {
            let m = CPos::FIELD3_BITS;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_v();
            bits = (bits & !m) | ((f as u64) << s);
        }
        if bits & CPos::CODE4_BITS != 0 {
            let m = CPos::FIELD4_BITS;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_v();
            bits = (bits & !m) | ((f as u64) << s);
        }
        // the kings must be done unconditionally as they are always present
        {
            let m = CPos::BLACK_KING_BITS;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_v();
            bits = (bits & !m) | ((f as u64) << s);
        }
        {
            let m = CPos::WHITE_KING_BITS;
            let s = m.trailing_zeros();
            let f = Field::from(((bits & m) >> s) as u8).mirror_v();
            bits = (bits & !m) | ((f as u64) << s);
        }
        CPos { bits: bits ^ CPos::V_BIT }
    }
}

impl Mirrorable for Move {
    fn mirror_h(&self) -> Self {
        let from = self.from().mirror_h();
        let to = self.to().mirror_h();
        Move::new(self.player(), self.piece(), self.promote(), from, to)
    }
    fn mirror_v(&self) -> Self {
        let from = self.from().mirror_v();
        let to = self.to().mirror_v();
        Move::new(self.player(), self.piece(), self.promote(), from, to)
    }
}
