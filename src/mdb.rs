#![allow(non_snake_case)] // sorry, need this because this is a rewrite of existing Java code
#![allow(non_upper_case_globals)] // as well as this

/*!
# Move Data Base

Functions for setting up tables that can later be used to find moves fast.

There is no table for Queen moves, we simply take the sum of Bishop and Rook moves.

The functions here don't take into account the general rule that the target field must not
have a figure of the same color. For example, a Bishop on c1 will always be able to
"move" to b2 and d2. This allows generation of possible moves as well as detection of coverage.

For Bishop/Rook, we return a bitmap of the fields that need to be free for the move to be
valid. Hence, 0 means the move is possible, and -1 means it is not possible (unless the board was completly empty).

*/

use super::basic::{Piece, Piece::*, Player, Player::*};
use super::cpos::Mirrorable;
use super::fieldset::*;
use serde::{Deserialize, Serialize};

/// Hilfsmodul für die Serialisierung von 64-Element-Arrays
mod big_array {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    pub fn serialize<S>(array: &[f64; 64], serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        array.as_slice().serialize(serializer)
    }
    pub fn deserialize<'de, D>(deserializer: D) -> Result<[f64; 64], D::Error>
    where
        D: Deserializer<'de>,
    {
        let v: Vec<f64> = Vec::deserialize(deserializer)?;
        let len = v.len();
        v.try_into().map_err(|_| {
            serde::de::Error::custom(format!("Erwartete PST-Tabelle mit 64 Elementen, fand {}", len))
        })
    }
}

/// Hilfsmodul für die Serialisierung von 49-Element-Arrays (Piece x Piece Kombinationen)
mod array49 {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    pub fn serialize<S>(array: &[f64; 49], serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        array.as_slice().serialize(serializer)
    }
    pub fn deserialize<'de, D>(deserializer: D) -> Result<[f64; 49], D::Error>
    where
        D: Deserializer<'de>,
    {
        let v: Vec<f64> = Vec::deserialize(deserializer)?;
        let len = v.len();
        v.try_into().map_err(|_| {
            serde::de::Error::custom(format!("Erwartete Piece-Tabelle mit 49 Elementen, fand {}", len))
        })
    }
}

/// Gewichte für die Evaluation
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct EvalWeights {
    pub mat_pawn: f64,
    pub mat_knight: f64,
    pub mat_bishop: f64,
    pub mat_rook: f64,
    pub mat_queen: f64,
    pub mat_king: f64,
    pub mobility: f64,
    pub check: f64,
    pub castling: f64,
    pub covered_king_opp: f64,
    pub covered_king_own: f64,
    pub blocked_bishop_pawn: f64,
    pub bad_bishop: f64,
    pub lazy_officer: f64,
    #[serde(with = "big_array")]
    pub pst_pawn: [f64; 64],
    #[serde(with = "big_array")]
    pub pst_knight: [f64; 64],
    #[serde(with = "big_array")]
    pub pst_bishop: [f64; 64],
    #[serde(with = "big_array")]
    pub pst_rook: [f64; 64],
    #[serde(with = "big_array")]
    pub pst_queen: [f64; 64],
    #[serde(with = "big_array")]
    pub pst_king_mg: [f64; 64],
    #[serde(with = "big_array")]
    pub pst_king_eg: [f64; 64],
    /// Gewichte für Schläge (Angreifer x Opfer). Index: attacker * 7 + victim.
    #[serde(with = "array49")]
    pub attack_weights: [f64; 49],
    /// Gewichte für Verteidigung (Verteidiger x Opfer). Index: defender * 7 + victim.
    #[serde(with = "array49")]
    pub defend_weights: [f64; 49],
}

impl EvalWeights {
    /// Serialisiert die Gewichte nach JSON
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Deserialisiert die Gewichte aus JSON
    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }

    pub fn piece_score(&self, p: Piece) -> f64 {
        match p {
            EMPTY => 0.0,
            PAWN => self.mat_pawn,
            KNIGHT => self.mat_knight,
            BISHOP => self.mat_bishop,
            ROOK => self.mat_rook,
            QUEEN => self.mat_queen,
            KING => self.mat_king,
        }
    }

    pub fn attack_weight(&self, attacker: Piece, victim: Piece) -> f64 {
        self.attack_weights[attacker as usize * 7 + victim as usize]
    }

    pub fn defend_weight(&self, defender: Piece, victim: Piece) -> f64 {
        self.defend_weights[defender as usize * 7 + victim as usize]
    }

    /// Liefert den PST-Wert für eine Figur auf einem bestimmten Feld.
    /// Für Schwarz wird das Feld gespiegelt, um die Tabelle symmetrisch zu nutzen.
    pub fn pst_value(&self, piece: Piece, player: Player, field: Field) -> f64 {
        let idx = if player == WHITE { field as usize } else { field.mirror_h() as usize };
        match piece {
            PAWN => self.pst_pawn[idx],
            KNIGHT => self.pst_knight[idx],
            BISHOP => self.pst_bishop[idx],
            ROOK => self.pst_rook[idx],
            QUEEN => self.pst_queen[idx],
            KING => self.pst_king_mg[idx],
            EMPTY => 0.0,
        }
    }

    /// Berechnet den PST-Wert für den König durch Interpolation zwischen Mittel- und Endspiel.
    /// mg_weight = 1.0 (reines Mittelspiel), mg_weight = 0.0 (reines Endspiel).
    pub fn king_pst_value(&self, player: Player, field: Field, mg_weight: f64) -> f64 {
        let idx = if player == WHITE { field as usize } else { field.mirror_h() as usize };
        let mg_val = self.pst_king_mg[idx];
        let eg_val = self.pst_king_eg[idx];
        mg_weight * mg_val + (1.0 - mg_weight) * eg_val
    }
}

impl Default for EvalWeights {
    fn default() -> Self {
        let mut w = EvalWeights {
            mat_pawn: 100.0,
            mat_knight: 300.0,
            mat_bishop: 305.0,
            mat_rook: 550.0,
            mat_queen: 875.0,
            mat_king: 1000.0,
            mobility: 4.0,
            check: 20.0,
            castling: 25.0,
            covered_king_opp: 5.0,
            covered_king_own: 6.0,
            blocked_bishop_pawn: 21.0,
            bad_bishop: 43.0,
            lazy_officer: 30.0,
            pst_pawn: [0.0; 64],
            pst_knight: [0.0; 64],
            pst_bishop: [0.0; 64],
            pst_rook: [0.0; 64],
            pst_queen: [0.0; 64],
            pst_king_mg: [0.0; 64],
            pst_king_eg: [0.0; 64],
            attack_weights: [0.0; 49],
            defend_weights: [0.0; 49],
        };

        // Initialisierung mit dem bisherigen "Zone"-Bonus als Standardwert
        for f in ALLFIELDS {
            let val = (f.zone() as i32 + 1) as f64 * 5.0;
            let idx = f as usize;
            w.pst_pawn[idx] = val;
            w.pst_knight[idx] = val;
            w.pst_bishop[idx] = val;
            w.pst_rook[idx] = val;
            w.pst_queen[idx] = val;
            w.pst_king_mg[idx] = val;
            w.pst_king_eg[idx] = val;
        }

        // Initialisierung der Angriffs- und Verteidigungsgewichte (Opfer / Angreifer)
        for attacker in 0..7 {
            for victim in 0..7 {
                let a_piece = Piece::from(attacker as u32);
                let v_piece = Piece::from(victim as u32);
                let score_a = w.piece_score(a_piece);
                let score_v = w.piece_score(v_piece);

                // Basisgewicht ist das Verhältnis von Opferwert zu Angreiferwert.
                // Ein Bauer, der eine Dame angreift, hat ein hohes Gewicht (875/100 = 8.75).
                let weight = if a_piece == EMPTY {
                    0.0
                } else if v_piece == EMPTY {
                    5.0 // Kleiner Bonus (5 Centipawns) für die Kontrolle leerer Felder
                } else {
                    // Verhältniswert: Je wertvoller das Opfer relativ zum Angreifer, desto höher das Gewicht.
                    score_v / score_a
                };

                let idx = attacker * 7 + victim;
                w.attack_weights[idx] = weight;
                w.defend_weights[idx] = weight;
            }
        }

        w
    }
}

pub static mut current_weights: EvalWeights = EvalWeights {
    mat_bishop: 305.0,
    mat_king: 1000.0,
    mat_knight: 300.0,
    mat_pawn: 100.0,
    mat_queen: 875.0,
    mat_rook: 550.0,
    mobility: 4.0,
    check: 20.0,
    castling: 25.0,
    covered_king_opp: 5.0,
    covered_king_own: 6.0,
    blocked_bishop_pawn: 21.0,
    bad_bishop: 43.0,
    lazy_officer: 30.0,
    pst_bishop: [0.0; 64],
    pst_king_mg: [0.0; 64],
    pst_king_eg: [0.0; 64],
    pst_knight: [0.0; 64],
    pst_pawn: [0.0; 64],
    pst_queen: [0.0; 64],
    pst_rook: [0.0; 64],
    attack_weights: [0.0; 49],
    defend_weights: [0.0; 49],
};

pub fn set_default_weights() {
    unsafe {
        current_weights = EvalWeights::default();
    }
}

pub fn get_mat_pawn() -> f64 {
    unsafe { current_weights.mat_pawn }
}
pub fn get_mat_knight() -> f64 {
    unsafe { current_weights.mat_knight }
}
pub fn get_mat_bishop() -> f64 {
    unsafe { current_weights.mat_bishop }
}
pub fn get_mat_rook() -> f64 {
    unsafe { current_weights.mat_rook }
}
pub fn get_mat_queen() -> f64 {
    unsafe { current_weights.mat_queen }
}
pub fn get_mat_king() -> f64 {
    unsafe { current_weights.mat_king }
}
pub fn get_piece_score(p: Piece) -> f64 {
    unsafe { current_weights.piece_score(p) }
}
pub fn get_mobility() -> f64 {
    unsafe { current_weights.mobility }
}
pub fn get_check() -> f64 {
    unsafe { current_weights.check }
}
pub fn get_castling() -> f64 {
    unsafe { current_weights.castling }
}
pub fn get_covered_king_opp() -> f64 {
    unsafe { current_weights.covered_king_opp }
}
pub fn get_covered_king_own() -> f64 {
    unsafe { current_weights.covered_king_own }
}
pub fn get_blocked_bishop_pawn() -> f64 {
    unsafe { current_weights.blocked_bishop_pawn }
}
pub fn get_bad_bishop() -> f64 {
    unsafe { current_weights.bad_bishop }
}
pub fn get_lazy_officer() -> f64 {
    unsafe { current_weights.lazy_officer }
}

pub fn get_pst_value(piece: Piece, player: Player, field: Field) -> f64 {
    unsafe { current_weights.pst_value(piece, player, field) }
}

/// Liefert den interpolierten PST-Wert für den König basierend auf der Endspielwahrscheinlichkeit.
pub fn get_pst_value_king(player: Player, field: Field, mg_weight: f64) -> f64 {
    unsafe { current_weights.king_pst_value(player, field, mg_weight) }
}

/// Liefert das Gewicht für einen Angriff (ungleichfarbig) basierend auf Angreifer und Opfer.
pub fn get_attack_weight(attacker: Piece, victim: Piece) -> f64 {
    unsafe { current_weights.attack_weight(attacker, victim) }
}

/// Liefert das Gewicht für eine Verteidigungsbeziehung (gleichfarbig) zwischen zwei Figuren.
pub fn get_defend_weight(defender: Piece, victim: Piece) -> f64 {
    unsafe { current_weights.defend_weight(defender, victim) }
}

/// Gibt eine Kopie des gesamten aktuellen Gewichtssatzes zurück.
pub fn get_weights() -> EvalWeights {
    unsafe { current_weights }
}

pub fn set_weights(weights: EvalWeights) {
    unsafe {
        current_weights = weights;
    }
}

const WEIGHTS_FILE: &str = "./data/weights.json";

/// deserialisiert aus ./data/weights.json, falls vorhanden, ansonsten werden die Default-Gewichte verwendet
pub fn load_weights() {
    match std::fs::read_to_string(WEIGHTS_FILE) {
        Ok(json) => match EvalWeights::from_json(&json) {
            Ok(weights) => {
                set_weights(weights);
                println!("# Gewichte aus {} geladen.", WEIGHTS_FILE);
            }
            Err(e) => {
                println!(
                    "# Fehler beim Parsen von {}: {}. Standardgewichte werden verwendet.",
                    WEIGHTS_FILE, e
                );
                set_default_weights();
            }
        },
        Err(_) => {
            println!(
                "# {} nicht gefunden. Standardgewichte werden verwendet.",
                WEIGHTS_FILE
            );
            set_default_weights();
        }
    }
}

/// serialisiert die aktuellen Gewichte in ./data/weights.json
pub fn save_weights() {
    let weights = get_weights();
    match weights.to_json() {
        Ok(json) => match std::fs::write(WEIGHTS_FILE, json) {
            Ok(_) => println!("# Gewichte in {} gespeichert.", WEIGHTS_FILE),
            Err(e) => println!(
                "# Fehler beim Schreiben von {}: {}. Gewichte konnten nicht gespeichert werden.",
                WEIGHTS_FILE, e
            ),
        },
        Err(e) => println!(
            "# Fehler beim Serialisieren der Gewichte: {}. Gewichte konnten nicht gespeichert werden.",
            e
        ),
    };
}

//
//                  Geometry
//      8        7        6       5         4        3       2        1
//  hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba hgfedcba
//  10101010 01010101 11001100 00110011 01100110 11001100 00110011 01100110
//
// A bit in a bitset corresponds to the index (0..63) that is equal to the number of
// lower significance bits.
// Example: e2 = 1000000000000(2) = 0x1000L = index 12
//          index 12 -> 1L << 12  = 0x1000L

///     the eight row
const OBEN: u64 = 0xff00000000000000u64;

///     the H file
const RECHTS: u64 = 0x8080808080808080u64;

///     the A file
const LINKS: u64 = 0x0101010101010101u64;

///     the first row
const UNTEN: u64 = 0x00000000000000ffu64;

///     when added to an index that is not on the H file, yields the index to the right, i.e. e5 -> f5
const EAST: i32 = 1;

///     when added to an index that is not on the A file, yields the index to the left, i.e. e5 -> d5
const WEST: i32 = -1;

///     when added to an index that is not on the first row, yields the index down one row, i.e e5 -> e4
const SOUTH: i32 = -8;

///     when added to an index that is not on the last row, yields the index up one row, i.e e5 -> e6
const NORTH: i32 = 8;

///     e5 -> f6
const NE: i32 = NORTH + EAST;

///     e5 -> f4
const SE: i32 = SOUTH + EAST;

///     e5 ->  d4
const SW: i32 = SOUTH + WEST;

///     e5 ->  d6
const NW: i32 = NORTH + WEST;

/// indicate a move that is not affected by other figures
const LEGAL: u64 = 0;

/// indicate an illegal move
const ILLEGAL: u64 = !0;

/**
 Moves for Bishops

 This table, as well as all other ...FromTo tables,
 is indexed with `(from<<6)+to`, where from and to are field numbers 0..63

 Each entry is a bit set that indicates which fields must be empty for making the move legal.
 Thus 0 means the move is legal, not considering occupation of the target field,
 whereas when all bits are set it indicates it is not legal
 since it cannot be the case that all fields are empty.

 Note that we don't have tables for queens, knights and kings.
 This is because queen moves are like either rook or bishop moves,
 Hence the queenFromTo values would simply be the union of rookFromTo and bishopFromTo values.

 Moves of knights and kings, on the other hand, can never be affected by the placement of
 other pieces, hence all values would be LEGAL or ILLEGAL.

*/
static mut bishopFromTo: [u64; 64 * 64] = [ILLEGAL; 64 * 64];
static mut rookFromTo: [u64; 64 * 64] = [ILLEGAL; 64 * 64];
static mut whitePawnFromTo: [u64; 64 * 64] = [ILLEGAL; 64 * 64];
static mut blackPawnFromTo: [u64; 64 * 64] = [ILLEGAL; 64 * 64];

/**
 Set of fields that can in principle be reached directly
 by a certain piece from a certain field.

 This is indexed by a field number 0..63

 The value can be used to find relevant entries in the ...FromTo tables, if applicable,
 and finally check the free fields in the current position.
*/
static mut bishopTo: [u64; 64] = [0; 64];
static mut rookTo: [u64; 64] = [0; 64];
static mut knightTo: [u64; 64] = [0; 64];
static mut kingTo: [u64; 64] = [0; 64];
static mut whitePawnTo: [u64; 64] = [0; 64];
static mut blackPawnTo: [u64; 64] = [0; 64];

/**
The inverse of whitePawnTo/blackPawnTo: if there is a black/white pawn on any of the
indicated fields, then the given field is attacked.
We need this only for pawns, since the moves of all other pieces are symmetric, that is,
for example, the set of fields an index can be attacked from with a bishop is the same as
the set of fields a bishop on index can go to.
*/
static mut whitePawnFrom: [u64; 64] = [0; 64];
static mut blackPawnFrom: [u64; 64] = [0; 64];

/**
Like `whitePawnFrom`, but lists also the fields a PAWN could come from without capturing.
For example: `whitePawnComeTo[E4] = {E2,E3,D3,F3}`
 */
static mut whitePawnComeTo: [u64; 64] = [0; 64];
static mut blackPawnComeTo: [u64; 64] = [0; 64];

/// Gives the ordinal number - 1 for a canonical king configuration
/// in a game with pawns.
/// For example, WK a1 BK c1 is the first hence it gets 0
/// while WK d8 BK h8 is the last (of 1806) hence 1805
static mut kingPawnConf: [u16; 64 * 64] = [0xffff; 64 * 64];

/// Gives the ordinal number - 1 for a canonical king configuration
/// in a game without pawns.
/// For example, WK a1 BK c1 is the first hence it gets 0
/// while WK d4 BK h8 is the last (of 564) hence 563
static mut kingWithoutPawnConf: [u16; 64 * 64] = [0xffff; 64 * 64];
/**
     tell if we can go into a certain direction from some field
*/
pub fn canGo(from: u64, direction: i32) -> bool {
    match direction {
        EAST => (from & RECHTS) == 0,
        WEST => (from & LINKS) == 0,
        NORTH => (from & OBEN) == 0,
        SOUTH => (from & UNTEN) == 0,
        NE => (from & (OBEN | RECHTS)) == 0,
        SE => (from & (UNTEN | RECHTS)) == 0,
        NW => (from & (OBEN | LINKS)) == 0,
        SW => (from & (UNTEN | LINKS)) == 0,
        _ => false,
    }
}

/// Go from some field in a certain direction
/// * fld - a singleton set that denotes the field in question
/// * direction - one of NORTH, EAST, SOUTH, WEST, NE, NW, SE, SW
/// Returns the field that is one step from the given field in a given direction,
///       if there is such a field, otherwise, nonsense results.
/// This should only be done when canGo returns true

pub fn goTowards(fld: u64, direction: i32) -> u64 {
    if direction < 0 {
        fld >> (-direction)
    } else {
        fld << direction
    }
}

/// tell if we cab go 2 steps in the indicated direction
pub fn canGo2(from: u64, direction: i32) -> bool {
    canGo(from, direction) && canGo(goTowards(from, direction), direction)
}

/// convert a singleton to an index
/// (actually, it looks only for the least significant 1 bit)
/// beware not to pass 0, because then the index becomes 64
pub fn setToIndex(singleton: u64) -> usize {
    singleton.trailing_zeros() as usize
}

/// Initialize the static mutable arrays.
/// **Must** run before anything else, preferably from `main()`
/// and it must only run **once**
pub fn initStatic() {
    unsafe {
        genPawn();
        genBishop();
        genRook();
        genKnight();
        genKing();
    }
    // lade die Gewichte aus der JSON-Datei, falls vorhanden
    load_weights();
}

unsafe fn genPawn() {
    let mut from = 0x100u64; // no pawns on base row
    while from < 0x0100_0000_0000_0000u64 {
        if canGo(from, NORTH) {
            let to = goTowards(from, NORTH);
            whitePawnTo[setToIndex(from)] |= to;
            whitePawnComeTo[setToIndex(to)] |= from;
            whitePawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = to;
            if (from & 0xff00u64) != 0 && canGo(to, NORTH) {
                // A2..H2 second rank
                let mask = to;
                let to = goTowards(to, NORTH);
                whitePawnTo[setToIndex(from)] |= to;
                whitePawnComeTo[setToIndex(to)] |= from;
                whitePawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = to | mask;
            }
        }
        if canGo(from, SOUTH) {
            let to = goTowards(from, SOUTH);
            blackPawnTo[setToIndex(from)] |= to;
            blackPawnComeTo[setToIndex(to)] |= from;
            blackPawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = to;
            if (from & 0x00ff000000000000u64) != 0 && canGo(to, SOUTH) {
                // A7..H7 seventh rank
                let mask = to;
                let to = goTowards(to, SOUTH);
                blackPawnTo[setToIndex(from)] |= to;
                blackPawnComeTo[setToIndex(to)] |= from;
                blackPawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = to | mask;
            }
        }
        if canGo(from, NE) {
            let to = goTowards(from, NE);
            whitePawnTo[setToIndex(from)] |= to;
            whitePawnFrom[setToIndex(to)] |= from;
            whitePawnComeTo[setToIndex(to)] |= from;
            whitePawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = LEGAL;
        }
        if canGo(from, NW) {
            let to = goTowards(from, NW);
            whitePawnTo[setToIndex(from)] |= to;
            whitePawnFrom[setToIndex(to)] |= from;
            whitePawnComeTo[setToIndex(to)] |= from;
            whitePawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = LEGAL;
        }
        if canGo(from, SE) {
            let to = goTowards(from, SE);
            blackPawnTo[setToIndex(from)] |= to;
            blackPawnFrom[setToIndex(to)] |= from;
            blackPawnComeTo[setToIndex(to)] |= from;
            blackPawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = LEGAL;
        }
        if canGo(from, SW) {
            let to = goTowards(from, SW);
            blackPawnTo[setToIndex(from)] |= to;
            blackPawnFrom[setToIndex(to)] |= from;
            blackPawnComeTo[setToIndex(to)] |= from;
            blackPawnFromTo[(setToIndex(from) << 6) + setToIndex(to)] = LEGAL;
        }
        from <<= 1;
    }
}

unsafe fn genBishop() {
    let directions = [NE, SE, SW, NW];

    let mut from = 1u64;

    while from != 0 {
        // for all fields
        for dp in directions.iter() {
            // for all directions
            let mut to = from;
            let mut mask = LEGAL;
            let d = *dp;
            while canGo(to, d) {
                to = goTowards(to, d);
                bishopTo[setToIndex(from)] |= to; // can go there
                bishopFromTo[(setToIndex(from) << 6) + setToIndex(to)] = mask;
                mask |= to; // accumulate all fields traversed so far
            }
        }
        from <<= 1;
    }
}

unsafe fn genRook() {
    let directions = [NORTH, SOUTH, WEST, EAST];

    let mut from = 1u64;

    while from != 0 {
        // for all fields
        for dp in directions.iter() {
            // for all directions
            let mut to = from;
            let mut mask = LEGAL;
            let d = *dp;
            while canGo(to, d) {
                to = goTowards(to, d);
                rookTo[setToIndex(from)] |= to; // can go there
                rookFromTo[(setToIndex(from) << 6) + setToIndex(to)] = mask;
                mask |= to; // accumulate all fields traversed so far
            }
        }
        from <<= 1;
    }
}

unsafe fn genKnight() {
    let directions = [NORTH, SOUTH, EAST, WEST];
    let schraeg1 = [NE, SE, NE, NW];
    let schraeg2 = [NW, SW, SE, SW];
    let mut from = 1u64;

    while from != 0 {
        for i in 0..4 {
            let d1 = directions[i];
            if canGo(from, d1) {
                let to1 = goTowards(from, d1);
                let d2 = schraeg1[i];
                let d3 = schraeg2[i];
                if canGo(to1, d2) {
                    let to2 = goTowards(to1, d2);
                    knightTo[setToIndex(from)] |= to2;
                }
                if canGo(to1, d3) {
                    let to2 = goTowards(to1, d3);
                    knightTo[setToIndex(from)] |= to2;
                }
            }
        }
        from <<= 1;
    }
}

unsafe fn genKing() {
    let directions = [NORTH, NE, EAST, SE, SOUTH, SW, WEST, NW];
    let mut from = 1u64;
    while from != 0 {
        for dp in directions.iter() {
            let d = *dp;
            if canGo(from, d) {
                let to = goTowards(from, d);
                kingTo[setToIndex(from)] |= to;
            }
        }
        from <<= 1;
    }
    let mut goodconfs = 0;
    for wk in 0..64 {
        for bk in 0..64 {
            if wk != bk && wk % 8 < 4 && kingTo[wk] & (1 << bk) == 0 {
                kingPawnConf[(wk << 6) + bk] = goodconfs;
                goodconfs += 1;
            } else {
                kingPawnConf[(wk << 6) + bk] = 0xffff;
            }
        }
    }
    assert_eq!(goodconfs, 1806);
    goodconfs = 0;
    for wk in 0..64 {
        let fwk = Field::from(wk as u64);
        let ondiag = A1H8_DIAGONAL.member(fwk);
        let intriangle = A1D1D4_TRIANGLE.member(fwk);
        for bk in 0..64 {
            let fbk = Field::from(bk as u64);
            if intriangle
                && wk != bk
                && kingTo[wk] & (1 << bk) == 0
                && (!ondiag || A1H1H8_TRIANGLE.member(fbk))
            {
                kingWithoutPawnConf[(wk << 6) + bk] = goodconfs;
                goodconfs += 1;
            } else {
                kingWithoutPawnConf[(wk << 6) + bk] = 0xffff;
            }
        }
    }
    assert_eq!(goodconfs, 462);
}

/**
 * Given a `from` and a `to` field, compute the set of fields that must be empty on the board
 * for the move to be possible with a white pawn.
 *
 * Note that legal capturing moves will always return the `BitSet::empty()` set, indicating the
 * move can be legal, for example when the target field is occupied by the opponent.
 *
 * Forward moves, on the other hand, will return a set with the target field as member, as they
 * are non attacking moves, that is, the move is impossible if the target field is occupied.
 */
pub fn canWhitePawn(from: Field, to: Field) -> BitSet {
    unsafe { BitSet::from(whitePawnFromTo[((from as usize) << 6) + (to as usize)]) }
}

/**
 * Given a `from` and a `to` field, compute the set of fields that must be empty on the board
 * for the move to be possible with a black pawn.
 *
 * Note that legal capturing moves will always return the `BitSet::empty()` set, indicating the
 * move can be legal, for example when the target field is occupied by the opponent.
 *
 * Forward moves, on the other hand, will return a set with the target field as member, as they
 * are non attacking moves, that is, the move is impossible if the target field is occupied.
 */
pub fn canBlackPawn(from: Field, to: Field) -> BitSet {
    unsafe { BitSet::from(blackPawnFromTo[((from as usize) << 6) + (to as usize)]) }
}

/**
Given a `from` and a `to` field, compute the set of fields that must be empty on the board
for the move to be possible with a bishop.

Note that for legal moves, the target field will not be a member of the result set.
This is because capturing moves are legal when the target field is occupied by the opponent.

A result of `BitSet::empty()` indicates there are no intermediate fields that must be empty
and the move is legal.

An illegal move will be indicated with a return value `!BitSet::empty()`, requiring all fields
of the board to be empty to be leagl. Of course, in all regular chess matches, this condition
can never be satisfied.
*/
pub fn canBishop(from: Field, to: Field) -> BitSet {
    unsafe { BitSet::from(bishopFromTo[((from as usize) << 6) + (to as usize)]) }
}

/**
  Given a `from` and a `to` field, compute the set of fields that must be empty on the board
  for the move to be possible with a rook.

  Note that for legal moves, the target field will not be a member of the result set.
  This is because capturing moves are legal when the target field is occupied by the opponent.

  A result of `BitSet::empty()` indicates there are no intermediate fields that must be empty
  and the move is legal.

  An illegal move will be indicated with a return value `!BitSet::empty()`, requiring all fields
  of the board to be empty to be leagl. Of course, in all regular chess matches, this condition
  can never be satisfied.
*/
pub fn canRook(from: Field, to: Field) -> BitSet {
    unsafe { BitSet::from(rookFromTo[((from as usize) << 6) + (to as usize)]) }
}

/// The set of fields that can be reached with a white pawn standing on `from`.
pub fn whitePawnTargets(from: Field) -> BitSet {
    unsafe { BitSet::from(whitePawnTo[from as usize]) }
}

/// The set of fields from whence a white pawn could come to field `to`.
pub fn whitePawnSources(to: Field) -> BitSet {
    unsafe { BitSet::from(whitePawnComeTo[to as usize]) }
}

/// The set of fields that can be reached with a black pawn standing on `from`.
pub fn blackPawnTargets(from: Field) -> BitSet {
    unsafe { BitSet::from(blackPawnTo[from as usize]) }
}

/// The set of fields from whence a black pawn could come to field `to`.
pub fn blackPawnSources(to: Field) -> BitSet {
    unsafe { BitSet::from(blackPawnComeTo[to as usize]) }
}

/// The set of fields that can be reached with a bishop standing on `from`.
pub fn bishopTargets(from: Field) -> BitSet {
    unsafe { BitSet::from(bishopTo[from as usize]) }
}

/// The set of fields that can be reached with a rook standing on `from`.
pub fn rookTargets(from: Field) -> BitSet {
    unsafe { BitSet::from(rookTo[from as usize]) }
}

/// The set of fields that can be reached with a king standing on `from`.
pub fn kingTargets(from: Field) -> BitSet {
    unsafe { BitSet::from(kingTo[from as usize]) }
}

/// The set of fields that can be reached with a knight standing on `from`.
pub fn knightTargets(from: Field) -> BitSet {
    unsafe { BitSet::from(knightTo[from as usize]) }
}

/// The set of fields from where a white pawn can attack the field given in `to`.
pub fn targetOfWhitePawns(to: Field) -> BitSet {
    unsafe { BitSet::from(whitePawnFrom[to as usize]) }
}

/// The set of fields from where a black pawn can attack the field given in `to`.
pub fn targetOfBlackPawns(to: Field) -> BitSet {
    unsafe { BitSet::from(blackPawnFrom[to as usize]) }
}

/// The ordinal number assigned to king configuration (wk, bk) in a game with pawns
fn kingPawnConfig(wk: Field, bk: Field) -> usize {
    unsafe { kingPawnConf[((wk as usize) << 6) + (bk as usize)] as usize }
}

/// The ordinal number assigned to king configuration (wk, bk) in a game with pawns
fn kingWithoutPawnConfig(wk: Field, bk: Field) -> usize {
    unsafe { kingWithoutPawnConf[((wk as usize) << 6) + (bk as usize)] as usize }
}

/// The ordinal number assigned to king configuration (wk, bk) with (true) or without (false) pawns
pub fn kingConfig(wk: Field, bk: Field, pawns: bool) -> usize {
    if pawns {
        kingPawnConfig(wk, bk)
    } else {
        kingWithoutPawnConfig(wk, bk)
    }
}
