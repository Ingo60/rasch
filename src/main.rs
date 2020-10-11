#![allow(non_snake_case)]

use rasch::computing as C;
use rasch::fieldset::*;
use rasch::mdb;
use rasch::position as P;
use rasch::zobrist as Z;

fn main() {
    mdb::initStatic();
    println!(
        "Can a white pawn go from E2 to E4? {}",
        mdb::canWhitePawn(Field::E2, Field::E4)
    );
    println!(
        "Can a white pawn go from E2 to E3? {}",
        mdb::canWhitePawn(Field::E2, Field::E3)
    );
    println!(
        "Can a white pawn go from E2 to D3? {}",
        mdb::canWhitePawn(Field::E2, Field::D3)
    );
    println!(
        "Can a white pawn go from E2 to G7? {}",
        mdb::canWhitePawn(Field::E2, Field::G7)
    );
    println!(
        "Here is where a white pawn can go from F2: {}",
        mdb::whitePawnTargets(Field::F2)
    );
    for f in (Field::E3 as u8)..=(Field::H5 as u8) {
        print!("{}", Field::from(f));
    }
    println!();
    /*
    for from in (!BitSet::empty()).into_iter() {
        for to in (!BitSet::empty()).into_iter() {
            let blackPawn = mdb::blackPawnTargets(from).member(to);
            let whitePawn = mdb::whitePawnTargets(from).member(to);
            let knight = mdb::knightTargets(from).member(to);
            let bishop = mdb::bishopTargets(from).member(to);
            let rook = mdb::rookTargets(from).member(to);
            let queen = rook || bishop;
            let king = mdb::kingTargets(from).member(to);
            if blackPawn || whitePawn || king || bishop || knight || rook {
                print!("From {} to {}:", from, to);
                if blackPawn {
                    print!(" black pawn");
                }
                if whitePawn {
                    print!(" white pawn");
                }
                if knight {
                    print!("  knight");
                }
                if bishop {
                    print!("  bishop");
                }
                if rook {
                    print!("  rook");
                }
                if queen {
                    print!("  queen");
                }
                if king {
                    print!("  king");
                }
                println!("");
            }
        }
    }
    */
    C::beginThinking();
    println!("stop thinking is {}", C::thinkingFinished());
    C::finishThinking();
    println!("stop thinking is now {}", C::thinkingFinished());
    // println!("imposssible {}", BitSet::empty().bitIndex());
    println!("ppfZobrist(1,6,0) {:x}", Z::ppfZobrist(1, 6, 0));
    let board = P::initialBoard();
    println!("hash {:x}", board.hash);
    println!("flags {}", board.flags);
    println!("whites {}", board.whites);
    println!("pawnSet {}", board.pawnSet);
    println!("bishopSet {}", board.bishopSet);
    println!("rookSet {}", board.rookSet);
    print!("moves: ");
    for mv in board.moves() {
        print!("{} ", mv);
    }
    println!();
}
