use rasch::fieldset::*;
use rasch::mdb;
use rasch::zobrist;

fn main() {
    println!("ppfZobrist(1,6,0) {:#}", zobrist::ppfZobrist(1, 6, 0));
    println!("value {:x}  !value {:x}", 0xaau8, !0xaau8);
    unsafe { mdb::genPawn() };
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
    println!("Here is where a white pawn can go from F2: {}", "notyet");
}
