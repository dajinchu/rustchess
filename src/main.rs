use std::mem::size_of;

use crate::board::Board;

mod board;
mod piece;

fn main() {
    let b = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string()).unwrap();
    println!("{:?}", b);
    println!("{}", b);
}
