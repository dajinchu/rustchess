#![feature(const_trait_impl)]
use std::mem::size_of;

use crate::board::Board;

mod board;
mod piece;
mod vec2d;
mod util;
mod movement;

fn main() {
    println!("{}", size_of::<Board>());
    let b = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string()).unwrap();
    b.valid_moves();
    println!("{:?}", b);
    println!("{}", b);
}
