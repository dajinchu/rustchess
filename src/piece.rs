use std::slice::Iter;
pub type Piece = (Color, PieceType);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Color {
    Black,
    White,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PieceType {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

use self::Color::*;
use self::PieceType::*;

pub fn fen_to_piece(fen: char) -> Option<Piece> {
    match fen {
        'p' => Some((Black, Pawn)),
        'r' => Some((Black, Rook)),
        'n' => Some((Black, Knight)),
        'b' => Some((Black, Bishop)),
        'q' => Some((Black, Queen)),
        'k' => Some((Black, King)),
        'P' => Some((White, Pawn)),
        'R' => Some((White, Rook)),
        'N' => Some((White, Knight)),
        'B' => Some((White, Bishop)),
        'Q' => Some((White, Queen)),
        'K' => Some((White, King)),
        _ => None,
    }
}

impl Color {
    pub fn iterator() -> Iter<'static, Color> {
        static COLORS: [Color; 2] = [White, Black];
        COLORS.iter()
    }
    pub fn other(&self) -> Color {
        match self {
            Black => White,
            White => Black
        }
    }
}
impl PieceType {
    pub fn iterator() -> Iter<'static, PieceType> {
        static PIECE_TYPES: [PieceType; 6] = [Pawn, Rook, Knight, Bishop, Queen, King];
        PIECE_TYPES.iter()
    }
}

pub fn piece_to_ascii(p: Piece) -> char {
    match p {
        (White, Pawn) => '♙',
        (White, Rook) => '♖',
        (White, Knight) => '♘',
        (White, Bishop) => '♗',
        (White, Queen) => '♕',
        (White, King) => '♔',
        (Black, Pawn) => '♟',
        (Black, Rook) => '♜',
        (Black, Knight) => '♞',
        (Black, Bishop) => '♝',
        (Black, Queen) => '♛',
        (Black, King) => '♚',
    }
}

pub fn ascii_to_piece(c: char) -> Option<Piece> {
    Some(match c {
        '♙' => (White, Pawn),
        '♖' => (White, Rook),
        '♘' => (White, Knight),
        '♗' => (White, Bishop),
        '♕' => (White, Queen),
        '♔' => (White, King),
        '♟' => (Black, Pawn),
        '♜' => (Black, Rook),
        '♞' => (Black, Knight),
        '♝' => (Black, Bishop),
        '♛' => (Black, Queen),
        '♚' => (Black, King),
        _ => return None,
    })
}
