use crate::piece::Color::*;
use crate::piece::PieceType::*;
use crate::piece::*;

/*
Notes:
How to represent color? Separate bitboards for each color or a mask?
*/

#[derive(Debug)]
pub struct Board {
    white: BitBoardPieces,
    black: BitBoardPieces,
    active: Color,
    castle_bq: bool,
    castle_bk: bool,
    castle_wq: bool,
    castle_wk: bool,
    passant: u8,
    halfmoves: u8,
}
const NO_PASSANT: u8 = 65;

#[derive(Debug)]
struct BitBoardPieces {
    pawns: BitBoard,
    rooks: BitBoard,
    knights: BitBoard,
    bishops: BitBoard,
    queens: BitBoard,
    kings: BitBoard,
}

type BitBoard = u64;

pub fn to_mask(file: u8, rank: u8) -> u64 {
    1 << (rank * 8 + file)
}

pub fn algebraic_to_index(algebraic: String) -> u8 {
    let mut chars = algebraic.chars();
    println!("{}", algebraic);
    let file = chars.next().unwrap() as u8 - b'a';
    let rank = chars.next().unwrap().to_digit(10).unwrap() as u8;
    rank * 8 + file
}

impl Board {
    pub fn from_fen(fen: String) -> Result<Board, ()> {
        let v: Vec<&str> = fen.split(' ').collect();
        match v[..] {
            [pieces, active, castle, passant, halfmove, fullmove] => {
                let active_color = match active {
                    "w" => White,
                    "b" => Black,
                    _ => return Err(()),
                };
                let mut wk = false;
                let mut wq = false;
                let mut bk = false;
                let mut bq = false;
                for c in castle.chars() {
                    match c {
                        'K' => wk = true,
                        'Q' => wq = true,
                        'k' => bk = true,
                        'q' => bq = true,
                        _ => (),
                    }
                }
                let passant = match passant {
                    "-" => NO_PASSANT,
                    _ => algebraic_to_index(passant.to_string()),
                };
                let mut board = Board {
                    white: BitBoardPieces::blank(),
                    black: BitBoardPieces::blank(),
                    active: active_color,
                    castle_wk: wk,
                    castle_wq: wq,
                    castle_bk: bk,
                    castle_bq: bq,
                    passant,
                    halfmoves: halfmove.to_string().parse::<u8>().unwrap(),
                };
                for (i, s) in pieces.split('/').enumerate() {
                    let rank = 7-i;
                    let mut file: u8 = 0;
                    for c in s.chars() {
                        match c.to_digit(10) {
                            Some(num_empties) => file += num_empties as u8,
                            None => {
                                board.set_square(file, rank as u8, fen_to_piece(c).unwrap());
                                file += 1;
                            },
                        }
                    }
                }
                Ok(board)
            }
            _ => Err(()),
        }
    }

    fn get_pieces_mut(&mut self, color: Color) -> &mut BitBoardPieces {
        match color {
            Black => &mut self.black,
            White => &mut self.white,
        }
    }

    fn get_pieces(&self, color: Color) -> &BitBoardPieces {
        match color {
            Black => &self.black,
            White => &self.white,
        }
    }

    fn bitboard_for(&self, color: Color, piece: PieceType) -> BitBoard {
        self.get_pieces(color).bitboard_for(piece)
    }

    fn piece_at(&self, file: u8, rank: u8) -> Option<Piece> {
        let mask = to_mask(file, rank);
        for color in Color::iterator() {
            for ptype in PieceType::iterator() {
                if mask & self.bitboard_for(*color, *ptype) != 0 {
                    return Some((*color, *ptype));
                }
            }
        }
        None
    }
    fn set_square(&mut self, file: u8, rank: u8, piece: Piece) {
        let pieces = self.get_pieces_mut(piece.0);
        pieces.set_bitboard(piece.1, pieces.bitboard_for(piece.1) | to_mask(file, rank));
        return ();
    }
}

impl BitBoardPieces {
    fn blank() -> BitBoardPieces {
        BitBoardPieces {
            pawns: 0,
            rooks: 0,
            knights: 0,
            bishops: 0,
            queens: 0,
            kings: 0,
        }
    }
    fn set_bitboard(&mut self, piece: PieceType, board: BitBoard) {
        match piece {
            Pawn => self.pawns = board,
            Rook => self.rooks = board,
            Knight => self.knights = board,
            Bishop => self.bishops = board,
            Queen => self.queens = board,
            King => self.kings = board,
        }
    }
    fn bitboard_for(&self, piece: PieceType) -> BitBoard {
        match piece {
            Pawn => self.pawns,
            Rook => self.rooks,
            Knight => self.knights,
            Bishop => self.bishops,
            Queen => self.queens,
            King => self.kings,
        }
    }
}

impl std::fmt::Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut str = String::with_capacity(64 + 8);
        for rank in 0..8 {
            for file in 0..8 {
                let c: char = match self.piece_at(file, rank) {
                    Some(p) => match p {
                        (_, Pawn) => 'P',
                        (_, Rook) => 'R',
                        (_, Knight) => 'N',
                        (_, Bishop) => 'B',
                        (_, Queen) => 'Q',
                        (_, King) => 'K',
                    },
                    None => ' ',
                };
                str.push(c)
            }
            str.push('\n')
        }
        write!(f, "{}", str)
    }
}
