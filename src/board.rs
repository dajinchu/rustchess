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
    passant: Option<Square>,
    halfmoves: u8,
}

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

#[derive(Debug, Copy, Clone)]
pub struct Square {
    rank: u8,
    file: u8,
}

#[derive(Debug, Copy, Clone)]
pub struct Move {
    from: Square,
    to: Square,
}

pub fn to_mask(file: u8, rank: u8) -> u64 {
    1 << (rank * 8 + file)
}
pub fn from_mask(mask: u64) -> Square {
    let mut rank = 0;
    let mut file = 0;
    // TODO: Might be faster with lookup table?
    for_bitboard_ones(mask, |r, f| {
        rank = r;
        file = f;
    });
    Square { rank, file }
}

pub fn algebraic_to_square(algebraic: String) -> Square {
    let mut chars = algebraic.chars();
    let file = letter_to_file(chars.next().unwrap());
    let rank = chars.next().unwrap().to_digit(10).unwrap() as u8;
    Square { rank, file }
}

pub fn file_to_letter(file: u8) -> char {
    (b'a' + file) as char
}

pub fn letter_to_file(file: char) -> u8 {
    file as u8 - b'a'
}

pub fn for_bitboard_ones(b: BitBoard, mut func: impl FnMut(u8, u8)) {
    let mut bb = b;
    for rank in 0..8 {
        for file in 0..8 {
            if bb & 1 == 1 {
                func(rank, file);
            }
            bb >>= 1;
        }
    }
}

fn ray_moves(from: Square, occ_opp: BitBoard, occ_self: BitBoard, dirs: &[Direction]) -> Vec<Move> {
    let mut moves = Vec::new();
    let origin = to_mask(from.file, from.rank);
    let mut dests = Vec::new();
    for dir in dirs {
        let mut pointer = origin;
        loop {
            pointer = safe_shift(pointer, *dir, 1);
            if pointer == 0 {
                break;
            }
            if pointer & occ_self != 0 {
                break;
            }
            if pointer & occ_opp != 0 {
                dests.push(pointer);
                break;
            }
            dests.push(pointer);
        }
    }
    let dest_map = dests.iter().fold(0, |acc, b| acc | b);
    for_bitboard_ones(dest_map, |rank, file| {
        moves.push(Move {
            from,
            to: Square { rank, file },
        })
    });
    moves
}

const D_N: Direction = 8;
const D_NE: Direction = 9;
const D_E: Direction = 1;
const D_SE: Direction = -7;
const D_S: Direction = -8;
const D_SW: Direction = -9;
const D_W: Direction = -1;
const D_NW: Direction = 7;
type Direction = i8;

const D_CARDINAL: [Direction; 4] = [D_N, D_E, D_S, D_W];
const D_ORDINAL: [Direction; 4] = [D_NE, D_SE, D_SW, D_NW];

/// Shift bitboard in given direction
const fn shift(board: BitBoard, dir: Direction) -> BitBoard {
    if dir < 0 {
        board >> -dir
    } else {
        board << dir
    }
}
/// Shift bitboard in given direction without wrapping
fn safe_shift(board: BitBoard, dir: Direction, dist: usize) -> BitBoard {
    shift(board & !mask_dir(dir, dist), dir * dist as i8)
}

const fn mask_file() -> [u64; 8] {
    let mut i = 0;
    let mut v: [u64; 8] = [0u64; 8];
    while i < 8 {
        v[i] = shift(0x0101_0101_0101_0101, D_E * i as i8);
        i += 1;
    }
    v
}
const fn mask_rank() -> [u64; 8] {
    let mut i = 0;
    let mut v: [u64; 8] = [0u64; 8];
    while i < 8 {
        v[i] = shift(0b1111_1111, D_N * i as i8);
        i += 1;
    }
    v
}

/// Get mask for the edge of the board in the given direction, with given steps of "thickness" for the edge
/// TODO: lookup table?
fn mask_dir(dir: Direction, steps: usize) -> BitBoard {
    let masks = match dir {
        D_N => MASK_RANK[(8 - steps)..8].to_vec(),
        D_E => MASK_FILE[(8 - steps)..8].to_vec(),
        D_S => MASK_RANK[0..(steps)].to_vec(),
        D_W => MASK_FILE[0..(steps)].to_vec(),
        D_NE => return mask_dir(D_N, steps) | mask_dir(D_E, steps),
        D_SE => return mask_dir(D_S, steps) | mask_dir(D_E, steps),
        D_SW => return mask_dir(D_S, steps) | mask_dir(D_W, steps),
        D_NW => return mask_dir(D_N, steps) | mask_dir(D_W, steps),
        _ => vec![0u64],
    };
    masks.iter().fold(0, |acc, m| acc | m)
}

const MASK_FILE: [u64; 8] = mask_file();
const MASK_RANK: [u64; 8] = mask_rank();
const MASK_EDGE: u64 = MASK_FILE[0] | MASK_FILE[7] | MASK_RANK[0] | MASK_RANK[7];

impl Board {
    pub fn starting_position() -> Board {
        Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string())
            .unwrap()
    }
    pub fn empty_board() -> Board {
        Board {
            white: BitBoardPieces::blank(),
            black: BitBoardPieces::blank(),
            active: White,
            castle_bq: true,
            castle_bk: true,
            castle_wq: true,
            castle_wk: true,
            passant: None,
            halfmoves: 0,
        }
    }
    pub fn from_fen(fen: String) -> Result<Board, ()> {
        let v: Vec<&str> = fen.split(' ').collect();
        match v[..] {
            [pieces, active, castle, passant, halfmove, _fullmove] => {
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
                let passant: Option<Square> = match passant {
                    "-" => None,
                    _ => Some(algebraic_to_square(passant.to_string())),
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
                    let rank = 7 - i;
                    let mut file: u8 = 0;
                    for c in s.chars() {
                        match c.to_digit(10) {
                            Some(num_empties) => file += num_empties as u8,
                            None => {
                                board.set_square(file, rank as u8, fen_to_piece(c).unwrap());
                                file += 1;
                            }
                        }
                    }
                }
                Ok(board)
            }
            _ => Err(()),
        }
    }

    pub fn valid_moves(&self) {
        self.pawn_moves().append(&mut self.king_moves())
    }
    fn pawn_moves(&self) -> Vec<Move> {
        self.get_pieces(self.active).pawn_moves(
            self.occupancy(self.active.other()),
            self.occupancy(self.active),
        )
    }
    fn king_moves(&self) -> Vec<Move> {
        self.get_pieces(self.active).king_moves(
            self.occupancy(self.active.other()),
            self.occupancy(self.active),
        )
    }
    fn rook_moves(&self) -> Vec<Move> {
        self.get_pieces(self.active).rook_moves(
            self.occupancy(self.active.other()),
            self.occupancy(self.active),
        )
    }
    fn bishop_moves(&self) -> Vec<Move> {
        self.get_pieces(self.active).bishop_moves(
            self.occupancy(self.active.other()),
            self.occupancy(self.active),
        )
    }
    fn occupancy(&self, color: Color) -> BitBoard {
        self.get_pieces(color).bitboard_occupancy()
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
    fn clear_square(&mut self, file: u8, rank: u8) {
        for c in Color::iterator() {
            for p in PieceType::iterator() {
                let pieces = self.get_pieces_mut(*c);
                pieces.set_bitboard(*p, pieces.bitboard_for(*p) & !to_mask(file, rank));
            }
        }
        return ();
    }
    fn set_square(&mut self, file: u8, rank: u8, piece: Piece) {
        let pieces = self.get_pieces_mut(piece.0);
        pieces.set_bitboard(piece.1, pieces.bitboard_for(piece.1) | to_mask(file, rank));
        return ();
    }
    fn to_ascii(&self) -> String {
        let mut str = String::with_capacity(90);
        for rank in (0..8).rev() {
            str.push(std::char::from_digit(rank + 1, 10).unwrap());
            for file in 0..8 {
                let c: char = match self.piece_at(file, rank as u8) {
                    Some(p) => piece_to_ascii(p),
                    None => ' ',
                };
                str.push(c)
            }
            str.push('\n')
        }
        str.push_str(" abcdefgh");
        str
    }
    /// initialize from ascii of board, with a1 bottom left (white perspective)
    fn from_ascii(str: String) -> Board {
        let mut b = Board::empty_board();
        for (i, c) in str.split('\n').enumerate() {
            let rank = 7 - i;
            for (file, c) in c.chars().enumerate() {
                match ascii_to_piece(c) {
                    Some(p) => b.set_square(file as u8, rank as u8, p),
                    None => (),
                }
            }
        }
        b
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
    fn bitboard_occupancy(&self) -> BitBoard {
        self.pawns | self.rooks | self.knights | self.bishops | self.queens | self.kings
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
    fn pawn_moves(&self, occ_opp: BitBoard, occ_self: BitBoard) -> Vec<Move> {
        let occ = occ_opp | occ_self;
        let unblocked = !(shift(occ, D_S)) & self.pawns;
        let mut moves = Vec::new();
        // Move forward 1
        for_bitboard_ones(shift(unblocked, D_N), |rank, file| {
            moves.push(Move {
                from: Square {
                    rank: rank - 1,
                    file,
                },
                to: Square { rank, file },
            })
        });
        // Move forward 2 if in starting square
        for_bitboard_ones(
            shift(unblocked & 0b11111111_00000000, 2 * D_N),
            |rank, file| {
                moves.push(Move {
                    from: Square {
                        rank: rank - 2,
                        file,
                    },
                    to: Square { rank, file },
                })
            },
        );
        // Captures
        for_bitboard_ones(
            shift(self.pawns, D_NE) & occ_opp & !MASK_FILE[0],
            |rank, file| {
                moves.push(Move {
                    from: Square {
                        rank: rank - 1,
                        file: file - 1,
                    },
                    to: Square { rank, file },
                })
            },
        );
        for_bitboard_ones(
            shift(self.pawns, D_NW) & occ_opp & !MASK_FILE[7],
            |rank, file| {
                moves.push(Move {
                    from: Square {
                        rank: rank - 1,
                        file: file + 1,
                    },
                    to: Square { rank, file },
                })
            },
        );
        moves
    }

    fn king_moves(&self, occ_opp: BitBoard, occ_self: BitBoard) -> Vec<Move> {
        let mut bits = self.kings;
        bits |= safe_shift(bits, D_E, 1) | safe_shift(bits, D_W, 1);
        bits |= safe_shift(bits, D_N, 1) | safe_shift(bits, D_S, 1);
        bits &= !occ_self;
        let king_sq = from_mask(self.kings);
        let mut moves = Vec::new();
        for_bitboard_ones(bits, |rank, file| {
            moves.push(Move {
                from: king_sq,
                to: Square { rank, file },
            })
        });
        moves
    }

    fn rook_moves(&self, occ_opp: BitBoard, occ_self: BitBoard) -> Vec<Move> {
        let mut moves = Vec::new();
        for_bitboard_ones(self.rooks, |rank, file| {
            moves.append(&mut ray_moves(
                Square { rank, file },
                occ_opp,
                occ_self,
                &D_CARDINAL,
            ));
        });
        moves
    }

    fn bishop_moves(&self, occ_opp: BitBoard, occ_self: BitBoard) -> Vec<Move> {
        let mut moves = Vec::new();
        for_bitboard_ones(self.bishops, |rank, file| {
            moves.append(&mut ray_moves(
                Square { rank, file },
                occ_opp,
                occ_self,
                &D_ORDINAL,
            ));
        });
        moves
    }
}

impl std::fmt::Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_ascii())
    }
}

impl std::fmt::Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}", self.from, self.to)
    }
}

impl std::fmt::Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}", file_to_letter(self.file), self.rank + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::match_array;

    fn move_strings(moves: Vec<Move>) -> Vec<String> {
        moves.iter().map(|m| m.to_string()).collect::<Vec<String>>()
    }

    #[test]
    fn test_file_to_letter() {
        assert_eq!(
            (0..8).map(|n| file_to_letter(n)).collect::<Vec<char>>(),
            vec!['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
        )
    }

    #[test]
    fn test_safe_shift() {
        assert_eq!(safe_shift(0b1, D_N, 1), 0b1_0000_0000);
        assert_eq!(safe_shift(0b1, D_E, 1), 0b10);
        assert_eq!(safe_shift(0b1, D_S, 1), 0);
        assert_eq!(safe_shift(0b1, D_W, 1), 0);
    }

    #[test]
    fn from_fen_starting_position() {
        let b =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string())
                .unwrap();
        assert_eq!(
            b.to_ascii(),
            [
                "8♜♞♝♛♚♝♞♜",
                "7♟♟♟♟♟♟♟♟",
                "6        ",
                "5        ",
                "4        ",
                "3        ",
                "2♙♙♙♙♙♙♙♙",
                "1♖♘♗♕♔♗♘♖",
                " abcdefgh"
            ]
            .join("\n")
        );
        assert_eq!(b.castle_bk, true);
        assert_eq!(b.castle_bq, true);
        assert_eq!(b.castle_wk, true);
        assert_eq!(b.castle_wq, true);
        assert!(b.passant.is_none());
        assert_eq!(b.halfmoves, 0)
    }

    #[test]
    fn from_fen_random() {
        let b = Board::from_fen(
            "6n1/1NqBpPbQ/3PP2P/2pppP2/1k1N3P/ppr3P1/2p5/R1n1RKBb w - - 0 1".to_string(),
        )
        .unwrap();
        assert_eq!(
            b.to_ascii(),
            [
                "8      ♞ ",
                "7 ♘♛♗♟♙♝♕",
                "6   ♙♙  ♙",
                "5  ♟♟♟♙  ",
                "4 ♚ ♘   ♙",
                "3♟♟♜   ♙ ",
                "2  ♟     ",
                "1♖ ♞ ♖♔♗♝",
                " abcdefgh",
            ]
            .join("\n")
        );
        assert_eq!(b.castle_bk, false);
        assert_eq!(b.castle_bq, false);
        assert_eq!(b.castle_wk, false);
        assert_eq!(b.castle_wq, false);
        assert!(b.passant.is_none());
        assert_eq!(b.halfmoves, 0)
    }

    #[test]
    fn pawn_moves_no_obstacles() {
        let b = Board::from_ascii(
            [
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "♙♙      ",
                "        ",
            ]
            .join("\n"),
        );
        let moves = b.pawn_moves();
        let expected = ["a2a3", "b2b3", "a2a4", "b2b4"];
        assert_eq!(move_strings(moves), expected)
    }

    #[test]
    fn pawn_moves_obstacles() {
        let b = Board::from_ascii(
            [
                "        ",
                "        ",
                "        ",
                "        ",
                "♟       ",
                "♙ ♙♗    ",
                "   ♙ ♙  ",
                "        ",
            ]
            .join("\n"),
        );
        let moves = b.pawn_moves();
        let expected = ["f2f3", "c3c4", "f2f4"];
        assert_eq!(move_strings(moves), expected)
    }

    #[test]
    fn pawn_moves_captures() {
        let b = Board::from_ascii(
            [
                "        ",
                "        ",
                "        ",
                "        ",
                "♟♟♟    ♟",
                "♙ ♙    ♙",
                "        ",
                "        ",
            ]
            .join("\n"),
        );
        let moves = b.pawn_moves();
        let expected = ["a3b4", "c3b4"];
        assert_eq!(move_strings(moves), expected)
    }

    #[test]
    fn king_moves_no_obstacles() {
        let b = Board::from_ascii(
            [
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "      ♔ ",
                "        ",
                "        ",
            ]
            .join("\n"),
        );
        let moves = b.king_moves();
        let expected = vec![
            "g3g4", "g3h4", "g3h3", "g3h2", "g3g2", "g3f2", "g3f3", "g3f4",
        ];
        assert!(match_array(move_strings(moves), expected))
    }

    #[test]
    fn king_moves_obstacles() {
        let b = Board::from_ascii(
            [
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "      ♟♙",
                "       ♔",
            ]
            .join("\n"),
        );
        let moves = b.king_moves();
        let expected = ["h1g1", "h1g2"];
        assert_eq!(move_strings(moves), expected)
    }

    #[test]
    fn rook_moves_no_obstacles() {
        let b = Board::from_ascii(
            [
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "  ♖     ",
                "        ",
            ]
            .join("\n"),
        );
        let moves = b.rook_moves();
        let expected = [
            "c1", "c3", "c4", "c5", "c6", "c7", "c8", "a2", "b2", "d2", "e2", "f2", "g2", "h2",
        ]
        .iter()
        .map(|s| "c2".to_string() + s)
        .collect();
        assert!(match_array(move_strings(moves), expected))
    }

    #[test]
    fn rook_moves_obstacles() {
        let b = Board::from_ascii(
            [
                "        ",
                "        ",
                "        ",
                "  ♟     ",
                "  ♟     ",
                "        ",
                "  ♖♙   ♟",
                "        ",
            ]
            .join("\n"),
        );
        let moves = b.rook_moves();
        let expected = ["a2", "b2", "c3", "c4", "c1"]
            .iter()
            .map(|s| "c2".to_string() + s)
            .collect();
        assert!(match_array(move_strings(moves), expected))
    }

    #[test]
    fn rook_moves_multiple() {
        let b = Board::from_ascii(
            [
                "        ",
                "        ",
                "        ",
                "  ♟     ",
                "  ♟     ",
                "        ",
                "  ♖♙   ♟",
                "   ♖   ♟",
            ]
            .join("\n"),
        );
        let moves = b.rook_moves();
        let expected = vec![
            "c2a2", "c2b2", "c2c3", "c2c4", "c2c1", "d1a1", "d1b1", "d1c1", "d1e1", "d1f1", "d1g1",
            "d1h1",
        ];
        assert!(match_array(move_strings(moves), expected))
    }

    #[test]
    fn bishop_moves_no_obs() {
        let b = Board::from_ascii(
            [
                "        ",
                "      ♗ ",
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
            ]
            .join("\n"),
        );
        let moves = b.bishop_moves();
        let expected = vec![
            "g7h8", "g7a1", "g7b2", "g7c3", "g7d4", "g7e5", "g7f6", "g7f8", "g7h6",
        ];
        assert!(match_array(move_strings(moves), expected))
    }

    #[test]
    fn bishop_moves_obs() {
        let b = Board::from_ascii(
            [
                "        ",
                "        ",
                "      ♜ ",
                "       ♗",
                "        ",
                "     ♕  ",
                "        ",
                "        ",
            ]
            .join("\n"),
        );
        let moves = b.bishop_moves();
        let expected = vec!["h5g6", "h5g4"];
        assert!(match_array(move_strings(moves), expected))
    }
}
