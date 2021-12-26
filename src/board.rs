use crate::movement::*;
use crate::piece::Color::*;
use crate::piece::PieceType::*;
use crate::piece::*;
use crate::vec2d::*;
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Square {
    pub rank: u8,
    pub file: u8,
}

impl Square {
    pub fn from_algebraic(algebraic: String) -> Square {
        let mut chars = algebraic.chars();
        let file = letter_to_file(chars.next().unwrap());
        let rank = (chars.next().unwrap().to_digit(10).unwrap() - 1) as u8;
        Square { rank, file }
    }
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

fn ray_moves(from: Square, occ_opp: BitBoard, occ_self: BitBoard, dirs: &[Vec2D]) -> Vec<Move> {
    let mut moves = Vec::new();
    let origin = to_mask(from.file, from.rank);
    let mut dests = Vec::new();
    for dir in dirs {
        let mut pointer = origin;
        loop {
            pointer = safe_shift(pointer, *dir);
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
        moves.push(Move::normal(from, Square { rank, file }))
    });
    moves
}

const D_N: Vec2D = vec2d!(1, 0);
const D_E: Vec2D = vec2d!(0, 1);
const D_S: Vec2D = vec2d!(-1, 0);
const D_W: Vec2D = vec2d!(0, -1);
const D_NE: Vec2D = D_N + D_E;
const D_SE: Vec2D = D_S + D_E;
const D_SW: Vec2D = D_S + D_W;
const D_NW: Vec2D = D_N + D_W;

const D_CARDINAL: [Vec2D; 4] = [D_N, D_E, D_S, D_W];
const D_ORDINAL: [Vec2D; 4] = [D_NE, D_SE, D_SW, D_NW];
const D_ALL: [Vec2D; 8] = [D_N, D_E, D_S, D_W, D_NE, D_SE, D_SW, D_NW];

/// Shift bitboard in given vec2d
const fn shift(board: BitBoard, v: Vec2D) -> BitBoard {
    let d = v.vertical * 8 + v.horizontal;
    if d < 0 {
        board >> -d
    } else {
        board << d
    }
}

/// Shift bitboard in given vec2d without wrapping
fn safe_shift(board: BitBoard, v: Vec2D) -> BitBoard {
    shift(board & !mask_dir(v), v)
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

/// Get mask for the edge of the board from vec2d, such that squares in the mask moved by the vec2d would go off-map
/// TODO: lookup table?
fn mask_dir(
    Vec2D {
        vertical,
        horizontal,
    }: Vec2D,
) -> BitBoard {
    let masks = [
        if vertical > 0 {
            &MASK_RANK[(8 - vertical as usize)..8]
        } else {
            &MASK_RANK[(0..(-vertical) as usize)]
        },
        if horizontal > 0 {
            &MASK_FILE[(8 - horizontal as usize)..8]
        } else {
            &MASK_FILE[(0..(-horizontal) as usize)]
        },
    ]
    .concat();
    masks.iter().fold(0, |acc, m| acc | m)
}

/// West to East
const MASK_FILE: [u64; 8] = mask_file();
/// South to North
const MASK_RANK: [u64; 8] = mask_rank();

const A_FILE: u8 = 0;
const B_FILE: u8 = 1;
const C_FILE: u8 = 2;
const D_FILE: u8 = 3;
const E_FILE: u8 = 4;
const F_FILE: u8 = 5;
const G_FILE: u8 = 6;
const H_FILE: u8 = 7;

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
                    _ => Some(Square::from_algebraic(passant.to_string())),
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

    pub fn valid_moves(&self) -> Vec<Move> {
        [
            self.pawn_moves(),
            self.king_moves(),
            self.rook_moves(),
            self.bishop_moves(),
            self.queen_moves(),
            self.knight_moves(),
        ]
        .concat()
    }

    /// TODO: Some checks on validity? Or just don't?
    pub fn make_move(&mut self, m: Move) {
        let Move { from, to, flag } = m;
        println!("{}", m);
        let p = self
            .piece_at(from.file, from.rank)
            .expect("Tried to move from square with no piece");

        // reset halfmove if pawn move or capture
        if p.1 == Pawn || self.piece_at(to.file, to.rank).is_some() {
            self.halfmoves = 0
        } else {
            self.halfmoves += 1;
        }

        self.move_piece(m, p);
        // Handle Castling
        match flag {
            MoveFlag::KingCastle => {
                self.set_square(F_FILE, to.rank, (self.active, Rook));
                self.clear_square(H_FILE, to.rank);
            }
            MoveFlag::QueenCastle => {
                self.set_square(D_FILE, to.rank, (self.active, Rook));
                self.clear_square(A_FILE, to.rank);
            }
            MoveFlag::Normal => (),
        }

        // update castle rights
        match (p.0, p.1, from.file) {
            (Black, King, E_FILE) => {
                self.castle_bk = false;
                self.castle_bq = false;
            }
            (White, King, E_FILE) => {
                self.castle_wk = false;
                self.castle_wq = false;
            }
            (Black, Rook, H_FILE) => {
                self.castle_bk = false;
            }
            (White, Rook, H_FILE) => {
                self.castle_wk = false;
            }
            (Black, Rook, A_FILE) => {
                self.castle_bq = false;
            }
            (White, Rook, A_FILE) => {
                self.castle_wq = false;
            }
            _ => (),
        }

        // swap active
        self.active = self.active.other();
    }

    /// internal helper to move a piece
    fn move_piece(&mut self, Move { to, from, .. }: Move, p: Piece) {
        self.set_square(to.file, to.rank, p);
        self.clear_square(from.file, from.rank);
    }

    fn pawn_moves(&self) -> Vec<Move> {
        self.get_pieces(self.active).pawn_moves(
            self.active,
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
    fn queen_moves(&self) -> Vec<Move> {
        self.get_pieces(self.active).queen_moves(
            self.occupancy(self.active.other()),
            self.occupancy(self.active),
        )
    }
    fn knight_moves(&self) -> Vec<Move> {
        self.get_pieces(self.active).knight_moves(
            self.occupancy(self.active.other()),
            self.occupancy(self.active),
        )
    }
    // TODO: Check king safety. How to calculate opponent attack? 
    fn castle_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();
        if self.active == Black
            && self.castle_bk
            && self.all_occupancy() & (to_mask(F_FILE, 7) | to_mask(G_FILE, 7)) == 0
        {
            moves.push(BLACK_KING_CASTLE);
        }
        if self.active == Black
            && self.castle_bq
            && self.all_occupancy() & (to_mask(B_FILE, 7) | to_mask(C_FILE, 7) | to_mask(D_FILE, 7))
                == 0
        {
            moves.push(BLACK_QUEEN_CASTLE);
        }
        if self.active == White
            && self.castle_wk
            && self.all_occupancy() & (to_mask(F_FILE, 0) | to_mask(G_FILE, 0)) == 0
        {
            moves.push(WHITE_KING_CASTLE);
        }
        if self.active == White
            && self.castle_wq
            && self.all_occupancy() & (to_mask(F_FILE, 0) | to_mask(C_FILE, 0) | to_mask(D_FILE, 0))
                == 0
        {
            moves.push(WHITE_QUEEN_CASTLE);
        }
        moves
    }
    fn occupancy(&self, color: Color) -> BitBoard {
        self.get_pieces(color).bitboard_occupancy()
    }
    fn all_occupancy(&self) -> BitBoard {
        self.occupancy(Black) | self.occupancy(White)
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
    fn to_ascii(&self, label: bool) -> String {
        let mut str = String::with_capacity(90);
        for rank in (0..8).rev() {
            if label {
                str.push(std::char::from_digit(rank + 1, 10).unwrap());
            }
            for file in 0..8 {
                let c: char = match self.piece_at(file, rank as u8) {
                    Some(p) => piece_to_ascii(p),
                    None => ' ',
                };
                str.push(c)
            }
            if rank > 0 {
                str.push('\n')
            }
        }
        if label {
            str.push_str("\n abcdefgh");
        }
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
    fn pawn_moves(&self, color: Color, occ_opp: BitBoard, occ_self: BitBoard) -> Vec<Move> {
        let sign = if color == Black { -1 } else { 1 };
        let occ = occ_opp | occ_self;
        let unblocked = !(shift(occ, D_S * sign)) & self.pawns;
        let mut moves = Vec::new();
        // Move forward 1
        for_bitboard_ones(shift(unblocked, D_N * sign), |rank, file| {
            moves.push(Move::normal(
                Square {
                    rank: (rank as i8 - sign) as u8,
                    file,
                },
                Square { rank, file },
            ))
        });
        // Move forward 2 if in starting square
        for_bitboard_ones(
            shift(
                unblocked & MASK_RANK[if color == Black { 6 } else { 1 }],
                D_N * 2 * sign,
            ),
            |rank, file| {
                moves.push(Move::normal(
                    Square {
                        rank: (rank as i8 - 2 * sign) as u8,
                        file,
                    },
                    Square { rank, file },
                ))
            },
        );
        // Captures
        for_bitboard_ones(
            safe_shift(self.pawns, D_N * sign + D_E) & occ_opp,
            |rank, file| {
                moves.push(Move::normal(
                    Square {
                        rank: (rank as i8 - sign) as u8,
                        file: file - 1,
                    },
                    Square { rank, file },
                ))
            },
        );
        for_bitboard_ones(
            safe_shift(self.pawns, D_N * sign + D_W) & occ_opp,
            |rank, file| {
                moves.push(Move::normal(
                    Square {
                        rank: (rank as i8 - sign) as u8,
                        file: file + 1,
                    },
                    Square { rank, file },
                ))
            },
        );
        moves
    }

    fn king_moves(&self, occ_opp: BitBoard, occ_self: BitBoard) -> Vec<Move> {
        let mut bits = self.kings;
        bits |= safe_shift(bits, D_E) | safe_shift(bits, D_W);
        bits |= safe_shift(bits, D_N) | safe_shift(bits, D_S);
        bits &= !occ_self;
        let king_sq = from_mask(self.kings);
        let mut moves = Vec::new();
        for_bitboard_ones(bits, |rank, file| {
            moves.push(Move::normal(king_sq, Square { rank, file }))
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

    fn queen_moves(&self, occ_opp: BitBoard, occ_self: BitBoard) -> Vec<Move> {
        let mut moves = Vec::new();
        for_bitboard_ones(self.bishops, |rank, file| {
            moves.append(&mut ray_moves(
                Square { rank, file },
                occ_opp,
                occ_self,
                &D_ALL,
            ));
        });
        moves
    }

    fn knight_moves(&self, occ_opp: BitBoard, occ_self: BitBoard) -> Vec<Move> {
        let mut moves = Vec::new();

        for (v, h) in [(1, 2), (2, 1)] {
            for (s1, s2) in [(1, 1), (1, -1), (-1, 1), (-1, -1)] {
                let vec = vec2d!(v * s1, h * s2);
                for_bitboard_ones(safe_shift(self.knights, vec) & !occ_self, |rank, file| {
                    moves.push(Move::normal(
                        Square {
                            rank: ((rank as i8) - vec.vertical) as u8,
                            file: ((file as i8) - vec.horizontal) as u8,
                        },
                        Square { rank, file },
                    ));
                });
            }
        }
        moves
    }
}

impl std::fmt::Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_ascii(false))
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
        assert_eq!(safe_shift(0b1, D_N), 0b1_0000_0000);
        assert_eq!(safe_shift(0b1, D_E), 0b10);
        assert_eq!(safe_shift(0b1, D_S), 0);
        assert_eq!(safe_shift(0b1, D_W), 0);
    }

    #[test]
    fn from_fen_starting_position() {
        let b =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string())
                .unwrap();
        assert_eq!(
            b.to_ascii(true),
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
            b.to_ascii(true),
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
    fn pawn_moves_black() {
        let mut b = Board::from_ascii(
            [
                "        ",
                "  ♟     ",
                "        ",
                "        ",
                "♟♟     ♟",
                "♙      ♙",
                "        ",
                "        ",
            ]
            .join("\n"),
        );
        b.active = Black;
        let moves = b.pawn_moves();
        let expected = vec!["b4a3", "b4b3", "c7c6", "c7c5"];
        assert!(match_array(move_strings(moves), expected));
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

    #[test]
    fn knight_moves_no_obs() {
        let b = Board::from_ascii(
            [
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "  ♘     ",
                "        ",
                "        ",
            ]
            .join("\n"),
        );
        let moves = b.knight_moves();
        let expected = vec![
            "c3a2", "c3b1", "c3d1", "c3e2", "c3a4", "c3b5", "c3d5", "c3e4",
        ];
        assert!(match_array(move_strings(moves), expected))
    }

    #[test]
    fn knight_moves_obs() {
        let b = Board::from_ascii(
            [
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "♜       ",
                "   ♗    ",
                " ♘      ",
            ]
            .join("\n"),
        );
        let moves = b.knight_moves();
        let expected = vec!["b1a3", "b1c3"];
        assert!(match_array(move_strings(moves), expected))
    }

    #[test]
    fn castle_moves() {
        let mut b = Board::from_ascii(
            [
                "♜   ♚♝♞♜",
                "♟♟♟♟♟♟♟♟",
                "        ",
                "        ",
                "        ",
                "        ",
                "♙♙♙♙♙♙♙♙",
                "♖♘♗♕♔  ♖",
            ]
            .join("\n"),
        );
        let moves = b.castle_moves();
        assert_eq!(moves, [WHITE_KING_CASTLE]);
        b.active = Black;
        let moves = b.castle_moves();
        assert_eq!(moves, [BLACK_QUEEN_CASTLE]);
    }

    #[test]
    fn castle_moves_lost_rights() {
        let mut b = Board::from_ascii(
            [
                "♜   ♚♝♞♜",
                "♟♟♟♟♟♟♟♟",
                "        ",
                "        ",
                "        ",
                "        ",
                "♙♙♙♙♙♙♙♙",
                "♖♘♗♕♔  ♖",
            ]
            .join("\n"),
        );
        b.castle_wk = false;
        let moves = b.castle_moves();
        assert_eq!(moves, []);
    }

    #[test]
    fn castle_moves_attacked_square() {
        let b = Board::from_ascii(
            [
                "    ♚♝♞ ",
                "♟♟♟♟♟♟♟♟",
                "        ",
                "        ",
                " ♜   ♜  ",
                "        ",
                "♙ ♙♙♙ ♙♙",
                "♖   ♔  ♖",
            ]
            .join("\n"),
        );
        let moves = b.castle_moves();
        assert_eq!(moves, [WHITE_QUEEN_CASTLE]);
    }

    #[test]
    fn make_move_normal() {
        let mut b = Board::from_ascii(
            [
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "   ♗    ",
                " ♘      ",
            ]
            .join("\n"),
        );
        b.make_move(Move::from_algebraic("b1a3".to_string()));
        assert_eq!(
            b.to_ascii(false),
            [
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "♘       ",
                "   ♗    ",
                "        ",
            ]
            .join("\n"),
        );
        assert_eq!(b.halfmoves, 1);
    }

    #[test]
    fn make_move_capture() {
        let mut b = Board::from_ascii(
            [
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "   ♗    ",
                " ♘      ",
            ]
            .join("\n"),
        );
        b.make_move(Move::from_algebraic("b1d2".to_string()));
        assert_eq!(
            b.to_ascii(false),
            [
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "        ",
                "   ♘    ",
                "        ",
            ]
            .join("\n"),
        );
        assert_eq!(b.halfmoves, 0);
    }

    #[test]
    fn make_move_lose_castle() {
        let mut b = Board::from_ascii(
            [
                "♜♞♝♛♚♝♞♜",
                "♟♟♟♟ ♟♟♟",
                "        ",
                "        ",
                "        ",
                "        ",
                " ♙♙♙♙♙♙ ",
                "♖♘♗♕♔♗♘♖",
            ]
            .join("\n"),
        );
        b.make_move(Move::from_algebraic("h1h2".to_string()));
        assert_eq!(b.castle_wk, false);
        assert_eq!(b.castle_wq, true);
        assert_eq!(b.castle_bk, true);
        assert_eq!(b.castle_bq, true);
        b.make_move(Move::from_algebraic("b1c3".to_string()));
        b.make_move(Move::from_algebraic("a1a4".to_string()));
        assert_eq!(b.castle_wk, false);
        assert_eq!(b.castle_wq, false);
        assert_eq!(b.castle_bk, true);
        assert_eq!(b.castle_bq, true);
        b.make_move(Move::from_algebraic("e8e7".to_string()));
        assert_eq!(b.castle_wk, false);
        assert_eq!(b.castle_wq, false);
        assert_eq!(b.castle_bk, false);
        assert_eq!(b.castle_bq, false);
        assert_eq!(b.halfmoves, 4);
    }

    #[test]
    fn make_move_do_castle() {
        let mut b = Board::from_ascii(
            [
                "♜   ♚♝♞♜",
                "♟♟♟♟♟♟♟♟",
                "        ",
                "        ",
                "        ",
                "        ",
                "♙♙♙♙♙♙♙♙",
                "♖♘♗♕♔  ♖",
            ]
            .join("\n"),
        );
        b.make_move(WHITE_KING_CASTLE);
        b.make_move(BLACK_QUEEN_CASTLE);
        assert_eq!(
            b.to_ascii(false),
            [
                "  ♚♜ ♝♞♜",
                "♟♟♟♟♟♟♟♟",
                "        ",
                "        ",
                "        ",
                "        ",
                "♙♙♙♙♙♙♙♙",
                "♖♘♗♕ ♖♔ ",
            ]
            .join("\n"),
        );
        assert_eq!(b.castle_wk, false);
        assert_eq!(b.castle_wq, false);
        assert_eq!(b.castle_bk, false);
        assert_eq!(b.castle_bq, false);
        assert_eq!(b.halfmoves, 2);
    }
    // promotion and en passant
}
