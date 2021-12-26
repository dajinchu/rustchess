use crate::board::Square;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MoveFlag {
    KingCastle,
    QueenCastle,
    Normal,
}
const BK: Square = Square { rank: 7, file: 4 };
const WK: Square = Square { rank: 0, file: 4 };
pub const BLACK_KING_CASTLE: Move = Move {
    from: BK,
    to: Square { rank: 7, file: 6 },
    flag: MoveFlag::KingCastle,
};
pub const BLACK_QUEEN_CASTLE: Move = Move {
    from: BK,
    to: Square { rank: 7, file: 2 },
    flag: MoveFlag::QueenCastle,
};
pub const WHITE_KING_CASTLE: Move = Move {
    from: WK,
    to: Square { rank: 0, file: 6 },
    flag: MoveFlag::KingCastle,
};
pub const WHITE_QUEEN_CASTLE: Move = Move {
    from: WK,
    to: Square { rank: 0, file: 2 },
    flag: MoveFlag::QueenCastle,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Move {
    pub from: Square,
    pub to: Square,
    pub flag: MoveFlag,
}

impl Move {
    pub fn normal(from: Square, to: Square) -> Move {
        Move {
            from,
            to,
            flag: MoveFlag::Normal,
        }
    }
    /// Create move from long algebraic
    pub fn from_algebraic(algebraic: String) -> Move {
        Move {
            from: Square::from_algebraic(algebraic.chars().take(2).collect()),
            to: Square::from_algebraic(algebraic.chars().skip(2).take(2).collect()),
            flag: MoveFlag::Normal,
        }
    }
}

impl std::fmt::Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}", self.from, self.to)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn from_algebraic() {
        assert_eq!(
            Move::from_algebraic("a2b3".to_string()),
            Move::normal(Square { rank: 1, file: 0 }, Square { rank: 2, file: 1 })
        );
        assert_eq!(
            Move::from_algebraic("h1h2".to_string()),
            Move::normal(Square { rank: 0, file: 7 }, Square { rank: 1, file: 7 })
        );
    }
}
