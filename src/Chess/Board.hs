module Chess.Board where

import Data.Char (ord)
import Data.Maybe
import Control.Monad (guard, join, msum)
import qualified Data.List.Safe as SL

-- |'PieceColor' represents the color of a chess piece.
data PieceColor
    = Black
    | White
    deriving (Eq, Show)

-- |'PieceType' represents the type of a chess piece.
data PieceType
    = King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn
    deriving (Eq, Show)

-- |'Piece' represents a chess piece.
data Piece = Piece { pieceColor :: PieceColor
                   , pieceType  :: PieceType
                   } deriving (Eq, Show)

-- |'Square' represents a single square of a chess board.
type Square = Maybe Piece

-- |'Rank' represents a row of a chess board.
type Rank   = [Square]

-- |'File' represents a column of a chess board.
type File   = [Square]

-- |'Board' represents a chess board.
type Board  = [Rank]

-- |'Position' is used for indexing a 'Board'.
type Position = (Int, Char)

-- |'empty' returns an 'Rank'.
empty :: Rank
empty = replicate 8 Nothing

-- |'pawns' returns a 'Rank' of 'Pawn's.
pawns :: PieceColor -> Rank
pawns c = map (Just . Piece c) (replicate 8 Pawn)

-- |'initialRank' returns the other, non-pawn, starting 'Rank'.
initialRank :: PieceColor -> Rank
initialRank c = map (Just . Piece c) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- |'initial' returns the initial 'Board' for a chess game.
initial :: Board
initial = [ initialRank Black
          , pawns       Black
          , empty
          , empty
          , empty
          , empty
          , pawns       White
          , initialRank White
          ]

-- |'fileIndex' returns the file index for a given character index.
-- The character must be 'a'-'h'.
fileIndex :: Char -> Maybe Int
fileIndex c = do
    let n = (ord c) - (ord 'a')
    guard (n > 0 && n <= 7)
    return n

-- |'square' returns the 'Square' at the given 'Position'.
square :: Board -> Position -> Square
square b (i, j) = do
    r <- rank b i
    n <- fileIndex j
    r !! n

-- |'rank' returns the specified 'Rank' if it exists. 'Rank's are indexed 1-8.
rank :: Board -> Int -> Maybe Rank
rank b n = b SL.!! (n - 1)

-- |'file' returns the specified 'File' if it exists. 'File's are indexed 'a'-'h'.
file :: Board -> Char -> Maybe File
file b c = do
    n <- fileIndex c
    return $ map (!! n) b

