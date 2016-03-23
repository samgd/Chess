module Chess.Moves where

import Chess.Board

-- |'Move' represents a 'Piece' movement on a chess 'Board'.
data Move = Move { cur :: Position
                 , new :: Position
                 } deriving (Eq, Show)


-- |'moves' returns a list of possible 'Move's.
moves :: PieceColor -> Board -> [Move]
moves = undefined
