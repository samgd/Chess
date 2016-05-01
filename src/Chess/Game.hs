module Chess.Game where

import Chess.Board (PieceColor (..), Board, Position, initial)

data Game = Game { player    :: PieceColor
                 , board     :: Board
                 , castling  :: PieceColor -> Bool
                 , enPassant :: Maybe Position
                 }

instance Show Game where
    show (Game p b c ep) = "Game " ++ show p ++ " " ++ show b ++ " " ++ castle ++ " " ++ show ep
        where castle = show (map (\pc -> (pc, c pc)) [White, Black])

-- |'newGame' returns a new 'Game' of chess. White plays first.
newGame :: Game
newGame = Game White initial (const True) Nothing

-- |'nextPlayer' returns a 'Game' with the player changed to the next
-- 'PieceColor'.
nextPlayer :: Game -> Game
nextPlayer game = game { player = if player game == White then Black else White }

-- |'updateCastling' returns a 'Game' with the castling function changed to
-- return the 'Bool' argument when given the 'PieceColor' argument.
updateCastling :: PieceColor -> Bool -> Game -> Game
updateCastling pc b game = game { castling = newCastling }
    where newCastling plr = if plr == pc
                            then b
                            else castling game pc
