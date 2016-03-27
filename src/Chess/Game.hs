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

-- |'mkGame' returns a 'Game' of chess but using the given 'PieceColor' and
-- 'Board' arguments as the player and board respectively.
mkGame :: PieceColor -> Board -> Game
mkGame p = maybeNext . updateBoard newGame
    where maybeNext = if p == Black then nextPlayer else id

-- |'nextPlayer' returns a 'Game' with the player changed to the next
-- 'PieceColor'.
nextPlayer :: Game -> Game
nextPlayer game = case player game of
                    White -> Game Black (board game) (castling game) (enPassant game)
                    Black -> Game White (board game) (castling game) (enPassant game)

-- |'updateBoard' returns a 'Game' with the 'Board' changed to the 'Board'
-- argument.
updateBoard :: Game -> Board -> Game
updateBoard game brd = Game (player game) brd (castling game) (enPassant game)

-- |'updateCastling' returns a 'Game' with the castling function changed to
-- return the 'Bool' argument when given the 'PieceColor' argument.
updateCastling :: PieceColor -> Bool -> Game -> Game
updateCastling pc b game = Game (player game) (board game) newCastling (enPassant game)
    where newCastling plr = if plr == pc
                            then b
                            else castling game pc

-- |'updateEnPassant' returns a 'Game' with enPassant set to the argument.
updateEnPassant :: Maybe Position -> Game -> Game
updateEnPassant mp game = Game (player game) (board game) (castling game) mp
