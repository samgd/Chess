module Chess.Game where

import Chess.Board (PieceColor (..), Board, initial)
import Chess.Move.Type (Move)

data Game = Game { player   :: PieceColor
                 , board    :: Board
                 , castling :: PieceColor -> Bool
                 , lastMove :: Maybe Move
                 }

instance Show Game where
    show (Game p b c m) = "Game " ++ show p ++ " " ++ show b ++ " " ++ castle ++ " " ++ show m
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
                    White -> Game Black (board game) (castling game) (lastMove game)
                    Black -> Game White (board game) (castling game) (lastMove game)

-- |'updateBoard' returns a 'Game' with the 'Board' changed to the 'Board'
-- argument.
updateBoard :: Game -> Board -> Game
updateBoard game brd = Game (player game) brd (castling game) (lastMove game)

updateCastling :: PieceColor -> Bool -> Game -> Game
updateCastling pc b game = Game (player game) (board game) newCastling (lastMove game)
    where newCastling plr = if plr == pc
                            then b
                            else castling game pc

updateLast :: Move -> Game -> Game
updateLast m game = Game (player game) (board game) (castling game) (Just m)
