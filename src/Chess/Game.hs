module Chess.Game where

import Chess.Board (PieceColor (..), Board, initial)

data Game = Game { player   :: PieceColor
                 , board    :: Board
                 , castling :: PieceColor -> Bool
                 }

instance Show Game where
    show (Game p b c) = "Game " ++ show p ++ " " ++ show b ++ " " ++ castle
        where castle = show (map (\pc -> (pc, c pc)) [White, Black])

-- |'newGame' returns a new 'Game' of chess. White plays first.
newGame :: Game
newGame = Game White initial (const True)

-- |'mkGame' returns a 'Game' of chess but using the given 'PieceColor' and
-- 'Board' arguments as the player and board respectively.
mkGame :: PieceColor -> Board -> Game
mkGame p = maybeNext . updateBoard newGame
    where maybeNext = if p == Black then nextPlayer else id

-- |'nextPlayer' returns a 'Game' with the player changed to the next
-- 'PieceColor'.
nextPlayer :: Game -> Game
nextPlayer game = case player game of
                    White -> Game Black (board game) (castling game)
                    Black -> Game White (board game) (castling game)

-- |'updateBoard' returns a 'Game' with the 'Board' changed to the 'Board'
-- argument.
updateBoard :: Game -> Board -> Game
updateBoard game brd = Game (player game) brd (castling game)
