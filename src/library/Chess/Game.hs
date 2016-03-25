module Chess.Game where

import Chess.Board (PieceColor (..), Board)

data Game = Game { player :: PieceColor
                 , board  :: Board
                 } deriving (Show)

nextPlayer :: Game -> Game
nextPlayer game = case player game of
                    White -> Game Black (board game)
                    Black -> Game White (board game)

