module Chess.Game where

import Chess.Board (PieceColor (..), Board)

data Game = Game { player :: PieceColor
                 , board  :: Board
                 } deriving (Show)

nextPlayer :: Game -> PieceColor
nextPlayer game = case player game of
                    White -> Black
                    Black -> White

