module Chess.Game where

import Chess.Board (PieceColor, Board)

data Game = Game { player :: PieceColor
                 , board  :: Board
                 }
