module Chess.Move.Type
    ( MoveType (..)
    , Move (..)
    ) where

import Chess.Board

data MoveType
    = Basic
    | Castling
    deriving (Eq, Show)

-- |'Move' represents a move on a chess 'Board'.
data Move = Move { typ :: MoveType
                 , cur :: Position
                 , new :: Position
                 } deriving (Eq, Show)

