module Chess.Moves
    ( Move
    , moves
    , move
    ) where

import Chess.Board
import Chess.Game

import qualified Data.List.Safe as SL
import Data.Maybe (isJust, Maybe (Just))
import Control.Monad ((>=>), guard)

-- |'Move' represents a 'Piece' movement on a chess 'Board'.
data Move = Move { cur :: Position
                 , new :: Position
                 } deriving (Eq, Show)

-- |'moves' returns a list of possible 'Move's.
moves :: Game -> [Move]
moves game = do
    file <- ['a'..'h']
    rank <- [1..8]
    basic game (file, rank)

-- |'move' returns a tuple consisting of the taken 'Square' and the new 'Game'
-- state.
move :: Game -> Move -> Maybe (Square, Game)
move game mv = do
    (osq, rmCur) <- update (cur mv) Nothing (board game)
    (nsq, upd)   <- update (new mv) osq     rmCur
    return (nsq, Game (nextPlayer game) upd)

----------------------------------  Internal  ----------------------------------

-- |'update' updates the given 'Board' with the given 'Square' at 'Position'
-- and returns a tuple of the replaced 'Square' and the new 'Board'.
update :: Position -> Square -> Board -> Maybe (Square, Board)
update (f, r) sq brd = do
    rnk <- rank brd r
    fi  <- fileIndex f
    osq <- rnk SL.!! fi

    let updateRank (i, rnk) = if i == r
                              then map updateFile (zip ['a'..'h'] rnk)
                              else rnk
        updateFile (i, csq) = if i == f
                              then sq
                              else csq

    return (osq, map updateRank (zip [8, 7..1] brd))

-- |'Direction' represents a direction of movement on a 'Board'.
data Direction = N | NE | E | SE | S | SW | W | NW

-- |'basic' returns a list of basic 'Move's for a given Game and position.
-- It does not check for castling or en passant.
basic :: Game -> Position -> [Move]
basic game op@(file, rank) = do
    guard $ validPosition op   -- Current position must be valid.
    (Just piece) <- [square (board game) op]
    np <- case pieceType piece of
            King   -> basicKing   game op
            Queen  -> basicQueen  game op
            Rook   -> basicRook   game op
            Bishop -> basicBishop game op
            Knight -> basicKnight game op
            Pawn   -> basicPawn   game op
    return $ Move op np

-- The following basic_PIECE_ functions return a list of possible new positions
-- for the given _PIECE_ at the stated position.

basicKing :: Game -> Position -> [Position]
basicKing game op = do
    (Just np) <- map (\d -> next game False d op) [N, NE, E, SE, S, SW, W, NW]
    guard $ np /= op
    return np

basicQueen :: Game -> Position -> [Position]
basicQueen game op = do
    move <- map (next game False) [N, NE, E, SE, S, SW, W, NW]
    let mvs = takeWhile isJust $ iterate (>>= move) (return op)
    (Just np) <- mvs
    guard $ np /= op
    return np

basicRook :: Game -> Position -> [Position]
basicRook game op = do
    move <- map (next game False) [N, E, S, W]
    let mvs = takeWhile isJust $ iterate (>>= move) (return op)
    (Just np) <- mvs
    guard $ np /= op
    return np

basicBishop :: Game -> Position -> [Position]
basicBishop game op = do
    move <- map (next game False) [NE, SE, SW, NW]
    let mvs = takeWhile isJust $ iterate (>>= move) (return op)
    (Just np) <- mvs
    guard $ np /= op
    return np

basicKnight :: Game -> Position -> [Position]
basicKnight game op = do
    let f op (m1, m2, m3) = next game True m1 op >>= next game True m2 >>= next game False m3
        jumps = [ (N, N, W), (N, N, E), (E, E, N), (E, E, S)
                , (S, S, E), (S, S, W), (W, W, S), (W, W, N) ]
    (Just np) <- map (f op) jumps
    guard $ np /= op
    return np

basicPawn :: Game -> Position -> [Position]
basicPawn game op@(_, rank) = do
    let f   = foldl1 (>=>) . map (next game False)
        plr = player game

        simpleMvs = case plr of
                      White -> [[N]]
                      Black -> [[S]]

        doubleMvs = case (plr, rank) of
                      (White, 2) -> [[N, N]]
                      (Black, 7) -> [[S, S]]
                      _          -> []

        pawnCapture dir = case square (board game) (nextPos dir op) of
                            Nothing  -> []
                            (Just pi) -> [[dir] | pieceColor pi /= player game]

        captMvs   = case plr of
                      White -> pawnCapture NE ++ pawnCapture NW
                      Black -> pawnCapture SE ++ pawnCapture SW

        mvs = simpleMvs ++ doubleMvs ++ captMvs

    (Just np) <- map (`f` op) mvs
    guard $ np /= op
    return np

-- |'next' returns the next position in the specified 'Direction'. The 'Bool' arg
-- is True if the 'Piece' is allowed to jump other 'Piece's. It prevents a
-- 'Piece' from moving to a 'Square' currently occupied by another 'Piece' of
-- the same player.
next :: Game -> Bool -> Direction -> Position -> Maybe Position
next game jumping d op@(file, rank) = do
    let sq = square (board game) op
    case sq of
        Nothing  -> pure ()
        (Just p) -> guard (pieceColor p == player game)

    let np = nextPos d op
        sq = square (board game) np
    case sq of
        Nothing  -> pure ()
        (Just p) -> if not jumping
                    then guard (pieceColor p /= player game)
                    else pure ()
    guard $ validPosition np
    return np

-- |'nextPos' naively returns the next 'Position' in a given 'Direction'.
nextPos :: Direction -> Position -> Position
nextPos d (file, rank) = np where np = case d of
                                         N  -> (     file, succ rank)
                                         NE -> (succ file, succ rank)
                                         E  -> (succ file,      rank)
                                         SE -> (succ file, pred rank)
                                         S  -> (     file, pred rank)
                                         SW -> (pred file, pred rank)
                                         W  -> (pred file,      rank)
                                         NW -> (pred file, succ rank)
