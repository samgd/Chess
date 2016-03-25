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
            King   -> basicKing          game op
            Queen  -> basicNoJumpNoLimit game op [N, NE, E, SE, S, SW, W, NW]
            Rook   -> basicNoJumpNoLimit game op [N,     E,     S,     W    ]
            Bishop -> basicNoJumpNoLimit game op [   NE,    SE,    SW,    NW]
            Knight -> basicKnight game op
            Pawn   -> basicPawn   game op
    return $ Move op np


-- |'basicNoJumpNoLimit' returns a list of possible new positions that start
-- from the given position and move in any ONE 'Direction' from those given in
-- the argument list.
basicNoJumpNoLimit :: Game -> Position -> [Direction] -> [Position]
basicNoJumpNoLimit game op dirs = do
    move <- map (next game False True) dirs
    let mvs = takeWhile isJust $ iterate (>>= move) (return op)
    (Just np) <- mvs
    guard $ np /= op
    return np

-- The following basic_PIECE_ functions return a list of possible new positions
-- for the given _PIECE_ at the stated position.

basicKing :: Game -> Position -> [Position]
basicKing game op = do
    (Just np) <- map (\d -> next game False True d op) [N, NE, E, SE, S, SW, W, NW]
    guard $ np /= op
    return np

basicKnight :: Game -> Position -> [Position]
basicKnight game op = do
    let f op (m1, m2, m3) = next game True True m1 op >>= next game True True m2 >>= next game False True m3
        jumps = [ (N, N, W), (N, N, E), (E, E, N), (E, E, S)
                , (S, S, E), (S, S, W), (W, W, S), (W, W, N) ]
    (Just np) <- map (f op) jumps
    guard $ np /= op
    return np

basicPawn :: Game -> Position -> [Position]
basicPawn game op@(_, rank) = do
    let plr = player game
        dir = if plr == White then N else S

        -- Move forward once if vacant.
        simpleMvs = [next game False False dir op]

        -- Move forward twice if yet to move and both are vacant.
        doubleMvs = let doubleHop = next game False False dir op >>= next game False False dir
                    in case (plr, rank) of
                         (White, 2) -> [doubleHop]
                         (Black, 7) -> [doubleHop]
                         _          -> []

        -- Capture pieces that are E/W of one step foward.
        captMvs = let brd = board game
                      capt cdir = case square brd (nextPos cdir $ nextPos dir op) of
                                    Nothing   -> []
                                    (Just pi) -> [next game False False dir op >>= next game False True cdir]

                  in capt E ++ capt W

    (Just np) <- simpleMvs ++ doubleMvs ++ captMvs
    guard $ np /= op
    return np

-- |'next' returns the next position in the specified 'Direction'.
next :: Game             -- ^ Current 'Game'.
     -> Bool             -- ^ True if the piece can jump another.
     -> Bool             -- ^ True if the piece can capture another.
     -> Direction        -- ^ 'Direction' of movement.
     -> Position         -- ^ Current 'Position'.
     -> Maybe Position   -- ^ New 'Position'.
next game jumping capture d op@(file, rank) = do
    let sq = square (board game) op
    case sq of
        Nothing  -> pure ()
        (Just p) -> guard (pieceColor p == player game)

    let np = nextPos d op
        sq = square (board game) np
    case sq of
        Nothing  -> pure ()
        (Just p) -> case (jumping, capture) of
                        -- Can't jump or capture so no move possible.
                        (False, False) -> Nothing
                        -- Can't jump but can capture. Check if capture OK.
                        (False, True)  -> guard $ pieceColor p /= player game
                        -- Can jump but can't capture. Check if jump OK.
                        (True, False)  -> guard $ pieceColor p == player game
                        -- Can jump and capture. Woohoo!
                        (True, True)   -> pure ()

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
