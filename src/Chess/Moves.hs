module Chess.Moves
    ( Move (..)
    , moves
    , move
    ) where

import Chess.Board
import Chess.Game

import qualified Data.List.Safe as SL
import Control.Monad (guard)
import Data.Maybe (isJust)

-- |'Move' represents a 'Piece' movement on a chess 'Board'.
data Move
    = Move Position Position
    | EnPassant
    deriving (Eq, Show)

cur :: Move -> Maybe Position
cur (Move c _) = Just c
cur EnPassant  = Nothing

new :: Move -> Maybe Position
new (Move _ n) = Just n
new EnPassant  = Nothing

-- |'moves' returns a list of possible 'Move's.
moves :: Game -> [Move]
moves game = do
    let mvs = basic game

        -- 'stopCheck' returns True if the Move gets the current player's king
        -- out of check.
        stopCheck :: Move -> Bool
        stopCheck mv = maybe False (not . inCheck . nextPlayer . snd) (move game mv)

    if inCheck game
      then filter stopCheck mvs
      else mvs

-- |'move' plays the given 'Move' and returns a tuple consisting of the taken
-- 'Square' and the new 'Game' state. It promotes 'Pawn's to 'Queen's if they
-- reach the eighth 'Rank'.
move :: Game -> Move -> Maybe (Square, Game)
move game mv = case mv of
                 (Move _ _) -> basicMove     game mv
                 EnPassant  -> enPassantMove game mv

----------------------------------  Internal  ----------------------------------

basicMove :: Game -> Move -> Maybe (Square, Game)
basicMove game mv = do
    oldPos <- cur mv
    newPos <- new mv
    let plr    = player game

    (oldSq, rmCur) <- update oldPos Nothing (board game)

    -- Cannot move opponents pieces!
    case oldSq of
        Nothing  -> pure () -- OK
        (Just p) -> guard $ pieceColor p == plr

    -- Promote pawn if pawn reaches eighth rank.
    let updSq = case (newPos, oldSq) of
                ((_, 8), Just (Piece White Pawn)) -> Just $ Piece White Queen
                ((_, 1), Just (Piece Black Pawn)) -> Just $ Piece Black Queen
                _                                   -> oldSq

    (newSq, updBrd) <- update newPos updSq rmCur
    return (newSq, nextPlayer $ updateBoard game updBrd)

enPassantMove :: Game -> Move -> Maybe (Square, Game)
enPassantMove game mv = undefined

-- |'inCheck' returns True if the current player's king is in check.
inCheck :: Game -> Bool
inCheck game = any captureKing (basic $ nextPlayer game)
    where captureKing mv = let sq = new mv >>= square (board game)
                           in  maybe False ((==) King . pieceType) sq

-- |'update' updates the given 'Board' with the given 'Square' at 'Position'
-- and returns a tuple of the replaced 'Square' and the new 'Board'.
update :: Position -> Square -> Board -> Maybe (Square, Board)
update (f, r) sq brd = do
    rnk <- rank brd r
    fi  <- fileIndex f
    osq <- rnk SL.!! fi

    let updateRank (i, rk)  = if i == r
                              then zipWith (curry updateFile) ['a'..'h'] rk
                              else rk
        updateFile (i, csq) = if i == f
                              then sq
                              else csq

    return (osq, zipWith (curry updateRank) [8, 7..1] brd)

-- |'Direction' represents a direction of movement on a 'Board'.
data Direction = N | NE | E | SE | S | SW | W | NW

-- |'basic' returns a list of basic 'Move's for a given Game.
-- It does not check for castling, en passant or any issues regarding check.
basic :: Game -> [Move]
basic game = do
    f <- ['a'..'h']
    r <- [1..8]
    let op = (f, r)
    (Just piece) <- [square (board game) op]
    np <- case pieceType piece of
            King   -> basicKing          game op
            Queen  -> basicNoJumpNoLimit game op [N, NE, E, SE, S, SW, W, NW]
            Rook   -> basicNoJumpNoLimit game op [N,     E,     S,     W    ]
            Bishop -> basicNoJumpNoLimit game op [   NE,    SE,    SW,    NW]
            Knight -> basicKnight        game op
            Pawn   -> basicPawn          game op
    return $ Move op np

-- |'basicNoJumpNoLimit' returns a list of possible new positions that start
-- from the given position and move in any ONE 'Direction' from those given in
-- the argument list.
basicNoJumpNoLimit :: Game -> Position -> [Direction] -> [Position]
basicNoJumpNoLimit game op dirs = do
    mv <- map (next game False True) dirs
    let mvs = takeWhile isJust $ iterate (>>= mv) (return op)
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
    let f pos (m1, m2, m3) = next game True True m1 pos >>= next game True True m2 >>= next game False True m3
        jumps = [ (N, N, W), (N, N, E), (E, E, N), (E, E, S)
                , (S, S, E), (S, S, W), (W, W, S), (W, W, N) ]
    (Just np) <- map (f op) jumps
    guard $ np /= op
    return np

basicPawn :: Game -> Position -> [Position]
basicPawn game op@(_, rnk) = do
    let plr = player game
        dir = if plr == White then N else S

        -- Move forward once if vacant.
        simpleMvs = [next game False False dir op]

        -- Move forward twice if yet to move and both are vacant.
        doubleMvs = let doubleHop = next game False False dir op >>= next game False False dir
                    in case (plr, rnk) of
                         (White, 2) -> [doubleHop]
                         (Black, 7) -> [doubleHop]
                         _          -> []

        -- Capture pieces that are E/W of one step foward.
        captMvs = let brd = board game
                      cdirs = if plr == White then [NE, NW] else [SE, SW]
                      capt cdir = case square brd (nextPos cdir op) of
                                    Nothing  -> []
                                    (Just _) -> [next game False True cdir op]

                  in concatMap capt cdirs

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
next game jumping capture d op = do
    case square (board game) op of
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
nextPos d (f, r) = np where np = case d of
                                   N  -> (     f, succ r)
                                   NE -> (succ f, succ r)
                                   E  -> (succ f,      r)
                                   SE -> (succ f, pred r)
                                   S  -> (     f, pred r)
                                   SW -> (pred f, pred r)
                                   W  -> (pred f,      r)
                                   NW -> (pred f, succ r)
