module Chess.Moves
    ( MoveType (..)
    , Move (..)
    , moves
    , move
    ) where

import Chess.Board
import Chess.Game

import qualified Data.List.Safe as SL
import Control.Monad (guard)
import Data.Maybe (isJust)

data MoveType
    = Basic
    | Castling
    deriving (Eq, Show)

-- |'Move' represents a move on a chess 'Board'.
data Move = Move { typ :: MoveType
                 , cur :: Position
                 , new :: Position
                 } deriving (Eq, Show)

-- |'moves' returns a list of possible 'Move's.
moves :: Game -> [Move]
moves game = do
    let stopCheck :: Move -> Bool
        -- 'stopCheck' returns True if the Move gets the current player's king
        -- out of check.
        stopCheck mv = maybe False (not . inCheck . nextPlayer . snd) (move game mv)

    if inCheck game
      then filter stopCheck (basicMoves game)
      else basicMoves game ++ castlingMoves game

-- |'move' plays the given 'Move' and returns a tuple consisting of the captured
-- 'Square', if any, and the new 'Game' state. It promotes 'Pawn's to 'Queen's
-- if they reach the eighth 'Rank'.
move :: Game -> Move -> Maybe (Square, Game)
move game mv = case typ mv of
                 Basic    -> basicMove    game mv
                 Castling -> castlingMove game mv >>= \g -> Just (Nothing, g)

----------------------------------  Internal  ----------------------------------

-- |'basicMove' plays a 'Basic' 'Move' and returns the captured 'Square', if
-- any, and the new 'Game' state. It promotes 'Pawn's to 'Queen's if they reach
-- the eighth 'Rank.
basicMove :: Game -> Move -> Maybe (Square, Game)
basicMove game mv = do
    guard $ typ mv == Basic

    let oldPos = cur mv
        newPos = new mv
        plr    = player game

    (oldSq, rmCur) <- update oldPos Nothing (board game)

    -- Cannot move opponents pieces!
    case oldSq of
        Nothing  -> pure () -- OK
        (Just p) -> guard $ pieceColor p == plr

    -- Promote pawn if pawn reaches eighth rank.
    let updSq = case (newPos, oldSq) of
                  ((_, 8), Just (Piece White Pawn)) -> Just $ Piece White Queen
                  ((_, 1), Just (Piece Black Pawn)) -> Just $ Piece Black Queen
                  _                                 -> oldSq

    -- Set castling to False if Rook or King moved.
    let updGame = if not $ castling game plr
                  then game
                  else case (oldPos, oldSq) of
                         (('a', 1), Just (Piece White Rook)) -> updateCastling White False game
                         (('e', 1), Just (Piece White King)) -> updateCastling White False game
                         (('h', 1), Just (Piece White Rook)) -> updateCastling White False game
                         (('a', 8), Just (Piece Black Rook)) -> updateCastling Black False game
                         (('e', 8), Just (Piece Black King)) -> updateCastling Black False game
                         (('h', 8), Just (Piece Black Rook)) -> updateCastling Black False game
                         _                                   -> game

    (newSq, updBrd) <- update newPos updSq rmCur
    return (newSq, nextPlayer $ updateBoard updGame updBrd)

-- |'castlingMove' plays a 'Castling' 'Move' and returns the new 'Game' state.
castlingMove :: Game -> Move -> Maybe Game
castlingMove game mv = do
    guard $ typ mv == Castling

    let kingPos = cur mv
        rookPos = new mv

    -- Move King.
    (updKingPos, updRookPos) <- case rookPos of
                                  ('a', ri) -> Just (('c', ri), ('d', ri))
                                  ('h', ri) -> Just (('g', ri), ('f', ri))
                                  _         -> Nothing

    (Nothing, updKing) <- basicMove game (Move Basic kingPos updKingPos)
    (Nothing, updRook) <- basicMove (nextPlayer updKing) (Move Basic rookPos updRookPos)

    return $ updateCastling (player game) False updRook

-- |'inCheck' returns True if the current player's king is in check.
inCheck :: Game -> Bool
inCheck game = any captureKing (basicMoves $ nextPlayer game)
    where captureKing mv = let sq = square (board game) (new mv)
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

-- |'basicMoves' returns a list of basic 'Move's for a given Game.
-- It does not check for castling, en passant or any issues regarding check.
basicMoves :: Game -> [Move]
basicMoves game = do
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
    return $ Move Basic op np

-- |'castlingMoves' returns a list of 'Castling' 'Move's for a given Game.
castlingMoves :: Game -> [Move]
castlingMoves game = do
    let plr = player game
    guard $ castling game plr

    let op       = if plr == White then Black else White
        opMvPoss = map new $ moves (nextPlayer $ updateCastling op False game)

        ri     = if plr == White then 1 else 8
        actRnk = case rank (board game) ri of
                   Nothing  -> []
                   (Just r) -> r
        -- [(Rook File, Section of Rank)]. If we've got this far then castling
        -- must be True thus neither the Rook or King have moved. Therefore we
        -- just check if there is Nothing between the Rooks and King.
        rkRnk = [ ('h', (Just $ Piece plr King) : replicate 2 Nothing ++ [Just $ Piece plr Rook])
                , ('a', (Just $ Piece plr Rook) : replicate 3 Nothing ++ [Just $ Piece plr King])
                ]

    -- Actual Rank must include Section.
    (rk, rnk) <- rkRnk
    guard $ rnk `SL.isInfixOf` actRnk

    -- Squares must not be capturable.
    let sqFiles = if rk == 'a' then ['a'..'e'] else ['e'..'h']
    guard $ all (`notElem` opMvPoss) [(f, ri) | f <- sqFiles]

    return $ Move Castling ('e', ri) (rk, ri)

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
