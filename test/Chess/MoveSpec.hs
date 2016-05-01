module Chess.MoveSpec (spec) where

import Test.Hspec

import Control.Monad (liftM)
import Data.Maybe (fromJust, isJust, isNothing)

import Chess.Board
import Chess.Move
import Chess.Game

blackGame :: Game
blackGame = newGame { player = Black }

whiteGame :: Game
whiteGame = newGame { player = White }

spec :: Spec
spec = do
    describe "moves" $ do
        describe "pawns" $ do
            it "should allow white pawns to move twice if not moved" $
                moves (whiteGame { board = mvTwice })
                `shouldMatchList`
                [ Move Basic ('b', 2) ('b', 3)
                , Move Basic ('b', 2) ('b', 4)
                ]

            it "should allow black pawns to move twice if not moved" $
                moves (blackGame { board = mvTwice })
                `shouldMatchList`
                [ Move Basic ('b', 7) ('b', 6)
                , Move Basic ('b', 7) ('b', 5)
                ]

            it "should allow white pawns to only capture diagonally" $
                moves (whiteGame { board = captDiag })
                `shouldMatchList`
                [ Move Basic ('b', 5) ('a', 6)
                , Move Basic ('b', 5) ('c', 6)
                ]

            it "should allow black pawns to only capture diagonally" $
                moves (blackGame { board = captDiag })
                `shouldMatchList`
                [ Move Basic ('a', 6) ('a', 5)
                , Move Basic ('a', 6) ('b', 5)
                , Move Basic ('c', 6) ('c', 5)
                , Move Basic ('c', 6) ('b', 5)
                                              ]

        describe "king in check" $
            it "should only allow moves that stop king being in check" $
                moves (blackGame { board = check })
                `shouldMatchList`
                [ Move Basic ('b', 6) ('a', 5)
                , Move Basic ('b', 6) ('a', 6)
                , Move Basic ('b', 6) ('a', 7)
                , Move Basic ('b', 6) ('c', 5)
                , Move Basic ('b', 6) ('c', 6)
                , Move Basic ('b', 6) ('c', 7)
                ]

        describe "castling" $ do
            it "moving the rook or king should change castling to false" $ do
                let gwhite  = whiteGame { board = castle }
                    gblack  = blackGame { board = castle }
                    mvdgs = [ move gwhite (Move Basic ('a', 1) ('a', 2))
                            , move gwhite (Move Basic ('e', 1) ('e', 2))
                            , move gwhite (Move Basic ('h', 1) ('h', 2))
                            , move gblack (Move Basic ('a', 8) ('a', 7))
                            , move gblack (Move Basic ('e', 8) ('e', 7))
                            , move gblack (Move Basic ('h', 8) ('h', 7))
                            ]
                    plrs = replicate 3 White ++ replicate 3 Black
                mvdgs `shouldSatisfy` all isJust
                zip plrs (map (snd . fromJust) mvdgs) `shouldSatisfy` all (\(p, g) -> not $ castling g p)

            it "should not return castling moves if the squares between the rook and king are filled" $
                moves newGame `shouldSatisfy` all (\mv -> typ mv /= Castling)

            it "should not return castling moves when in check" $
                moves (blackGame { board = castleCheck }) `shouldSatisfy` all (\mv -> typ mv /= Castling)

            it "should not return castling moves if the king can be captured en route" $
                moves (blackGame { board = castlePass }) `shouldSatisfy` all (\mv -> typ mv /= Castling)

            it "should return castling moves when conditions hold" $ do
                moves (whiteGame { board = castle}) `shouldSatisfy` \mvs -> all (`elem` mvs) [ Move Castling ('e', 1) ('a', 1)
                                                                                             , Move Castling ('e', 1) ('h', 1)
                                                                                             ]
                moves (blackGame { board = castle}) `shouldSatisfy` \mvs -> all (`elem` mvs) [ Move Castling ('e', 8) ('a', 8)
                                                                                             , Move Castling ('e', 8) ('h', 8)
                                                                                             ]

            it "should return enPassantMoves when conditions hold" $
                moves (whiteGame { board = enPassantBrd, enPassant = Just ('c', 6) })
                `shouldMatchList`
                [ Move Basic     ('d', 5) ('d', 6)
                , Move EnPassant ('d', 5) ('c', 6)
                ]

    describe "move" $ do
        it "should only allow black player to move own pieces" $
            move (blackGame { board = pawnProm }) (Move Basic ('b', 7) ('b', 8)) `shouldSatisfy` isNothing

        it "should only allow white player to move own pieces" $
            move (whiteGame { board = pawnProm }) (Move Basic ('b', 2) ('b', 1)) `shouldSatisfy` isNothing

        it "should change the game player when playing a white basic move" $
            move whiteGame (Move Basic ('b', 2) ('b', 3))
            `shouldSatisfy`
            (\res -> case liftM (player . snd) res of
                       Just Black -> True
                       _          -> False)

        it "should change the game player when playing a black basic move" $
            move blackGame (Move Basic ('b', 7) ('b', 6))
            `shouldSatisfy`
            (\res -> case liftM (player . snd) res of
                       Just White -> True
                       _          -> False)

        it "should promote black pawn to queen when pawn reaches rank 1" $
            move (blackGame { board = pawnProm }) (Move Basic ('b', 2) ('b', 1))
            `shouldSatisfy`
            (\res -> case res >>= ((`square` ('b', 1)) . board . snd) of
                       (Just (Piece Black Queen)) -> True
                       _                          -> False)

        it "should promote white pawn to queen when pawn reaches rank 8" $
            move (whiteGame { board = pawnProm }) (Move Basic ('b', 7) ('b', 8))
            `shouldSatisfy`
            (\res -> case res >>= ((`square` ('b', 8)) . board . snd) of
                       (Just (Piece White Queen)) -> True
                       _                          -> False)

        it "should change the game player when playing a black castling move" $
            move (blackGame { board = castle }) (Move Castling ('e', 8) ('a', 8))
            `shouldSatisfy`
            (\res -> case liftM (player . snd) res of
                       Just White -> True
                       _          -> False)

        it "should change the game player when playing a white castling move" $
            move (whiteGame { board = castle }) (Move Castling ('e', 1) ('h', 1))
            `shouldSatisfy`
            (\res -> case liftM (player . snd) res of
                       Just Black -> True
                       _          -> False)

        it "should move king to c8 and rook to d8 for black long castling" $
            move (blackGame { board = castle }) (Move Castling ('e', 8) ('a', 8))
            `shouldSatisfy`
            (\res -> case res >>= (`rank` 8) . board . snd of
                       (Just r) -> r == ([Nothing, Nothing, Just $ Piece Black King, Just $ Piece Black Rook] ++ replicate 3 Nothing ++ [Just $ Piece Black Rook])
                       _        -> False)

        it "should move king to g8 and rook to f8 for black short castling" $
            move (blackGame { board = castle }) (Move Castling ('e', 8) ('h', 8))
            `shouldSatisfy`
            (\res -> case res >>= (`rank` 8) . board . snd of
                       (Just r) -> r == (((Just $ Piece Black Rook) : replicate 4 Nothing) ++ [Just $ Piece Black Rook, Just $ Piece Black King, Nothing])
                       _        -> False)

        it "should move king to c1 and rook to d1 for white long castling" $
            move (whiteGame { board = castle }) (Move Castling ('e', 1) ('a', 1))
            `shouldSatisfy`
            (\res -> case res >>= (`rank` 1) . board . snd of
                       (Just r) -> r == ([Nothing, Nothing, Just $ Piece White King, Just $ Piece White Rook] ++ replicate 3 Nothing ++ [Just $ Piece White Rook])
                       _        -> False)

        it "should move king to g1 and rook to f1 for white short castling" $
            move (whiteGame { board = castle }) (Move Castling ('e', 1) ('h', 1))
            `shouldSatisfy`
            (\res -> case res >>= (`rank` 1) . board . snd of
                       (Just r) -> r == (((Just $ Piece White Rook) : replicate 4 Nothing) ++ [Just $ Piece White Rook, Just $ Piece White King, Nothing])
                       _        -> False)

        it "should set enPassant when pawn makes a double move" $
            move whiteGame (Move Basic ('c', 2) ('c', 4))
            `shouldSatisfy`
            (\res -> case res >>= enPassant . snd of
                       Just ('c', 3) -> True
                       _            -> False)

        it "should unset enPassant when basic non-double move is made" $
            let g = whiteGame { board = enPassantBrd, enPassant = Just ('c', 3) }
            in  move g (Move Basic ('d', 2) ('d', 3))
                `shouldSatisfy`
                (\res -> case res >>= enPassant . snd of
                           Nothing -> True
                           _       -> False)

        it "should capture adjacent pawn when en passant move is made" $
            let g = whiteGame { board = enPassantBrd, enPassant = Just ('c', 6) }
            in  move g (Move EnPassant ('d', 5) ('c', 6))
                `shouldSatisfy`
                (\res -> case liftM ((==) enPassantBrdCapt . board . snd) res of
                           (Just b) -> b
                           _        -> False)

mvTwice :: Board
mvTwice = [ replicate 8 Nothing
          , Nothing : (Just $ Piece Black Pawn) : replicate 6 Nothing
          , replicate 8 Nothing
          , replicate 8 Nothing
          , replicate 8 Nothing
          , replicate 8 Nothing
          , Nothing : (Just $ Piece White Pawn) : replicate 6 Nothing
          , replicate 8 Nothing
          ]

captDiag :: Board
captDiag = [ replicate 8 Nothing
           , replicate 8 Nothing
           , replicate 3 (Just $ Piece Black Pawn) ++ replicate 5 Nothing
           , Nothing : (Just $ Piece White Pawn) : replicate 6 Nothing
           , replicate 8 Nothing
           , replicate 8 Nothing
           , replicate 8 Nothing
           , replicate 8 Nothing
           ]

check :: Board
check = [ replicate 8 Nothing
        , replicate 8 Nothing
        , Nothing : (Just $ Piece Black King) : replicate 6 Nothing
        , (Just $ Piece White Pawn) : replicate 7 Nothing
        , replicate 8 Nothing
        , replicate 8 Nothing
        , Nothing : (Just $ Piece White Rook) : replicate 6 Nothing
        , replicate 8 Nothing
        ]

pawnProm :: Board
pawnProm = [ replicate 8 Nothing
           , Nothing : (Just $ Piece White Pawn) : replicate 6 Nothing
           , replicate 8 Nothing
           , replicate 8 Nothing
           , replicate 8 Nothing
           , replicate 8 Nothing
           , Nothing : (Just $ Piece Black Pawn) : replicate 6 Nothing
           , replicate 8 Nothing
           ]

castleCheck :: Board
castleCheck = [ (Just $ Piece Black Rook) : replicate 3 Nothing ++ [Just $ Piece Black King] ++ replicate 2 Nothing ++ [Just $ Piece Black Rook]
              , replicate 8 Nothing
              , replicate 4 Nothing ++ [Just $ Piece White Rook] ++ replicate 3 Nothing
              , replicate 8 Nothing
              , replicate 8 Nothing
              , replicate 8 Nothing
              , replicate 8 Nothing
              , replicate 8 Nothing
              ]

castlePass :: Board
castlePass = [ (Just $ Piece Black Rook) : replicate 3 Nothing ++ [Just $ Piece Black King] ++ replicate 2 Nothing ++ [Just $ Piece Black Rook]
              , replicate 8 Nothing
              , replicate 8 (Just $ Piece White Rook)
              , replicate 8 Nothing
              , replicate 8 Nothing
              , replicate 8 Nothing
              , replicate 8 Nothing
              , replicate 8 Nothing
              ]

castle :: Board
castle = [ (Just $ Piece Black Rook) : replicate 3 Nothing ++ [Just $ Piece Black King] ++ replicate 2 Nothing ++ [Just $ Piece Black Rook]
         , (Just $ Piece Black Pawn) : replicate 6 Nothing ++ [Just $ Piece Black Pawn]
         , replicate 8 Nothing
         , replicate 8 Nothing
         , replicate 8 Nothing
         , replicate 8 Nothing
         , (Just $ Piece White Pawn) : replicate 6 Nothing ++ [Just $ Piece White Pawn]
         , (Just $ Piece White Rook) : replicate 3 Nothing ++ [Just $ Piece White King] ++ replicate 2 Nothing ++ [Just $ Piece White Rook]
         ]

enPassantBrd :: Board
enPassantBrd = [ replicate 8 Nothing
               , replicate 8 Nothing
               , replicate 8 Nothing
               , Nothing : Nothing : (Just $ Piece Black Pawn) : (Just $ Piece White Pawn) : replicate 4 Nothing
               , replicate 8 Nothing
               , replicate 8 Nothing
               , replicate 8 Nothing
               , replicate 8 Nothing
               ]

enPassantBrdCapt :: Board
enPassantBrdCapt = [ replicate 8 Nothing
                   , replicate 8 Nothing
                   , Nothing : Nothing : (Just $ Piece White Pawn) : replicate 5 Nothing
                   , replicate 8 Nothing
                   , replicate 8 Nothing
                   , replicate 8 Nothing
                   , replicate 8 Nothing
                   , replicate 8 Nothing
                   ]
