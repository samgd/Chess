module Chess.MoveSpec (spec) where

import Test.Hspec

import Control.Monad (liftM)
import Data.Maybe (fromJust, isJust, isNothing)

import Chess.Board
import Chess.Moves
import Chess.Game

spec :: Spec
spec = do
    describe "moves" $ do
        describe "pawns" $ do
            it "should allow white pawns to move twice if not moved" $
                moves (mkGame White mvTwice) `shouldMatchList` [ Move Basic ('b', 2) ('b', 3)
                                                               , Move Basic ('b', 2) ('b', 4)
                                                               ]

            it "should allow black pawns to move twice if not moved" $
                moves (mkGame Black mvTwice) `shouldMatchList` [ Move Basic ('b', 7) ('b', 6)
                                                               , Move Basic ('b', 7) ('b', 5)
                                                               ]

            it "should allow white pawns to only capture diagonally" $
                moves (mkGame White captDiag) `shouldMatchList` [ Move Basic ('b', 5) ('a', 6)
                                                                , Move Basic ('b', 5) ('c', 6)
                                                                ]

            it "should allow black pawns to only capture diagonally" $
                moves (mkGame Black captDiag) `shouldMatchList` [ Move Basic ('a', 6) ('a', 5)
                                                                , Move Basic ('a', 6) ('b', 5)
                                                                , Move Basic ('c', 6) ('c', 5)
                                                                , Move Basic ('c', 6) ('b', 5)
                                                                ]

        describe "king in check" $
            it "should only allow moves that stop king being in check" $
                moves (mkGame Black check) `shouldMatchList` [ Move Basic ('b', 6) ('a', 5)
                                                             , Move Basic ('b', 6) ('a', 6)
                                                             , Move Basic ('b', 6) ('a', 7)
                                                             , Move Basic ('b', 6) ('c', 5)
                                                             , Move Basic ('b', 6) ('c', 6)
                                                             , Move Basic ('b', 6) ('c', 7)
                                                             ]
        describe "castling" $ do
            it "moving the rook or king should change castling to false" $ do
                let mvdgs = [ move (mkGame White castle) (Move Basic ('a', 1) ('a', 2))
                            , move (mkGame White castle) (Move Basic ('e', 1) ('e', 2))
                            , move (mkGame White castle) (Move Basic ('h', 1) ('h', 2))
                            , move (mkGame Black castle) (Move Basic ('a', 8) ('a', 7))
                            , move (mkGame Black castle) (Move Basic ('e', 8) ('e', 7))
                            , move (mkGame Black castle) (Move Basic ('h', 8) ('h', 7))
                            ]
                    plrs = replicate 3 White ++ replicate 3 Black
                mvdgs `shouldSatisfy` all isJust
                zip plrs (map (snd . fromJust) mvdgs) `shouldSatisfy` all (\(p, g) -> not $ castling g p)

            it "should not return castling moves if the squares between the rook and king are filled" $
                moves newGame `shouldSatisfy` all (\mv -> typ mv /= Castling)

            it "should not return castling moves when in check" $
                moves (mkGame Black castleCheck) `shouldSatisfy` all (\mv -> typ mv /= Castling)

            it "should not return castling moves if the king can be captured en route" $
                moves (mkGame Black castlePass) `shouldSatisfy` all (\mv -> typ mv /= Castling)

            it "should return castling moves when conditions hold" $ do
                moves (mkGame White castle) `shouldSatisfy` \mvs -> all (`elem` mvs) [ Move Castling ('e', 1) ('a', 1)
                                                                                     , Move Castling ('e', 1) ('h', 1)
                                                                                     ]
                moves (mkGame Black castle) `shouldSatisfy` \mvs -> all (`elem` mvs) [ Move Castling ('e', 8) ('a', 8)
                                                                                     , Move Castling ('e', 8) ('h', 8)
                                                                                     ]

    describe "move" $ do
        it "should only allow black player to move own pieces" $
            move (mkGame Black pawnProm) (Move Basic ('b', 7) ('b', 8)) `shouldSatisfy` isNothing

        it "should only allow white player to move own pieces" $
            move (mkGame White pawnProm) (Move Basic ('b', 2) ('b', 1)) `shouldSatisfy` isNothing

        it "should change the game player when playing a white basic move" $
            move (mkGame White initial) (Move Basic ('b', 2) ('b', 3))
            `shouldSatisfy`
            (\res -> case liftM (player . snd) res of
                       Just Black -> True
                       _          -> False)

        it "should change the game player when playing a black basic move" $
            move (mkGame Black initial) (Move Basic ('b', 7) ('b', 6))
            `shouldSatisfy`
            (\res -> case liftM (player . snd) res of
                       Just White -> True
                       _          -> False)

        it "should promote black pawn to queen when pawn reaches rank 1" $
            move (mkGame Black pawnProm) (Move Basic ('b', 2) ('b', 1))
            `shouldSatisfy`
            (\res -> case res >>= ((`square` ('b', 1)) . board . snd) of
                       (Just (Piece Black Queen)) -> True
                       _                          -> False)

        it "should promote white pawn to queen when pawn reaches rank 8" $
            move (mkGame White pawnProm) (Move Basic ('b', 7) ('b', 8))
            `shouldSatisfy`
            (\res -> case res >>= ((`square` ('b', 8)) . board . snd) of
                       (Just (Piece White Queen)) -> True
                       _                          -> False)

        it "should change the game player when playing a black castling move" $
            move (mkGame Black castle) (Move Castling ('e', 8) ('a', 8))
            `shouldSatisfy`
            (\res -> case liftM (player . snd) res of
                       Just White -> True
                       _          -> False)

        it "should change the game player when playing a white castling move" $
            move (mkGame White castle) (Move Castling ('e', 1) ('h', 1))
            `shouldSatisfy`
            (\res -> case liftM (player . snd) res of
                       Just Black -> True
                       _          -> False)

        it "should move king to c8 and rook to d8 for black long castling" $
            move (mkGame Black castle) (Move Castling ('e', 8) ('a', 8))
            `shouldSatisfy`
            (\res -> case res >>= (`rank` 8) . board . snd of
                       (Just r) -> r == ([Nothing, Nothing, Just $ Piece Black King, Just $ Piece Black Rook] ++ replicate 3 Nothing ++ [Just $ Piece Black Rook])
                       _        -> False)

        it "should move king to g8 and rook to f8 for black short castling" $
            move (mkGame Black castle) (Move Castling ('e', 8) ('h', 8))
            `shouldSatisfy`
            (\res -> case res >>= (`rank` 8) . board . snd of
                       (Just r) -> r == (((Just $ Piece Black Rook) : replicate 4 Nothing) ++ [Just $ Piece Black Rook, Just $ Piece Black King, Nothing])
                       _        -> False)

        it "should move king to c1 and rook to d1 for white long castling" $
            move (mkGame White castle) (Move Castling ('e', 1) ('a', 1))
            `shouldSatisfy`
            (\res -> case res >>= (`rank` 1) . board . snd of
                       (Just r) -> r == ([Nothing, Nothing, Just $ Piece White King, Just $ Piece White Rook] ++ replicate 3 Nothing ++ [Just $ Piece White Rook])
                       _        -> False)

        it "should move king to g1 and rook to f1 for white short castling" $
            move (mkGame White castle) (Move Castling ('e', 1) ('h', 1))
            `shouldSatisfy`
            (\res -> case res >>= (`rank` 1) . board . snd of
                       (Just r) -> r == (((Just $ Piece White Rook) : replicate 4 Nothing) ++ [Just $ Piece White Rook, Just $ Piece White King, Nothing])
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

