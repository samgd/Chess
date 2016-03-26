module Chess.MoveSpec (spec) where

import Test.Hspec
import Data.Maybe (isNothing)

import Chess.Board
import Chess.Moves
import Chess.Game

spec :: Spec
spec = do
    describe "moves" $ do
        describe "pawns" $ do
            it "should allow white pawns to move twice if not moved" $
                moves (mkGame White mvTwice) `shouldMatchList` [ Move ('b', 2) ('b', 3)
                                                               , Move ('b', 2) ('b', 4)
                                                               ]

            it "should allow black pawns to move twice if not moved" $
                moves (mkGame Black mvTwice) `shouldMatchList` [ Move ('b', 7) ('b', 6)
                                                               , Move ('b', 7) ('b', 5)
                                                               ]

            it "should allow white pawns to only capture diagonally" $
                moves (mkGame White captDiag) `shouldMatchList` [ Move ('b', 5) ('a', 6)
                                                                , Move ('b', 5) ('c', 6)
                                                                ]

            it "should allow black pawns to only capture diagonally" $
                moves (mkGame Black captDiag) `shouldMatchList` [ Move ('a', 6) ('a', 5)
                                                                , Move ('a', 6) ('b', 5)
                                                                , Move ('c', 6) ('c', 5)
                                                                , Move ('c', 6) ('b', 5)
                                                                ]

        describe "king in check" $
           it "should only allow moves that stop king being in check" $
                moves (mkGame Black check) `shouldMatchList` [ Move ('b', 6) ('a', 5)
                                                             , Move ('b', 6) ('a', 6)
                                                             , Move ('b', 6) ('a', 7)
                                                             , Move ('b', 6) ('c', 5)
                                                             , Move ('b', 6) ('c', 6)
                                                             , Move ('b', 6) ('c', 7)
                                                             ]

    describe "move" $ do
        it "should only allow white player to move own pieces" $
            move (mkGame White pawnProm) (Move ('b', 2) ('b', 1)) `shouldSatisfy` isNothing

        it "should only allow black player to move own pieces" $
            move (mkGame Black pawnProm) (Move ('b', 7) ('b', 8)) `shouldSatisfy` isNothing

        it "should promote white pawn to queen when pawn reaches rank 8" $
            move (mkGame White pawnProm) (Move ('b', 7) ('b', 8))
            `shouldSatisfy`
            (\res -> case res >>= ((`square` ('b', 8)) . board . snd) of
                       (Just (Piece White Queen)) -> True
                       _                          -> False)

        it "should promote black pawn to queen when pawn reaches rank 1" $
            move (mkGame Black pawnProm) (Move ('b', 2) ('b', 1))
            `shouldSatisfy`
            (\res -> case res >>= ((`square` ('b', 1)) . board . snd) of
                       (Just (Piece Black Queen)) -> True
                       _                          -> False)

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
