module Chess.MoveSpec where

import Test.Hspec
import Test.QuickCheck

import Chess.Board
import Chess.Moves
import Chess.Game

spec :: Spec
spec = do
    describe "moves" $ do
        describe "pawns" $ do
            it "should allow white pawns to move twice if not moved" $
                moves (Game White mvTwice) `shouldMatchList` [ Move ('b', 2) ('b', 3)
                                                             , Move ('b', 2) ('b', 4)
                                                             ]

            it "should allow black pawns to move twice if not moved" $
                moves (Game Black mvTwice) `shouldMatchList` [ Move ('b', 7) ('b', 6)
                                                             , Move ('b', 7) ('b', 5)
                                                             ]

            it "should allow white pawns to only capture diagonally" $
                moves (Game White captDiag) `shouldMatchList` [ Move ('b', 5) ('a', 6)
                                                              , Move ('b', 5) ('c', 6)
                                                              ]

            it "should allow black pawns to only capture diagonally" $
                moves (Game Black captDiag) `shouldMatchList` [ Move ('a', 6) ('a', 5)
                                                              , Move ('a', 6) ('b', 5)
                                                              , Move ('c', 6) ('c', 5)
                                                              , Move ('c', 6) ('b', 5)
                                                              ]

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
