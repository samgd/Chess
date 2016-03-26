module Chess.BoardSpec where

import qualified Data.List.Safe as SL
import Data.Maybe (isNothing)
import Test.Hspec
import Test.QuickCheck

import Chess.Board

spec :: Spec
spec = do
    describe "validPosition" $ do
        it "returns True when given a valid position" $
            all validPosition [(f, r) | f <- ['a'..'h'], r <- [1..8]] `shouldBe` True

        it "returns False when given an invalid position" $ property $
            \p@(f, r) -> ((f `notElem` ['a'..'h']) || (r `notElem` [1..8])) ==> not $ validPosition p

    describe "fileIndex" $ do
        it "returns correct index when given a valid file" $
            [fileIndex f | f <- ['a'..'h']] `shouldBe` map Just [0..7]

        it "returns Nothing when given an invalid file" $ property $
            \f -> (f `notElem` ['a'..'h']) ==> isNothing $ fileIndex f

    describe "rankIndex" $ do
        it "returns correct index when given a valid rank" $
            [rankIndex r | r <- [1..8]] `shouldBe` map Just [7, 6..0]

        it "returns Nothing when given an invalid rank" $ property $
            \r -> (r `notElem` [1..8]) ==> isNothing $ rankIndex r

    describe "square" $
        it "returns correct square for a given position" $
            map (square board) (zip ['a'..'h'] [8,7..1]) `shouldBe` sqs

    describe "rank" $ do
        it "returns Nothing when given an invalid index" $ property $
            \i -> (i `notElem` [1..8]) ==> isNothing $ rank board i

        it "returns correct rank when given a valid index" $
            [rank board r | r <- [1..8]] `shouldBe` map Just (reverse board)

    describe "file" $ do
        it "returns Nothing when given an invalid file" $ property $
            \c -> (c `notElem` ['a'..'h']) ==> isNothing $ file board c

        it "returns correct file when given a valid index" $
            [file board f | f <- ['a'..'h']] `shouldBe` map Just (SL.transpose board)


sqs :: [Square]
sqs = [ Just $ Piece White King
      , Just $ Piece White Queen
      , Just $ Piece White Rook
      , Just $ Piece White Bishop
      , Just $ Piece Black Bishop
      , Just $ Piece Black Rook
      , Just $ Piece Black Queen
      , Just $ Piece Black King
      ]

board :: Board
board = [                        [sqs !! 0] ++ replicate 7 Nothing
        , replicate 1 Nothing ++ [sqs !! 1] ++ replicate 6 Nothing
        , replicate 2 Nothing ++ [sqs !! 2] ++ replicate 5 Nothing
        , replicate 3 Nothing ++ [sqs !! 3] ++ replicate 4 Nothing
        , replicate 4 Nothing ++ [sqs !! 4] ++ replicate 3 Nothing
        , replicate 5 Nothing ++ [sqs !! 5] ++ replicate 2 Nothing
        , replicate 6 Nothing ++ [sqs !! 6] ++ replicate 1 Nothing
        , replicate 7 Nothing ++ [sqs !! 7]
        ]

