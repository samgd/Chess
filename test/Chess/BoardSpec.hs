module Chess.BoardSpec where

import Data.Char (chr)
import Test.Hspec

import Chess.Board

spec :: Spec
spec = do
    describe "validPosition" $ do
        it "returns True when given a valid position" $ 
            all validPosition [(f, r) | f <- ['a', 'h'], r <- [1, 8]] `shouldBe` True           

        it "returns False when given an invalid position" $
            any validPosition [(f, r) | f <- [chr 0..chr 255], r <- [-100, 100], not $ r `elem` [1..8]] `shouldBe` False

    describe "fileIndex" $ do
        it "returns correct index when given a valid file" $
            [fileIndex f | f <- ['a'..'h']] `shouldBe` map Just [0..7] 
            
        it "returns Nothing when given an invalid file" $
            all (== Nothing) [fileIndex f | f <- [chr 0..chr 255], not $ f `elem` ['a'..'h']] `shouldBe` True

    describe "rankIndex" $ do
        it "returns correct index when given a valid rank" $
            [rankIndex r | r <- [1..8]] `shouldBe` map Just [7, 6..0]

        it "returns Nothing when given an invalid rank" $
            all (== Nothing) [rankIndex r | r <- [-100, 100], not $ r `elem` [1..8]] `shouldBe` True

