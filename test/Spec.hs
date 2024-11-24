module Main (main) where

import Test.Hspec
import Warmup1 (sleepIn)

main :: IO ()
main = hspec $ do
  describe "Warmup1.sleepIn" $ do
    it "allows sleeping in on weekdays if not a weekday and not a vacation" $
      sleepIn False False `shouldBe` True

    it "does not allow sleeping in on weekdays if it is a weekday and not a vacation" $
      sleepIn True False `shouldBe` False

    it "allows sleeping in if it is a vacation, regardless of weekday" $ do
      sleepIn False True `shouldBe` True
      sleepIn True True `shouldBe` True