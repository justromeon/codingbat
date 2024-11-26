module Warmup1Spec (spec) where

import Test.Hspec
import Warmup1 (sleepIn, diff21)

spec :: Spec
spec = describe "Warmup1.sleepIn" $ do
  it "allows sleeping in on weekdays if not a weekday and not a vacation" $
    sleepIn False False `shouldBe` True

  it "does not allow sleeping in on weekdays if it is a weekday and not a vacation" $
    sleepIn True False `shouldBe` False

  it "allows sleeping in if it is a vacation, regardless of weekday" $ do
    sleepIn False True `shouldBe` True
    sleepIn True True `shouldBe` True

  describe "Warmup1.diff21" $ do
    it "returns 2 for diff21(19)" $
      diff21 19 `shouldBe` 2
    it "returns 11 for diff21(10)" $
      diff21 10 `shouldBe` 11
    it "returns 0 for diff21(21)" $
      diff21 21 `shouldBe` 0
    it "returns 2 for diff21(22)" $
      diff21 22 `shouldBe` 2
    it "returns 8 for diff21(25)" $
      diff21 25 `shouldBe` 8
    it "returns 18 for diff21(30)" $
      diff21 30 `shouldBe` 18
    it "returns 21 for diff21(0)" $
      diff21 0 `shouldBe` 21
    it "returns 20 for diff21(1)" $
      diff21 1 `shouldBe` 20
    it "returns 19 for diff21(2)" $
      diff21 2 `shouldBe` 19
    it "returns 22 for diff21(-1)" $
      diff21 (-1) `shouldBe` 22
    it "returns 23 for diff21(-2)" $
      diff21 (-2) `shouldBe` 23
    it "returns 58 for diff21(50)" $
      diff21 50 `shouldBe` 58