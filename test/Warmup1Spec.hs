module Warmup1Spec (spec) where

import Test.Hspec
import Warmup1 (sleepIn, diff21, nearHundred)

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
    it "returns 0 for diff21(21)" $
      diff21 21 `shouldBe` 0
    it "returns 2 for diff21(19)" $
      diff21 19 `shouldBe` 2
    it "returns 2 for diff21(22)" $
      diff21 22 `shouldBe` 2
    it "returns 18 for diff21(30)" $
      diff21 30 `shouldBe` 18
    it "returns 21 for diff21(-1)" $
      diff21 (-1) `shouldBe` 22

  describe "Warmup1.nearHundred" $ do
    it "returns True for nearHundred(93)" $
      nearHundred 93 `shouldBe` True
    it "returns False for nearHundred(89)" $
      nearHundred 89 `shouldBe` False
    it "returns False for nearHundred(111)" $
      nearHundred 111 `shouldBe` False
    it "returns True for nearHundred(190)" $
      nearHundred 190 `shouldBe` True
    it "returns False for nearHundred(290)" $
      nearHundred 290 `shouldBe` False