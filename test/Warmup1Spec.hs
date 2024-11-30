module Warmup1Spec (spec) where

import Test.Hspec
import Warmup1 (sleepIn, diff21, nearHundred, missingChar, monkeyTrouble)

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

  describe "Warmup1.missingChar" $ do
    it "removes character at index 1 from 'kitten'" $
      missingChar "kitten" 1 `shouldBe` "ktten"
    it "removes first character from 'Hi'" $
      missingChar "Hi" 0 `shouldBe` "i"
    it "removes last character from 'Hi'" $
      missingChar "Hi" 1 `shouldBe` "H"
    it "removes character at index 2 from 'code'" $
      missingChar "code" 2 `shouldBe` "coe"
    it "removes last character from 'chocolate'" $
      missingChar "chocolate" 8 `shouldBe` "chocolat"
  
  describe "Warmup1.monkeyTrouble" $ do
    it "returns True when both monkeys are smiling" $
      monkeyTrouble True True `shouldBe` True
    it "returns True when neither monkey is smiling" $
      monkeyTrouble False False `shouldBe` True
    it "returns False when only first monkey is smiling" $
      monkeyTrouble True False `shouldBe` False
    it "returns False when only second monkey is smiling" $
      monkeyTrouble False True `shouldBe` False