module Warmup1Spec (spec) where

import Test.Hspec
import Warmup1 ( sleepIn, diff21, nearHundred, missingChar, monkeyTrouble, parrotTrouble
               , posNeg, frontBack, sumDouble, makes10, notString
               )

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
    it "returns absolute difference when number equals 21" $
      diff21 21 `shouldBe` 0
    it "returns absolute difference when number is less than 21" $
      diff21 19 `shouldBe` 2
    it "returns double the absolute difference when number is over 21" $
      diff21 22 `shouldBe` 2
    it "returns double the absolute difference for larger numbers over 21" $
      diff21 30 `shouldBe` 18
    it "returns absolute difference for negative numbers" $
      diff21 (-1) `shouldBe` 22

  describe "Warmup1.nearHundred" $ do
    it "returns True when number is within 10 of 100" $
      nearHundred 93 `shouldBe` True
    it "returns False when number is not within 10 of 100" $
      nearHundred 89 `shouldBe` False
    it "returns False when number is just beyond range of 100" $
      nearHundred 111 `shouldBe` False
    it "returns True when number is within 10 of 200" $
      nearHundred 190 `shouldBe` True
    it "returns False when number is not within 10 of 100 or 200" $
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
  
  describe "Warmup1.parrotTrouble" $ do
    it "returns True when talking before 7" $
      parrotTrouble True 6 `shouldBe` True
    it "returns False when not talking before 7" $
      parrotTrouble False 6 `shouldBe` False
    it "returns True when talking after 20" $
      parrotTrouble True 21 `shouldBe` True
    it "returns False when not talking after 20" $
      parrotTrouble False 21 `shouldBe` False
    it "returns False when talking during normal hours" $
      parrotTrouble True 20 `shouldBe` False

  describe "Warmup1.posNeg" $ do
    it "returns True when one number positive, one negative, and flag is False" $
      posNeg 1 (-1) False `shouldBe` True
    it "returns True when both numbers negative and flag is True" $
      posNeg (-4) (-5) True `shouldBe` True
    it "returns False when both numbers negative and flag is False" $
      posNeg (-4) (-5) False `shouldBe` False
    it "returns True when one negative, one positive regardless of order" $
      posNeg (-4) 5 False `shouldBe` True
    it "returns False when flag is True but only one number is negative" $
      posNeg (-4) 5 True `shouldBe` False

  describe "Warmup1.frontBack" $ do
    it "swaps first and last char of multi-char string" $
      frontBack "code" `shouldBe` "eodc"
    it "handles single character string" $
      frontBack "a" `shouldBe` "a"
    it "handles two character string" $
      frontBack "ab" `shouldBe` "ba"
    it "handles empty string" $
      frontBack "" `shouldBe` ""
    it "handles longer string with uppercase" $
      frontBack "Chocolate" `shouldBe` "ehocolatC"
  
  describe "Warmup1.sumDouble" $ do
    it "returns sum when inputs are different" $
      sumDouble 1 2 `shouldBe` 3
    it "returns double sum when inputs are same" $
      sumDouble 2 2 `shouldBe` 8
    it "handles negative numbers" $
      sumDouble (-1) 0 `shouldBe` (-1)
    it "handles zero inputs" $
      sumDouble 0 0 `shouldBe` 0
    it "handles larger numbers" $
      sumDouble 3 3 `shouldBe` 12
    
  describe "Warmup1.makes10" $ do
    it "returns True when one number is 10" $
      makes10 10 1 `shouldBe` True
    it "returns True when sum is 10" $
      makes10 8 2 `shouldBe` True
    it "returns False when neither condition met" $
      makes10 8 3 `shouldBe` False
    it "returns True when both numbers are 10" $
      makes10 10 10 `shouldBe` True
    it "returns True with negative numbers summing to 10" $
      makes10 12 (-2) `shouldBe` True
  
  describe "Warmup1.notString" $ do
    it "returns 'not candy' when input is 'candy'" $
      notString "candy" `shouldBe` "not candy"
    it "returns 'not x' when input is 'x'" $
      notString "x" `shouldBe` "not x"
    it "returns 'not bad' when input is 'not bad'" $
      notString "not bad" `shouldBe` "not bad"
    it "returns 'not bad' when input is 'bad'" $
      notString "bad" `shouldBe` "not bad"
    it "returns 'not' when input is 'not'" $
      notString "not" `shouldBe` "not"
    it "returns 'not is not' when input is 'is not'" $
      notString "is not" `shouldBe` "not is not"
    it "returns 'not no' when input is 'no'" $
      notString "no" `shouldBe` "not no"