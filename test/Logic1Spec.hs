module Logic1Spec (spec) where

import Test.Hspec
import Logic1

spec :: Spec
spec = do
  describe "Logic1.cigarParty" $ do
    context "when it's a weekday" $ do
      it "returns True for exactly 30 cigars on a weekday" $
        cigarParty 30 False `shouldBe` True
      it "returns True for exactly 40 cigars on a weekday" $
        cigarParty 40 False `shouldBe` True
      it "returns False for more than 40 cigars on a weekday" $
        cigarParty 41 False `shouldBe` False
      it "returns False for less than 30 cigars on a weekday" $
        cigarParty 29 False `shouldBe` False

    context "when it's a weekend" $ do
      it "returns True for exactly 30 cigars on a weekend" $
        cigarParty 30 True `shouldBe` True
      it "returns True for more than 40 cigars on a weekend" $
        cigarParty 60 True `shouldBe` True
      it "returns False for less than 30 cigars on a weekend" $
        cigarParty 29 True `shouldBe` False
  
  describe "Logic1.caughtSpeeding" $ do
    it "handles normal day speed thresholds" $ do
      caughtSpeeding 60 False `shouldBe` 0
      caughtSpeeding 65 False `shouldBe` 1
      caughtSpeeding 85 False `shouldBe` 2
    
    it "handles birthday speed thresholds" $ do
      caughtSpeeding 65 True `shouldBe` 0
      caughtSpeeding 85 True `shouldBe` 1
      caughtSpeeding 90 True `shouldBe` 2
  
  describe "love6" $ do
    it "returns True when one number is exactly 6" $ do
      love6 6 12 `shouldBe` True
      love6 9 6 `shouldBe` True
    it "returns True when numbers sum to 6" $ do
      love6 1 5 `shouldBe` True
      love6 3 3 `shouldBe` True
    it "returns True when numbers differ by 6" $ do
      love6 0 6 `shouldBe` True
      love6 12 6 `shouldBe` True
    it "returns False for other combinations" $ do
      love6 4 7 `shouldBe` False
      love6 2 9 `shouldBe` False
      love6 5 8 `shouldBe` False
  
  describe "Logic1.dateFashion" $ do
    it "returns 2 when one person is very stylish (8+) and other is not unstylish" $ do
      dateFashion 5 10 `shouldBe` 2
      dateFashion 9 9 `shouldBe` 2
    it "returns 0 when either person is unstylish (2 or less)" $ do
      dateFashion 5 2 `shouldBe` 0
      dateFashion 10 2 `shouldBe` 0
      dateFashion 2 9 `shouldBe` 0
    it "returns 1 for average stylishness (3-7 for both)" $ do
      dateFashion 5 5 `shouldBe` 1
      dateFashion 3 7 `shouldBe` 1

  describe "Logic1.sortaSum" $ do
    it "returns 20 when sum is between 10 and 19 inclusive" $ do
      sortaSum 9 4 `shouldBe` 20
      sortaSum 4 6 `shouldBe` 20
      sortaSum 14 6 `shouldBe` 20
    it "returns actual sum when sum is less than 10" $ do
      sortaSum 3 4 `shouldBe` 7
      sortaSum 4 5 `shouldBe` 9
    it "returns actual sum when sum is greater than 19" $ do
      sortaSum 10 11 `shouldBe` 21
      sortaSum 14 7 `shouldBe` 21
    it "handles negative numbers correctly" $ do
      sortaSum 12 (-3) `shouldBe` 9
      sortaSum (-3) 12 `shouldBe` 9

  describe "Logic1.in1to10" $ do
    context "when outsideMode is False" $ do
      it "returns True when n is within [1, 10]" $ do
        in1to10 5 False `shouldBe` True
        in1to10 10 False `shouldBe` True
        in1to10 1 False `shouldBe` True
        in1to10 9 False `shouldBe` True
      it "returns False when n is outside [1, 10]" $ do
        in1to10 11 False `shouldBe` False
        in1to10 0 False `shouldBe` False
        in1to10 (-1) False `shouldBe` False
        in1to10 99 False `shouldBe` False
    context "when outsideMode is True" $ do
      it "returns True when n is outside (1, 10)" $ do
        in1to10 11 True `shouldBe` True
        in1to10 0 True `shouldBe` True
        in1to10 (-1) True `shouldBe` True
        in1to10 1 True `shouldBe` True
        in1to10 10 True `shouldBe` True
        in1to10 (-99) True `shouldBe` True
      it "returns False when n is within (1, 10)" $ do
        in1to10 9 True `shouldBe` False