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