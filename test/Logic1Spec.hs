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