module Logic2Spec (spec) where

import Test.Hspec
import Logic2

spec :: Spec
spec = do
  describe "Logic2.makeBricks" $ do
    context "when it is possible to make the goal length with the given bricks" $ do
      it "returns True" $ do
        makeBricks 3 1 8 `shouldBe` True
        makeBricks 3 2 10 `shouldBe` True
        makeBricks 6 1 11 `shouldBe` True
        makeBricks 1 4 11 `shouldBe` True
        makeBricks 0 3 10 `shouldBe` True
        makeBricks 43 1 46 `shouldBe` True
        makeBricks 40 2 47 `shouldBe` True
        makeBricks 0 2 10 `shouldBe` True
        makeBricks 1000000 1000 1000100 `shouldBe` True
        makeBricks 20 0 19 `shouldBe` True
        makeBricks 20 4 39 `shouldBe` True

    context "when it is not possible to make the goal length with the given bricks" $ do
      it "returns False" $ do
        makeBricks 3 1 9 `shouldBe` False
        makeBricks 3 2 9 `shouldBe` False
        makeBricks 6 0 11 `shouldBe` False
        makeBricks 1 4 12 `shouldBe` False
        makeBricks 1 1 7 `shouldBe` False
        makeBricks 7 1 13 `shouldBe` False
        makeBricks 40 1 46 `shouldBe` False
        makeBricks 40 2 52 `shouldBe` False
        makeBricks 22 2 33 `shouldBe` False
        makeBricks 2 1000000 100003 `shouldBe` False
        makeBricks 20 0 21 `shouldBe` False
        makeBricks 20 4 51 `shouldBe` False