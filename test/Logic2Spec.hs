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

  describe "Logic2.noTeenSum" $ do
    context "when there are no teens to fix" $ do
      it "returns the sum of the numbers" $ do
        noTeenSum 1 2 3 `shouldBe` 6
    context "when there are teens to fix" $ do
      it "returns the sum of the numbers, with teens fixed" $ do
        noTeenSum 2 13 1 `shouldBe` 3
        noTeenSum 2 1 14 `shouldBe` 3
        noTeenSum 2 1 15 `shouldBe` 18
        noTeenSum 17 1 2 `shouldBe` 3
        noTeenSum 16 17 18 `shouldBe` 16
        noTeenSum 17 18 19 `shouldBe` 0
        noTeenSum 15 16 1 `shouldBe` 32
        noTeenSum 15 15 19 `shouldBe` 30
        noTeenSum 5 17 18 `shouldBe` 5
        noTeenSum 17 18 16 `shouldBe` 16
        noTeenSum 17 19 18 `shouldBe` 0