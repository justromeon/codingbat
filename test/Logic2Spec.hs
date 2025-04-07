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

  describe "Logic2.makeChocolate" $ do
    context "when goal can be reached using big chocolates" $ do
      it "should return the correct number of small chocolates" $ do
        makeChocolate 4 1 9 `shouldBe` 4
        makeChocolate 6 2 7 `shouldBe` 2
        makeChocolate 5 4 9 `shouldBe` 4
        makeChocolate 1 2 6 `shouldBe` 1
        makeChocolate 6 1 10 `shouldBe` 5
        makeChocolate 6 2 12 `shouldBe` 2
        makeChocolate 60 100 550 `shouldBe` 50
    context "when goal cannot be reached" $ do
      it "should return -1" $ do
        makeChocolate 4 1 10 `shouldBe` -1
        makeChocolate 1 2 7 `shouldBe` -1
        makeChocolate 6 1 12 `shouldBe` -1
        makeChocolate 7 1 13 `shouldBe` -1
    context "when goal can be reached using only small chocolates" $ do
      it "should return the goal" $ do
        makeChocolate 4 1 4 `shouldBe` 4
    context "when goal is a multiple of 5 and small chocolates are not needed" $ do
      it "should return 0" $ do
        makeChocolate 4 1 5 `shouldBe` 0
        makeChocolate 1 2 5 `shouldBe` 0

  describe "Logic2.loneSum" $ do
    context "when all numbers are distinct" $ do
      it "should return the sum of all numbers" $ do
        loneSum 1 2 3 `shouldBe` 6
        loneSum 4 2 3 `shouldBe` 9
        loneSum 2 9 3 `shouldBe` 14
    context "when two numbers are the same" $ do
      it "should return the lone number" $ do
        loneSum 3 2 3 `shouldBe` 2
        loneSum 9 2 2 `shouldBe` 9
        loneSum 2 2 9 `shouldBe` 9
        loneSum 2 9 2 `shouldBe` 9
        loneSum 1 3 1 `shouldBe` 3
    context "when all numbers are the same" $ do
      it "should return 0" $ do
        loneSum 3 3 3 `shouldBe` 0

  describe "Logic2.roundSum" $ do
    context "when all numbers are closer to the lower multiple of 10" $ do
      it "should round down correctly" $ do
        roundSum 12 13 14 `shouldBe` 30
        roundSum 4 4 4 `shouldBe` 0
    context "when some numbers are closer to the higher multiple of 10" $ do
      it "should round appropriately" $ do
        roundSum 16 17 18 `shouldBe` 60
        roundSum 9 4 4 `shouldBe` 10
        roundSum 0 9 0 `shouldBe` 10
        roundSum 10 10 19 `shouldBe` 40
        roundSum 23 11 26 `shouldBe` 60
        roundSum 23 24 29 `shouldBe` 70
        roundSum 11 24 36 `shouldBe` 70
        roundSum 14 12 26 `shouldBe` 50
    context "when numbers are exactly in the middle" $ do
      it "should round up" $ do
        roundSum 25 24 25 `shouldBe` 80
    context "with larger numbers" $ do
      it "should handle larger inputs correctly" $ do
        roundSum 20 30 40 `shouldBe` 90
        roundSum 45 21 30 `shouldBe` 100
        roundSum 24 36 32 `shouldBe` 90
        roundSum 12 10 24 `shouldBe` 40
  
  describe "Logic2.luckySum" $ do
    context "when no argument is 13" $ do
      it "should return the sum of all arguments" $ do
        luckySum 1 2 3 `shouldBe` 6
        luckySum 6 5 2 `shouldBe` 13
        luckySum 7 2 1 `shouldBe` 10
    context "when the last argument (c) is 13" $ do
      it "should ignore the last argument and sum the first two" $ do
        luckySum 1 2 13 `shouldBe` 3
        luckySum 9 4 13 `shouldBe` 13
        luckySum 3 3 13 `shouldBe` 6
    context "when the middle argument (b) is 13" $ do
      it "should ignore the middle and last arguments, returning the first" $ do
        luckySum 1 13 3 `shouldBe` 1
        luckySum 1 13 13 `shouldBe` 1
        luckySum 8 13 2 `shouldBe` 8
    context "when the first argument (a) is 13" $ do
      it "should ignore all arguments and return 0" $ do
        luckySum 13 2 3 `shouldBe` 0
        luckySum 13 2 13 `shouldBe` 0
        luckySum 13 13 2 `shouldBe` 0