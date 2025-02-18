module String1Spec (spec) where

import Test.Hspec
import String1

spec :: Spec
spec = do
  describe "String1.helloName" $ do
    it "greets simple name 'Bob' with Hello greeting" $
      helloName "Bob" `shouldBe` "Hello Bob!"
    it "handles multi-word name with spaces" $
      helloName "ho ho ho" `shouldBe` "Hello ho ho ho!"
    it "handles single character name" $
      helloName "X" `shouldBe` "Hello X!"
    it "handles name containing exclamation mark" $
      helloName "xyz!" `shouldBe` "Hello xyz!!"
    it "handles name 'Hello' itself" $
      helloName "Hello" `shouldBe` "Hello Hello!"

  describe "String1.makeOutWord" $ do
    it "places 'Yay' inside standard tags" $
      makeOutWord "<<>>" "Yay" `shouldBe` "<<Yay>>"
    it "handles longer word with standard tags" $
      makeOutWord "<<>>" "WooHoo" `shouldBe` "<<WooHoo>>"
    it "works with different style brackets" $
      makeOutWord "[[]]" "word" `shouldBe` "[[word]]"
    it "handles non-standard delimiter patterns" $
      makeOutWord "HHoo" "Hello" `shouldBe` "HHHellooo"
    it "works with asymmetric delimiters" $
      makeOutWord "abyz" "YAY" `shouldBe` "abYAYyz"