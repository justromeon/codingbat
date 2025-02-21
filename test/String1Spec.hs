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

  describe "String1.firstHalf" $ do
    it "returns first half of even-length word 'WooHoo'" $
      firstHalf "WooHoo" `shouldBe` "Woo"
    it "splits 'HelloThere' into first half correctly" $
      firstHalf "HelloThere" `shouldBe` "Hello"
    it "handles single character when string length is two" $
      firstHalf "ab" `shouldBe` "a"
    it "handles empty string correctly" $
      firstHalf "" `shouldBe` ""
    it "returns first half of numeric string correctly" $
      firstHalf "0123456789" `shouldBe` "01234"

  describe "String1.nonStart" $ do
    it "combines words 'Hello' and 'There' after removing first letters" $
      nonStart "Hello" "There" `shouldBe` "ellohere"
    it "combines 'java' and 'code' without their first letters" $
      nonStart "java" "code" `shouldBe` "avaode"
    it "handles case when second string has only one character" $
      nonStart "ab" "x" `shouldBe` "b"
    it "returns empty string when both inputs are single characters" $
      nonStart "a" "x" `shouldBe` ""
    it "combines similar-ending words correctly" $
      nonStart "mart" "dart" `shouldBe` "artart"