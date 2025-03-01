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

  describe "String1.makeAbba" $ do
    it "creates palindrome pattern with 'Hi' and 'Bye'" $
      makeAbba "Hi" "Bye" `shouldBe` "HiByeByeHi"
    it "forms ABBA pattern with longer second string" $
      makeAbba "Yo" "Alice" `shouldBe` "YoAliceAliceYo"
    it "handles case with empty second string" $
      makeAbba "x" "" `shouldBe` "xx"
    it "handles case with empty first string" $
      makeAbba "" "y" `shouldBe` "yy"
    it "creates pattern with identical strings" $
      makeAbba "Ya" "Ya" `shouldBe` "YaYaYaYa"

  describe "String1.extraEnd" $ do
   it "repeats last two characters of 'Hello' three times" $
     extraEnd "Hello" `shouldBe` "lololo"
   it "repeats entire string 'ab' three times" $
     extraEnd "ab" `shouldBe` "ababab"
   it "repeats entire string 'Hi' three times" $
     extraEnd "Hi" `shouldBe` "HiHiHi"
   it "repeats last two characters of 'Candy' three times" $
     extraEnd "Candy" `shouldBe` "dydydy"
   it "repeats last two characters of 'Code' three times" $
     extraEnd "Code" `shouldBe` "dedede"

  describe "String1.withoutEnd" $ do
    it "returns the middle characters of 'Hello' (without first and last)" $
      withoutEnd "Hello" `shouldBe` "ell"
    it "returns the middle characters of 'coding' (without first and last)" $
      withoutEnd "coding" `shouldBe` "odin"
    it "returns the middle characters of 'Chocolate!' (without first and last)" $
      withoutEnd "Chocolate!" `shouldBe` "hocolate"
    it "returns empty string for a 2-character string 'ab'" $
      withoutEnd "ab" `shouldBe` ""
    it "handles longer strings like 'woohoo' correctly" $
      withoutEnd "woohoo" `shouldBe` "ooho"

  describe "String1.left2" $ do
    it "rotates first two characters of medium-length word to the end" $
      left2 "Hello" `shouldBe` "lloHe"
    it "handles short word with exactly two characters" $
      left2 "Hi" `shouldBe` "Hi"
    it "works with typical four-letter programming word" $
      left2 "code" `shouldBe` "deco"
    it "handles numeric input correctly" $
      left2 "12345" `shouldBe` "34512"
    it "processes longer words preserving character order" $
      left2 "Chocolate" `shouldBe` "ocolateCh"

  describe "String1.makeTags" $ do
    it "wraps 'Yay' in i tags" $
      makeTags "i" "Yay" `shouldBe` "<i>Yay</i>"
    it "wraps 'Hello' in i tags" $
      makeTags "i" "Hello" `shouldBe` "<i>Hello</i>"
    it "wraps 'Yay' in cite tags" $
      makeTags "cite" "Yay" `shouldBe` "<cite>Yay</cite>"
    it "handles longer tag names like 'address'" $
      makeTags "address" "here" `shouldBe` "<address>here</address>"
    it "handles empty content with i tag" $
      makeTags "i" "" `shouldBe` "<i></i>"

  describe "String1.firstTwo" $ do
    it "returns first two characters of 'Hello'" $
      firstTwo "Hello" `shouldBe` "He"
    it "returns first two characters of 'abcdefg'" $
      firstTwo "abcdefg" `shouldBe` "ab"
    it "returns entire string when input is exactly two characters" $
      firstTwo "ab" `shouldBe` "ab"
    it "returns entire string when input is one character" $
      firstTwo "a" `shouldBe` "a"
    it "returns empty string when input is empty" $
      firstTwo "" `shouldBe` ""

  describe "String1.comboString" $ do
    it "wraps shorter string around longer one (hi, Hello)" $
      comboString "hi" "Hello" `shouldBe` "hiHellohi"
    it "wraps shorter string around longer one (b, aaa)" $
      comboString "b" "aaa" `shouldBe` "baaab"
    it "returns single string when one input is empty (aaa, empty)" $
      comboString "aaa" "" `shouldBe` "aaa"
    it "returns single string when one input is empty (empty, bb)" $
      comboString "" "bb" `shouldBe` "bb"
    it "wraps the longer string when the second string is shorter (bb, a)" $
      comboString "bb" "a" `shouldBe` "abba"