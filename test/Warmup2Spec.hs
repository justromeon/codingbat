module Warmup2Spec (spec) where

import Test.Hspec
import Warmup2

spec :: Spec
spec = do
  describe "Warmup2.stringTimes" $ do
    it "repeats 'Hi' twice" $
      stringTimes "Hi" 2 `shouldBe` "HiHi"
    it "repeats 'Hi' three times" $
      stringTimes "Hi" 3 `shouldBe` "HiHiHi"
    it "repeats 'Hi' once" $
      stringTimes "Hi" 1 `shouldBe` "Hi"
    it "returns empty string when repeated 0 times" $
      stringTimes "Hi" 0 `shouldBe` ""
    it "repeats 'Hi' five times" $
      stringTimes "Hi" 5 `shouldBe` "HiHiHiHiHi"
    it "repeats 'Oh Boy!' twice" $
      stringTimes "Oh Boy!" 2 `shouldBe` "Oh Boy!Oh Boy!"
    it "repeats 'x' four times" $
      stringTimes "x" 4 `shouldBe` "xxxx"
    it "returns empty string when original is empty" $
      stringTimes "" 4 `shouldBe` ""
    it "repeats 'code' twice" $
      stringTimes "code" 2 `shouldBe` "codecode"
    it "repeats 'code' three times" $
      stringTimes "code" 3 `shouldBe` "codecodecode"
  
  describe "Warmup2.stringSplosion" $ do
    it "correctly builds incremental strings for 'Code'" $
      stringSplosion "Code" `shouldBe` "CCoCodCode"
    it "generates progressive substrings for 'abc'" $
      stringSplosion "abc" `shouldBe` "aababc"
    it "handles short strings like 'fade'" $
      stringSplosion "fade" `shouldBe` "ffafadfade"
    it "builds incremental strings for longer words like 'Kitten'" $
      stringSplosion "Kitten" `shouldBe` "KKiKitKittKitteKitten"
    it "works with strings of different lengths like 'Good'" $
      stringSplosion "Good" `shouldBe` "GGoGooGood"
  
  describe "Warmup2.arrayFront9" $ do
    it "returns True when 9 is in first 4 elements of [1, 2, 9, 3, 4]" $
      arrayFront9 [1, 2, 9, 3, 4] `shouldBe` True
    it "returns False when 9 is after first 4 elements" $
      arrayFront9 [1, 2, 3, 4, 9] `shouldBe` False
    it "checks first 4 elements for 9 in a longer array" $
      arrayFront9 [3, 9, 2, 3, 3] `shouldBe` True
    it "returns True when 9 is in first 4 elements" $
      arrayFront9 [9, 2, 3] `shouldBe` True
    it "handles arrays with multiple 9s in first 4 elements" $
      arrayFront9 [1, 9, 9] `shouldBe` True
  
  describe "Warmup2.frontTimes" $ do
    it "repeats first 3 chars of 'Chocolate' twice" $
      frontTimes "Chocolate" 2 `shouldBe` "ChoCho"
    it "repeats entire string when length is 3 or less" $
      frontTimes "Abc" 3 `shouldBe` "AbcAbcAbc"
    it "handles short strings correctly" $
      frontTimes "Ab" 4 `shouldBe` "AbAbAbAb"
    it "repeats single character string" $
      frontTimes "A" 4 `shouldBe` "AAAA"
    it "handles zero repetitions" $
      frontTimes "Abc" 0 `shouldBe` ""
  
  describe "Warmup2.last2" $ do
    it "counts occurrences of last two-character substring in 'hixxhi'" $
      last2 "hixxhi" `shouldBe` 1
    it "counts occurrences of last two-character substring in 'xaxxaxaxx'" $
      last2 "xaxxaxaxx" `shouldBe` 1
    it "counts occurrences of last two-character substring in 'xxaxxaxxaxx'" $
      last2 "xxaxxaxxaxx" `shouldBe` 3
    it "returns zero for strings shorter than three characters" $
      last2 "hi" `shouldBe` 0
    it "counts occurrences of last two-character substring in numeric string" $
      last2 "13121312" `shouldBe` 1

  describe "Warmup2.array123" $ do
    it "detects sequence 1,2,3 in middle of larger array" $
      array123 [1, 1, 2, 3, 1] `shouldBe` True
    it "returns True when array starts with sequence 1,2,3" $
      array123 [1, 2, 3, 1, 2, 3] `shouldBe` True
    it "returns True for array containing exactly sequence 1,2,3" $
      array123 [1, 2, 3] `shouldBe` True
    it "returns False when array lacks complete 1,2,3 sequence" $
      array123 [1, 1, 2, 4, 1] `shouldBe` False
    it "returns False for empty or too short arrays" $
      array123 [] `shouldBe` False

  describe "Warmup2.stringBits" $ do
    it "returns every other character from 'Hello'" $
      stringBits "Hello" `shouldBe` "Hlo"
    it "returns alternate characters from a longer word 'Greetings'" $
      stringBits "Greetings" `shouldBe` "Getns"
    it "handles string with spaces correctly" $
      stringBits "Hello Kitten" `shouldBe` "HloKte"
    it "returns hidden word from pattern 'hxaxpxpxy'" $
      stringBits "hxaxpxpxy" `shouldBe` "happy"
    it "returns empty string for empty input" $
      stringBits "" `shouldBe` ""
    it "returns first character only for two-character input" $
      stringBits "Hi" `shouldBe` "H"

  describe "Warmup2.arrayCount9" $ do
    it "counts single occurrence of 9 in array" $
      arrayCount9 [1, 2, 9] `shouldBe` 1
    it "counts multiple occurrences of 9 in array" $
      arrayCount9 [1, 9, 9, 3, 9] `shouldBe` 3
    it "returns zero when 9 is not present" $
      arrayCount9 [1, 2, 3] `shouldBe` 0 
    it "handles empty array correctly" $
      arrayCount9 [] `shouldBe` 0
    it "detects 9 at start of array" $
      arrayCount9 [9, 2, 4, 3, 1] `shouldBe` 1

  describe "Warmup2.stringMatch" $ do
   it "counts overlapping two-character matches in equal length strings" $
     stringMatch "xxcaazz" "xxbaaz" `shouldBe` 3
   it "counts matches in shorter string when lengths differ" $
     stringMatch "hello" "he" `shouldBe` 1
   it "handles strings with no matching two-character sequences" $
     stringMatch "abc" "axc" `shouldBe` 0
   it "returns zero for strings shorter than two characters" $
     stringMatch "h" "hello" `shouldBe` 0
   it "handles asymmetric matches between different length strings" $
     stringMatch "aaxxaaxx" "iaxxai" `shouldBe` 3