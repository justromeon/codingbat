module Warmup2Spec (spec) where

import Test.Hspec
import Warmup2 (stringTimes, stringSplosion)

spec :: Spec
spec = describe "Warmup2.stringTimes" $ do
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