module Main (main) where

import Test.Hspec

import qualified Warmup1Spec
import qualified Warmup2Spec
import qualified String1Spec
import qualified Logic1Spec
import qualified Logic2Spec

main :: IO ()
main = hspec $ do
  Warmup1Spec.spec
  Warmup2Spec.spec
  String1Spec.spec
  Logic1Spec.spec
  Logic2Spec.spec