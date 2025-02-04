module Main (main) where

import Test.Hspec
import qualified Warmup1Spec
import qualified Warmup2Spec

main :: IO ()
main = hspec $ do
  describe "Warmup1 module tests" Warmup1Spec.spec
  describe "Warmup2 module tests" Warmup2Spec.spec