module Main (main) where

import Test.Hspec
import qualified Warmup1Spec

main :: IO ()
main = hspec $ do
  describe "Warmup1 module tests" Warmup1Spec.spec
