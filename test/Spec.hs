module Main (main) where

import Test.Hspec
import qualified Warmup1Spec
import qualified Warmup2Spec

main :: IO ()
main = hspec $ do
  Warmup1Spec.spec
  Warmup2Spec.spec