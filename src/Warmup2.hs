module Warmup2 where

stringTimes :: String -> Int -> String
stringTimes str n = concat (replicate n str)