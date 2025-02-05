module Warmup2 where

stringTimes :: String -> Int -> String
stringTimes str n = concat (replicate n str)

stringSplosion :: String -> String
stringSplosion = concat . aux
  where
    aux ""     = []
    aux (c:cs) = [c] : map (c:) (aux cs)