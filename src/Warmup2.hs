module Warmup2 where

stringTimes :: String -> Int -> String
stringTimes str n = concat (replicate n str)

stringSplosion :: String -> String
stringSplosion = concat . aux
  where
    aux ""     = []
    aux (c:cs) = [c] : map (c:) (aux cs)

arrayFront9 :: [Int] -> Bool
arrayFront9 = elem 9 . take 4

frontTimes :: String -> Int -> String
frontTimes str = concat . flip replicate (take 3 str)