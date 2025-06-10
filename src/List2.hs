module List2 where

countEvens :: [Int] -> Int
countEvens = length . filter even

sum13 :: [Int] -> Int
sum13 [] = 0
sum13 xs = sum before13 + sum13 (drop 2 rest)
  where
    (before13,rest) = break (==13) xs