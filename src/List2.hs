module List2 where

countEvens :: [Int] -> Int
countEvens = length . filter even