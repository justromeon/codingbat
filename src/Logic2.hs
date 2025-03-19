module Logic2 where

makeBricks :: Int -> Int -> Int -> Bool
makeBricks small big goal = big*5 + small >= goal && goal `mod` 5 <= small

noTeenSum :: Int -> Int -> Int -> Int
noTeenSum a b c = sum $ filter (`notElem` [13,14,17,18,19]) [a,b,c]