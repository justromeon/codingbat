module Logic2 where

makeBricks :: Int -> Int -> Int -> Bool
makeBricks small big goal = big*5 + small >= goal && goal `mod` 5 <= small