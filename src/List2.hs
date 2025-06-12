module List2 where

countEvens :: [Int] -> Int
countEvens = length . filter even

sum13 :: [Int] -> Int
sum13 [] = 0
sum13 xs = sum before13 + sum13 (drop 2 rest)
  where
    (before13,rest) = break (==13) xs

bigDiff :: [Int] -> Int
bigDiff = abs . (subtract <$> minimum <*> maximum)

sum67 :: [Int] -> Int
sum67 [] = 0
sum67 (6:xs) = sum67 . drop 1 . dropWhile (/=7) $ xs
sum67 (x:xs) = x + sum67 xs