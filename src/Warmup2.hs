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

last2 :: String -> Int
last2 str = length $ filter (\(l2,l1) -> [l2,l1] == final2) $ zip rest (tail rest)
  where
    (rest, final2) = splitAt (length str - 2) str

array123 :: [Int] -> Bool
array123 = elem (1,2,3) . (zip3 <$> id <*> drop 1 <*> drop 2)