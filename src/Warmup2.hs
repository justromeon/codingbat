module Warmup2 where

import           Data.Function (on)
import qualified Data.Set as S

stringTimes :: [a] -> Int -> [a]
stringTimes xs n = concat (replicate n xs)

stringSplosion :: [a] -> [a]
stringSplosion = concat . foldr (\x acc -> [x] : map (x:) acc) []

arrayFront9 :: [Int] -> Bool
arrayFront9 = elem 9 . take 4

frontTimes :: [a] -> Int -> [a]
frontTimes str = concat . flip replicate (take 3 str)

last2 :: Eq a => [a] -> Int
last2 str = length $ filter (\(l2,l1) -> [l2,l1] == final2) $ zip rest (tail rest)
  where
    (rest, final2) = splitAt (length str - 2) str

array123 :: [Int] -> Bool
array123 = elem (1,2,3) . (zip3 <$> id <*> drop 1 <*> drop 2)

stringBits :: [a] -> [a]
stringBits []  = []
stringBits [x] = [x]
stringBits (x:_:zs) = x : stringBits zs

arrayCount9 :: [Int] -> Int
arrayCount9 = length . filter (== 9)

stringMatch :: Ord a => [a] -> [a] -> Int
stringMatch = (S.size .) . (S.intersection `on` S.fromList . bigrams)
  where
    bigrams = zip <*> tail