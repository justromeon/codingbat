module Logic2 where
import qualified Data.Map as M

makeBricks :: Int -> Int -> Int -> Bool
makeBricks small big goal = big*5 + small >= goal && goal `mod` 5 <= small

noTeenSum :: Int -> Int -> Int -> Int
noTeenSum a b c = sum $ filter (`notElem` [13,14,17,18,19]) [a,b,c]

makeChocolate :: Int -> Int -> Int -> Int
makeChocolate small big goal
    | smallNeeded <= small = smallNeeded
    | otherwise            = -1
  where
    bigsFitted  = min big (goal `div` 5)
    smallNeeded = goal - bigsFitted*5

loneSum :: Int -> Int -> Int -> Int
loneSum a b c
  | a == b && b == c = 0
  | a == b = c
  | a == c = b
  | b == c = a
  | otherwise = a + b + c

roundSum :: Int -> Int -> Int -> Int
roundSum a b c = sum (map roundByOnes [a,b,c])
  where
    roundByOnes n
      | n `mod` 10 >= 5 = ((n `div` 10) + 1) * 10
      | otherwise       =  (n `div` 10)      * 10

luckySum :: Int -> Int -> Int -> Int
luckySum a b c = sum $ takeWhile (/=13) [a,b,c]

closeFar :: Int -> Int -> Int -> Bool
closeFar a b c
    | abs (a - b) <= 1 = abs (a - c) >= 2 && abs (b - c) >= 2
    | abs (a - c) <= 1 = abs (a - b) >= 2 && abs (b - c) >= 2
    | otherwise        = False