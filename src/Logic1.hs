module Logic1 where

import Data.List (find)

cigarParty :: Int -> Bool -> Bool
cigarParty cigars isWeekend = cigars >= 30 && (isWeekend || cigars <= 40)

caughtSpeeding :: Int -> Bool -> Int
caughtSpeeding speed isBirthday
    | speed <= 60 + adjustment = 0
    | speed <= 80 + adjustment = 1
    | otherwise = 2
  where
    adjustment = if isBirthday then 5 else 0