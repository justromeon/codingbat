module Logic1 where

cigarParty :: Int -> Bool -> Bool
cigarParty cigars isWeekend = cigars >= 30 && (isWeekend || cigars <= 40)

caughtSpeeding :: Int -> Bool -> Int
caughtSpeeding speed isBirthday
    | speed <= 60 + adjustment = 0
    | speed <= 80 + adjustment = 1
    | otherwise = 2
  where
    adjustment = if isBirthday then 5 else 0

love6 :: Int -> Int -> Bool
love6 x y = 6 `elem` [x, y, x+y, abs (x-y)]

dateFashion :: Int -> Int -> Int
dateFashion you date
    | any (<=2) [you,date] = 0
    | any (>=8) [you,date] = 2
    | otherwise            = 1

sortaSum :: Int -> Int -> Int
sortaSum x y
    | x + y `elem` [10..19] = 20
    | otherwise             = x + y