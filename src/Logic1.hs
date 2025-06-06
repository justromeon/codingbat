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

in1to10 :: Int -> Bool -> Bool
in1to10 n outsideMode
    | outsideMode = n <= 1 || n >= 10
    | otherwise   = n >= 1 && n <=  10

squirrelPlay :: Int -> Bool -> Bool
squirrelPlay temp isSummer = temp >= 60 && (if isSummer then temp <= 100 else temp <= 90)

alarmClock :: Int -> Bool -> String
alarmClock day vacation
    | vacation && weekend         = "off"
    | vacation && not weekend     = "10:00"
    | not vacation && weekend     = "10:00"
    | not vacation && not weekend = "7:00"
  where
    weekend = day == 0 || day == 6

nearTen :: Int -> Bool
nearTen = flip elem [0,1,2,8,9] . flip mod 10