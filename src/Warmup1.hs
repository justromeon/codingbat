module Warmup1 where

sleepIn :: Bool -> Bool -> Bool
sleepIn weekday vacation = not weekday || vacation

diff21 :: Int -> Int
diff21 n = if n > 21 then (n - 21) * 2 else 21 - n

nearHundred :: Int -> Bool
nearHundred n = abs (n - 100) <= 10 || abs (n - 200) <= 10

missingChar :: String -> Int -> String
missingChar str n = fstHalf ++ drop 1 sndHalf
  where
    (fstHalf, sndHalf) = splitAt n str

monkeyTrouble :: Bool -> Bool -> Bool
monkeyTrouble = (==)

parrotTrouble :: Bool -> Int -> Bool
parrotTrouble talking hour = talking && (hour < 7 || hour > 20)