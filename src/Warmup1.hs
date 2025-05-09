module Warmup1 where

sleepIn :: Bool -> Bool -> Bool
sleepIn weekday vacation = not weekday || vacation

diff21 :: Int -> Int
diff21 n = if n > 21 then (n - 21) * 2 else 21 - n

nearHundred :: Int -> Bool
nearHundred n = abs (n - 100) <= 10 || abs (n - 200) <= 10

missingChar :: [a] -> Int -> [a]
missingChar str n = fstHalf ++ drop 1 sndHalf
  where
    (fstHalf, sndHalf) = splitAt n str

monkeyTrouble :: Bool -> Bool -> Bool
monkeyTrouble = (==)

parrotTrouble :: Bool -> Int -> Bool
parrotTrouble talking hour = talking && (hour < 7 || hour > 20)

posNeg :: Int -> Int -> Bool -> Bool
posNeg x y True = x<0 && y<0
posNeg x y False = (x<0 && y>0) || (x>0 && y<0)

frontBack :: [a] -> [a]
frontBack []     = []
frontBack [c]    = [c]
frontBack (c:cs) = last cs : init cs ++ [c]

sumDouble :: Int -> Int -> Int
sumDouble x y = if x == y then (x + y) * 2 else x + y

makes10 :: Int -> Int -> Bool
makes10 x y = x == 10 || y == 10 || x+y == 10

notString :: String -> String
notString str@('n':'o':'t':_) = str
notString str = "not " ++ str

front3 :: [a] -> [a]
front3 = concat . replicate 3 . take 3