module Warmup1 where

sleepIn :: Bool -> Bool -> Bool
sleepIn weekday vacation = not weekday || vacation

diff21 :: Int -> Int
diff21 n = if n > 21 then (n - 21) * 2 else 21 - n