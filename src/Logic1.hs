module Logic1 where

import Data.List (find)

cigarParty :: Int -> Bool -> Bool
cigarParty cigars isWeekend = cigars >= 30 && (isWeekend || cigars <= 40)

caughtSpeeding :: Int -> Bool -> Int
caughtSpeeding speed isBirthday
    | isBirthday = maybe 2 snd (find ((>=speed) . (+5) . fst) limits)
    | otherwise  = maybe 2 snd (find ((>=speed)        . fst) limits)
  where
    limits = [(60,0), (80,1)]