module Logic1 where

cigarParty :: Int -> Bool -> Bool
cigarParty cigars isWeekend = cigars >= 30 && (isWeekend || cigars <= 40)