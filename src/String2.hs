module String2 where

doubleChar :: String -> String
doubleChar = (<* [1..2])

countCode :: String -> Int
countCode ('c':'o':_:'e':rest) = 1 + countCode rest
countCode []     = 0
countCode (_:xs) = countCode xs