module String2 where
import Data.Char (toLower)
import Data.Function (on)
import Data.List (isSuffixOf)

doubleChar :: String -> String
doubleChar = (<* [1..2])

countCode :: String -> Int
countCode ('c':'o':_:'e':rest) = 1 + countCode rest
countCode []     = 0
countCode (_:xs) = countCode xs

countHi :: String -> Int
countHi = length . filter (== ('h','i')) . bigrams
  where
    bigrams = zip <*> tail

endOther :: String -> String -> Bool
endOther = (\s1 s2 -> s1 `isSuffixOf` s2 || s2 `isSuffixOf` s1) `on` map toLower