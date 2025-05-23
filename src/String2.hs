module String2 where
import Data.Char (toLower)
import Data.Function (on)
import Data.List (isSuffixOf, isInfixOf)

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

catDog :: String -> Bool
catDog = (==) <$> countCat <*> countDog
  where
    countCat ('c':'a':'t':rest) = 1 + countCat rest
    countCat []     = 0
    countCat (_:xs) = countCat xs

    countDog ('d':'o':'g':rest) = 1 + countDog rest
    countDog []     = 0
    countDog (_:xs) = countDog xs

xyzThere :: String -> Bool
xyzThere = ("xyz" `isInfixOf`) . rmIvalidX
  where
    rmIvalidX str = [c | (prev, c) <- zip (' ':str) str, c /= 'x' || prev /= '.']