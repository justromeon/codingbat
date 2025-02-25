module String1 where
import Data.Function (on)

helloName :: String -> String
helloName name = "Hello " ++ name ++ "!" 

makeOutWord :: [a] -> [a] -> [a]
makeOutWord out word = left ++ word ++ right
  where
    (left,right) = splitAt (length out - 2) out

firstHalf :: [a] -> [a]
firstHalf = take =<< (`div` 2) . length

nonStart :: [a] -> [a] -> [a]
nonStart = (++) `on` drop 1

makeAbba :: [a] -> [a] -> [a]
makeAbba xs ys = concat [xs,ys,ys,xs]

extraEnd :: [a] -> [a]
extraEnd = concat . replicate 3 . (drop =<< subtract 2 . length) 