module String1 where
import Data.Function (on)

helloName :: String -> String
helloName name = "Hello " ++ name ++ "!" 

makeOutWord :: String -> String -> String
makeOutWord out word = left ++ word ++ right
  where
    (left,right) = splitAt (length out - 2) out

firstHalf :: String -> String
firstHalf = take =<< (`div` 2) . length

nonStart :: String -> String -> String
nonStart = (++) `on` drop 1

makeAbba :: String -> String -> String
makeAbba xs ys = concat [xs,ys,ys,xs]