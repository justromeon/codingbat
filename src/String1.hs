module String1 where

helloName :: String -> String
helloName name = "Hello " ++ name ++ "!" 

makeOutWord :: String -> String -> String
makeOutWord out word = left ++ word ++ right
  where
    (left,right) = splitAt (length out - 2) out

firstHalf :: String -> String
firstHalf = take =<< (flip div 2 . length)