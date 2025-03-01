module String1 where
import Data.Function (on)

helloName :: String -> String
helloName name = "Hello " ++ name ++ "!" 

makeOutWord :: [a] -> [a] -> [a]
makeOutWord out word = left ++ word ++ right
  where
    (left,right) = splitAt (length out `div` 2) out

firstHalf :: [a] -> [a]
firstHalf = take =<< (`div` 2) . length

nonStart :: [a] -> [a] -> [a]
nonStart = (++) `on` drop 1

makeAbba :: [a] -> [a] -> [a]
makeAbba xs ys = concat [xs,ys,ys,xs]

extraEnd :: [a] -> [a]
extraEnd = concat . replicate 3 . (drop =<< subtract 2 . length)

withoutEnd :: [a] -> [a]
withoutEnd = reverse . drop 1 . reverse . drop 1

left2 :: [a] -> [a]
left2 = (++) <$> drop 2 <*> take 2

makeTags :: String -> String -> String
makeTags tag word = '<':tag++">" ++ word ++ "</"++tag++">"

firstTwo :: [a] -> [a]
firstTwo = take 2

comboString :: [a] -> [a] -> [a]
comboString xs ys = case (compare `on` length) xs ys of
  LT -> xs ++ ys ++ xs
  EQ -> xs ++ ys ++ xs
  GT -> ys ++ xs ++ ys