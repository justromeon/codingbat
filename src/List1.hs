module List1 where
import Data.Function (on)

firstLast6 :: [Int] -> Bool
firstLast6 = ( (||) `on` (==6) ) <$> head <*> last

commonEnd :: [Int] -> [Int] -> Bool
commonEnd [x] [y]       = x == y
commonEnd (x:xs) (y:ys) = x == y || last xs == last ys

reverse3 :: [a] -> [a]
reverse3 = reverse