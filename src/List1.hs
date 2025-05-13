module List1 where
import Data.Function (on)

firstLast6 :: [Int] -> Bool
firstLast6 = ( (||) `on` (==6) ) <$> head <*> last