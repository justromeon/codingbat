module List1 where
import Data.Function (on)

firstLast6 :: [Int] -> Bool
firstLast6 = ( (||) `on` (==6) ) <$> head <*> last

commonEnd :: [Int] -> [Int] -> Bool
commonEnd [x] [y]       = x == y
commonEnd (x:xs) (y:ys) = x == y || last xs == last ys

reverse3 :: [a] -> [a]
reverse3 = reverse

middleWay :: [a] -> [a] -> [a]
middleWay [_,x,_] [_,y,_] = [x,y]

sameFirstLast :: Eq a => [a] -> Bool
sameFirstLast = (==) <$> head <*> last

sum3 :: [Int] -> Int
sum3 = sum

maxEnd3 :: [Int] -> [Int]
maxEnd3 = replicate 3 . (max <$> head <*> last)

makeEnds :: [Int] -> [Int]
makeEnds = (:) <$> head <*> (pure . last)

makePi :: [Int]
makePi = [3,1,4]

rotateLeft3 :: [Int] -> [Int]
rotateLeft3 = uncurry (flip (++)) . splitAt 1

sum2 :: [Int] -> Int
sum2 = sum . take 2

has23 :: [Int] -> Bool
has23 = any (`elem` [2,3])