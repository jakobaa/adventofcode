--import ListSetOps
import Data.List
input = [x | x <- [359282..820401], let list = toList x in and [checkDecr list, two_adjacent list]]

toList :: Int -> [Int]
toList 0 = []
toList i =  (i `mod` 10:toList (i `div` 10))

checkDecr xs = and $ zipWith (>=) xs $ drop 1 xs

countsOf :: [Int] -> (Int,[Int])
countsOf []     = (0,[])
countsOf i@(l:ls) = let a = length $ elemIndices l i in (a, drop a i)

allCounts :: [Int] -> [Int]
allCounts [] = []
allCounts l = let (a,ls) = countsOf l in
  (a:allCounts ls)

checkTwo :: Int -> Bool
checkTwo i = let a = allCounts . toList $ i
             in 2 `elem` a

two_adjacent :: [Int] -> Bool
two_adjacent []   = False
two_adjacent (x:[]) = False
two_adjacent (x:y:xs) = if x == y then True else two_adjacent (y:xs)

non_decreasing :: Int -> Bool
non_decreasing = non' . show
  where non' :: String -> Bool
        non' [] = False


part1 = length input
part2 = length [x | x <- input, checkTwo x]
