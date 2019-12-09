import Control.Monad.State
import Text.ParserCombinators.ReadP hiding (between, get)
import Data.Char

type Point =  (Int, Int)
type Distance = Int
type Line = (Point, UpRight)
type Distances = (Int,Int)

data UpRight = Up Int | Ri Int
  deriving Show

instance Read UpRight where
  readsPrec _ = readP_to_S dirs

dirs :: ReadP UpRight
dirs = do
  d <- satisfy isUDRL
  i <- read <$> digits
  let parse = case d of
                'U' -> Up i
                'D' -> Up (-i)
                'R' -> Ri i
                'L' -> Ri (-i)
  return parse
    where
      isUDRL char = char `elem` "UDRL"
      digits = munch1 isDigit

getPoint :: Line -> Point
getPoint ((x,y),Up i) = (x, y+i)
getPoint ((x,y),Ri i) = (x+i, y)

getDist (Up i) = abs i
getDist (Ri i) = abs i

pointOnLines :: Point -> Line -> Line -> Bool
pointOnLines p1 l1 l2 = pointOnLine p1 l1 && pointOnLine p1 l2
  where
    pointOnLine (x,y) ((x1,y1), Up l) = x == x1 && between y y1 (y1 + l)
    pointOnLine (x,y) ((x1,y1), Ri l) = y == y1 && between x x1 (x1 + l)
    between i i1 i2 = (a + b) == c
      where a = distance i i1
            b = distance i i2
            c = distance i1 i2
    distance i1 i2 = abs $ i1 - i2

lineCross :: Line -> Line -> Maybe (Point,Distance)
lineCross l1 l2 = case (l1,l2) of
  ((_, Up _),(_, Up _)) -> Nothing
  ((_, Ri _),(_, Ri _)) -> Nothing
  (((x1,y1), Ri _),((x2,y2), Up _)) -> checkPoint (x2,y1)
  (((x1,y1), Up _),((x2,y2), Ri _)) -> checkPoint (x1,y2)
  where checkPoint p = if pointOnLines p l1 l2
          then Just (p,pointDistance (fst l1) (fst l2))
          else Nothing
        pointDistance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

  
start = (0,0) :: Point

intersections :: Line -> Point -> Distances -> [UpRight] -> State [(Point,Distance)] ()
intersections _ _ _ [] = return ()
intersections l current (d1,d2) (x:xs) = case lineCross l (current,x) of
                                           Nothing ->
                                             next
                                           Just (p,d) -> do
                                             s <- get
                                             put ((p,d1+d2+d):s)
                                             next
  where next = intersections l (getPoint (current,x)) (d1,d2 + getDist x) xs


allIntersections :: [UpRight] -> [UpRight] -> Point -> Distance -> State [(Point,Distance)] ()
allIntersections [] _  _ _   = return ()
allIntersections d1 d2 point dist = do
  let line = (point, head d1) :: Line
  intersections line start (dist,0) d2
  allIntersections (drop 1 d1) d2 (getPoint line) (dist + getDist (head d1))


smallestManhDist :: [(Point,Distance)] -> Int
smallestManhDist = foldr min maxBound . filter (>0) . fmap manhattanDist
  where
    manhattanDist ((x,y),dist) = abs x + abs y
    
smallestWireDist :: [(Point,Distance)] -> Int
smallestWireDist = foldr min maxBound . filter (>0) . fmap snd

manyDirs :: ReadP [UpRight]
manyDirs = sepBy dirs (char ',')


extractIntersections str = let (d1,d2) = readInput str
       in execState (allIntersections d1 d2 (0,0) 0) []

readInput :: String -> ([UpRight], [UpRight])
readInput str = let strs = lines str in
  let clean = fmap ((++ "]") . ('[':)) strs
  in (read (head clean), read (clean !! 1))

example1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
example0 = "R8,U5,L5,D3\nU7,R6,D4,L4"

main :: IO ()
main = do
  s <- readFile "input.txt"
--  let s = example1
  print $ smallestWireDist $ extractIntersections s
