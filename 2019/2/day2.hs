import Data.List.Split
import Data.Array.ST
import Control.Monad.ST
import Control.Monad.Writer

type Arg = Int
type Index = Int
type List = [Int]

parseInput :: String -> IO List
parseInput file = readFile file >>= return . map read . endByOneOf ",\n"

fixInput :: List -> (Int, Int) -> List
fixInput (op:i1:i2:xs) (verb,noun) = op:verb:noun:xs

main :: IO ()
main = do input <- parseInput "input.txt"
          let allInputs =  [doInput 0 . fixInput input $ (x,y) |  x <- [0..99], y <- [0..99]]
          print . head $ filter (\a -> head a == 19690720) allInputs



doInput :: Index -> List ->  List
doInput start list = case list!!start of
  1  -> doInput (start+4) $ replace (arg1+arg2) out list
  2  -> doInput (start+4) $ replace (arg1*arg2) out list
  99 -> list
  _  -> error "unknown op code"
  where
    arg1 = list!!in1
    arg2 = list!!in2
    (op:in1:in2:out:xs) = take 4 (drop start list)

replace :: Arg -> Index -> List -> List
replace x i list = let (li, st) = splitAt i list
                 in li ++ x:(drop 1 st)
