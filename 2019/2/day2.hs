
import Data.List.Split
import Data.Array.ST
import Control.Monad.ST
import Control.Monad


type Index = Int
type List = [Int]
type IntCode = Int

               --       s index content
type Array s = STUArray s Index IntCode

data Operation = Add | Mul

parseInput :: String -> IO List
parseInput file = readFile file >>= return . map read . endByOneOf ",\n"

fixInput :: List -> (Int, Int) -> List
fixInput (op:i1:i2:xs) (verb,noun) = op:verb:noun:xs

main :: IO ()
main = do input <- parseInput "input.txt"
          let allInputs =  [doInput 0 . fixInput input $ (x,y) |  x <- [0..99], y <- [0..99]]
          print . head $ filter (\a -> head a == 19690720) allInputs


--fillArray :: STUArray s Int IntCode -> [Int] -> ST s (STUArray s Int IntCode)
--fillArray arr list = 

makeArray :: [Int] -> ST s (Array s)
makeArray list = do arr <- newListArray (0,length list - 1) list :: ST s (Array s)
                    return arr

readIndices :: [Index] -> Array s -> ST s [Int]
readIndices xs arr = forM xs (\x -> getOne x arr)

getOne :: Int -> Array s -> ST s IntCode
getOne i ass = do a <- readArray ass i
                  return a

doOp :: Operation -> [Index] -> Array s -> ST s ()
doOp Add (i1:i2:out:[]) arr = do x <- readArray arr i1
                                 y <- readArray arr i2
                                 writeArray arr out (x+y)
doOp Mul (i1:i2:out:[]) arr = undefined

runIntCodes :: Array s -> Index -> ST s (Array s)
runIntCodes arr i = undefined

runStateThing :: [Int] -> ST s ()
runStateThing input = do arr <- makeArray input
                         return ()

newMain :: IO ()
newMain = do input <- parseInput "input.txt"
             let arst = runST $ runStateThing input
             return ()
--             print . head $ filter (\a -> head a == 19690720) allInputs


doStuffArray intList = do arr <- makeArray intList
                          a <- readArray arr $ 3
                          doOp Add [1,2,3,4] arr
                          b <- readArray arr $ 3
                          c <- readIndices [4,5,6] arr
                          return (a,b,c)

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

replace :: Int -> Index -> List -> List
replace x i list = let (li, st) = splitAt i list
                 in li ++ x:(drop 1 st)
