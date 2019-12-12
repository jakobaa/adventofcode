module IntComp where
import Data.List.Split
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Debug.Trace
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

type Index = Int
type IntCode = Int
type Param = Int

type IntFun = Int -> Int -> Int
type JumpFun = Int -> Int -> Bool
type Input  = [Int]
type Output = [Int]
               --       s index content
type Array s = STUArray s Index IntCode

data Cases s = Input | Output | Done
data Operation = Add | Mul | Save (Maybe Int) | Out | JumpTrue | JumpFalse | LessThan | Equal | Halt
  deriving (Eq, Show)

data ParamMode = Pos | Imm
  deriving (Eq, Show)

parseInput :: String -> IO Input
parseInput file = map read . endByOneOf ",\n" <$> readFile file

fixInput :: Input -> (Int, Int) -> Input
fixInput (op:i1:i2:xs) (verb,noun) = op:verb:noun:xs


--fillArray :: STUArray s Int IntCode -> [Int] -> ST s (STUArray s Int IntCode)
--fillArray arr list = 

makeArray :: [Int] -> ST s (Array s)
makeArray list = newListArray (0,length list - 1) list
                 

readIndices :: [Index] -> Array s -> ST s [Int]
readIndices xs arr = forM xs (`getOne` arr)

getOne :: Int -> Array s -> ST s IntCode
getOne i ass = readArray ass i
               

getVal :: ParamMode -> Index -> Array s -> ST s Int
getVal Imm i arr = getImmVal arr i
getVal Pos i arr = getPosVal arr i

getImmVal :: Array s -> Index -> ST s Int
getImmVal = readArray 

getPosVal :: Array s -> Index -> ST s Int
getPosVal arr i = do i_val <- readArray arr i
                     trace ("i_val: " ++ show i_val) readArray arr i_val


addMul :: IntFun -> Index -> [ParamMode] -> Array s -> ST s (Index, Maybe Int)
addMul f i modes arr = do x <- getVal (modes!!0) (i+1) arr
                          y <- getVal (modes!!1) (i+2) arr
                          out <- getVal Imm (i+3) arr
                          writeArray arr out (x `f` y)
                          return (i+4, Nothing)

jump :: JumpFun -> Index -> [ParamMode] -> Array s -> ST s (Index, Maybe Int)
jump f i modes arr =  do
  x <- getVal (modes!!0) (i+1) arr
  if x `f` 0
    then do y <- getVal (modes!!1) (i+2) arr
            return (y,Nothing)
    else return (i+3,Nothing)



lessOrEq :: JumpFun -> Index -> [ParamMode] -> Array s -> ST s (Index, Maybe Int)
lessOrEq f i modes arr =  do
  x <- getVal (modes!!0) (i+1) arr
  y <- getVal (modes!!1) (i+2) arr
  if x `f` y
    then store 1
    else store 0
  return (i+4,Nothing)
  where store a = do y <- getVal Imm (i+3) arr
                     trace ("read index " ++ show (i+3) ++ ", writing 1 to " ++ show y) 
                       writeArray arr y a
doOp :: Operation -> Index -> [ParamMode] -> Array s -> ST s (Index, Maybe Int)
doOp Add i modes arr = addMul (+) i modes arr
doOp Mul i modes arr = addMul (*) i modes arr
doOp (Save (Just input)) i modes arr = do trace ("modes:" ++ show modes) $ return ()
                                          x <- getVal Imm (i+1) arr
                                          trace ("x = " ++ show x) $ 
                                            writeArray arr x input
                                          return (i+2, Nothing)
doOp Out i modes arr = do val <- getVal (modes!!0) (i+1) arr
                          return (i+2, Just val)
doOp JumpTrue  i modes arr = jump (/=) i modes arr
doOp JumpFalse i modes arr = jump (==) i modes arr
doOp LessThan  i modes arr = lessOrEq (<) i modes arr
doOp Equal     i modes arr = lessOrEq (==) i modes arr
doOp Halt _ _ _            = return (0,Nothing)

code2op :: IntCode -> Operation
code2op 1 = Add
code2op 2 = Mul
code2op 3 = Save Nothing
code2op 4 = Out
code2op 5 = JumpTrue
code2op 6 = JumpFalse
code2op 7 = LessThan
code2op 8 = Equal
code2op 99 = Halt
code2op op = error ("Undefined operation: " ++ show op)

getOp :: [Int] -> Operation
getOp list = trace (show list) $ case list of
                                   [x] -> code2op x
                                   xs  -> code2op $ xs!!1 * 10 + xs!!0

getParam :: Int -> ParamMode
getParam 0 = Pos
getParam 1 = Imm

class ParamClass a where
  getNumParams :: a -> Int

instance ParamClass Operation where
  getNumParams op
    | op `elem` [Save Nothing, Out] = 1
    | op `elem` [JumpTrue, JumpFalse] = 2
    | op `elem` [Add, Mul, LessThan, Equal] = 3
    | otherwise = 0
    

getParamModes :: [Int] -> Operation -> [ParamMode]
getParamModes params = trace ("params: " ++ show params) padParams . getNumParams
  where padParams n = map getParam $ params ++ take (n - length params) [0,0..]
        

readCode :: Int -> (Operation, [ParamMode])
readCode code = (op, modes)
  where list = toListRev code
        op = getOp list
        modes = getParamModes (drop 2 list) op

handleInput (Save a, modes) = do (inp,out) <- get
                                 trace (show (inp,out)) $ return ()
                                 let ret = (Save (Just (head inp)), modes)
                                 put (drop 1 inp, out)
                                 --trace "head inp2" $ return ()
                                 return ret
handleInput a = return a

getFirstTen :: Int -> Array s -> ST s ()
getFirstTen 0 arr = do val <- readArray arr 0
                       trace ("i" ++ show 0 ++ ",v" ++ show val) return ()
getFirstTen i arr = do val <- readArray arr i
                       trace ("i" ++ show i ++ ",v" ++ show val) getFirstTen (i-1) arr

runIntCodes :: Array s -> Index -> StateT ([Int], [Int]) (ST s) (Cases s)
runIntCodes arr i = do --trace "IntCoding" (return ())
                       code <- lift $ readArray arr i
--                       val225 <- lift $ readArray arr 225
  --                     trace ("value at 225: " ++ show val225) (return ())
                       --lift $ getFirstTen 5 arr
                       trace (show code) (return ())
                       (op,modes) <- handleInput $ readCode code
                       trace (show op ++ ", modes: " ++ show modes) (return ())
                       newI <- lift  (doOp op i modes arr) >>= appendOut
                       trace ("new index: " ++ show newI) (return ())
                       case op of
                         Halt -> do
                           --return $ trace "halting!" ()
                           return Done
                         _    -> do
                           return $ trace ("new index: " ++ show newI) ()
                           runIntCodes arr newI
--                       return (Done arr)


appendOut (index,Just val) = do (inp,out) <- get
                                put (inp,val:out)
                                return index
appendOut (index,Nothing)  = return index

runStateThing :: Array s -> Index -> ST s (Array s)
runStateThing inputArray i = do
  (res,state) <- runStateT (runIntCodes inputArray i) ([5],[])
  case res of
    Input -> undefined
    Output -> undefined
    Done  -> return ()
  trace (show state) $ return ()    
  return inputArray
  --snib <- getElems input
  --return (snib,5)

initProgram :: Array s -> Input -> ST s (Array s)
initProgram arr input = undefined

snibsnab :: Int -> (ST s (Array s), Int)
snibsnab = undefined

newMain :: IO ()
newMain = do
  input <- parseInput "5/input.txt"
  --trace (show (take 5 input)) (return 5)
  let array = makeArray input
  let arst = runSTUArray (array >>= \a -> runStateThing a 0)
  
  print arst
--             print . head $ filter (\a -> head a == 19690720) allInputs

toListRev :: Int -> [Int]
toListRev 0 = []
toListRev i =  i `mod` 10:toListRev (i `div` 10)

toList :: Int -> [Int]
toList = reverse . toListRev
