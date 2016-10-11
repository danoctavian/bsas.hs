module Lib where


import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad
import Data.IORef

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- useful links

{-
https://tryhaskell.org/
https://www.haskell.org/hoogle/
-}

-- stateless functions
myadd :: Int -> Int -> Int
myadd x y = x + y

myString = "String"


-- data types 

data BinaryTree a = Node (BinaryTree a) (BinaryTree a) | Leaf a

exampleTree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))


-- alternatively

data BinaryTree' a =
  Node'{
    leftChild :: (BinaryTree a),
    rightChild :: (BinaryTree a)
  }
  | Leaf' a

-- currying

inc x = myadd 1

{- 

!!! TOO FAST. better on a white board.

inc 7


(myadd incSize) 7
(\y -> incSize + y) 7
incSize + 7
1 + 7 
8

1 + 7 + useless
8 + useless 
8 + 1 `div` 0


getLine >>= (\x -> print x)
-}

-- pattern matching on lists with recursion

mysum (x : xs) = x + mysum xs
mysum [] = 0

mylen (x: xs) = 1 + mylen xs
mylen [] = 0


-- haskell lambdas

myadd' = \x y -> x + y
myadd'' x = \y -> x + y

-- higher order functions

myIncrements = map inc [1, 2, 3]

myIncrements' = map (\x -> x + 1) [1, 2, 3]

mysum' xs = foldl myadd 0 

-- list functions and type casting

average xs = (mysum xs) / (fromIntegral $ length xs)

takeSome = take 3 [1, 2, 3, 4, 5]
theTail = tail [1, 2, 3, 4, 5]
theHead = head [1, 2, 3]
noHeadBreaking = head []

-- no nulls. using the Maybe type. fix the head function

data MyMaybe a = MyJust a | MyNothing

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

{-
more complex example: moving averages
input 1 2 3 4 5 6
input 3
output 2 3 5

-}

movingAverage allNumbers@(n : ns) windowSize
  = if (length numbersInWindow == windowSize) then
      windowAverage : (movingAverage ns windowSize)
    else
      [windowAverage]
    where
      numbersInWindow = take windowSize allNumbers
      windowAverage = average numbersInWindow
movingAverage [] _ = []


-- first attempt at I/O - list
-- executeIO :: [IO ()] -> IO ()
-- executeIO = 

ioAnd :: IO a -> IO b -> IO b
ioAnd firstComp secondComp = firstComp >> secondComp


ioBind :: (IO a) -> (a -> IO b) -> (IO b)
ioBind = (>>=)

ioAnd' c1 c2= c1 `ioBind` (\_ -> c2)


-- yes or no. first attempt

yesOrNo =
  putStrLn "(y/n)?" `ioAnd`
  getLine `ioBind`
  (\line -> case line of
    "y" -> putStrLn "You answered yes."
    "n" -> putStrLn "You answered no."
    other -> putStrLn "Unknown response. Try again." `ioAnd` yesOrNo
    )

-- better version
yesOrNo' = do
  putStrLn "(y/n)?"
  line <- getLine
  case line of
    "y" -> putStrLn "You answered yes."
    "n" -> putStrLn "You answered no."
    other -> putStrLn "Unknown response. Try again." >> yesOrNo'

-- syntactic sugar for writing imperative-style code: do notation

myIoFunc =
  getLine >>=
    (\myLine ->
      getLine >>=
      (\mySecondLine ->
        print myLine >> return mySecondLine)) 

-- better

myIoFunc' = do
  myLine <- getLine
  secondLine <- getLine
  print myLine
  return secondLine

-- I/O 

demoManyThreads = do
  let threadCount = 10 ^ 6
  
  forM [0..threadCount] $ \i -> do
      putStrLn (show i)
  return ()


