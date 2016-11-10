module HaskellIntroDemos where

import Control.Concurrent
import Control.Concurrent.Async

myadd :: Int -> Int -> Int
myadd x y = x + y

myadd3 :: Int -> Int -> Int -> Int
myadd3 x y z = x + y + z

myList :: [Int]
myList = [1, 2, 3, 4]

mySum :: [Int] -> Int
mySum [] = 0
mySum (firstElement : rest) = firstElement + mySum rest


myLength :: [b] -> Int
myLength [] = 0
myLength (firstElement : rest)  = 1 + (myLength rest)

incFirst x y = x + 1

data Pair a b = Pair a b
  deriving (Show)

data BinTree a = Leaf a | Node (BinTree a) (BinTree a)
  deriving (Show)

myTree :: BinTree Int
myTree = (Node (Leaf 2) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5))))

zeroOutLeaves :: BinTree Int -> BinTree Int
zeroOutLeaves (Leaf x) = Leaf 0
zeroOutLeaves (Node leftChild rightChild) = Node (zeroOutLeaves leftChild) (zeroOutLeaves rightChild)

runThreads = do
  putStrLn "running threads"
  asyncBigNum1 <- async ((putStrLn "hello" >> return (2 ^ 3636) ) :: IO Integer)
  asyncBigNum2 <- async (putStrLn "why" >> return (2 ^ 454747))

  bigNum1 <- wait asyncBigNum1
  putStrLn (show bigNum1)
  bigNum2 <- wait asyncBigNum2
  putStrLn (show bigNum2)
  return ()
