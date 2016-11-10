module HaskellIntro where


myString = "wtf"

myadd :: Int -> Int -> Int
myadd x y = x + y

--mytinyList = [1, 2, 3, 4]



-- recursive functions

-- 1.. n
-- 4 -> 10

sumUpTo :: Int -> Int
sumUpTo 0 = 0
sumUpTo n = n + sumUpTo (n - 1)

sum3 :: Int -> Int -> Int -> Int
sum3 x y z = x + y + z

inc = myadd 1


-- lists

x = 1425



mySum :: [Int] -> Int
mySum [] = 0
mySum (firstElement : rest) = firstElement + mySum rest

myLength :: [Int] -> Int
myLength [] = 0
myLength (firstElement : rest) = 1 + myLength rest

repeatInts :: Int -> [Int]
repeatInts x = x : repeatInts x

-- higher order functions

evens = filter even [1..20]


data IntPair = IntPair Int Int
  deriving (Show)

sumIntPair :: IntPair -> Int
sumIntPair (IntPair x y) = x + y

data Pair a b = Pair a b


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (head : rest) = Just head

-- IO 

andIO :: IO a -> IO b -> IO b
andIO = (>>)
mian = putStrLn "write your name." `andIO` getLine `andIO` putStrLn "Thanks!"


bind :: IO a -> (a -> IO b) -> IO b
bind = (>>=)

mian2 = putStrLn "write your name" `andIO` getLine `bind` (\line -> putStrLn ("Thanks " ++ line))