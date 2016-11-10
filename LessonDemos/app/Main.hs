module Main where

import HaskellIntroDemos
import BlockchainAPIQueryDemos

{-
name = raw_input()
print name
-}

{-
bind >>=

andIO = (>>)
-}

oldMain :: IO ()
oldMain = getLine >>=
        (\name -> putStrLn ("hello " ++ name)) >>
        getLine >>=
        (\surname -> putStrLn surname)

main = do
  name <- getLine
  surname <- getLine
  return 3636
  putStrLn name
  putStrLn surname
  age <- getLine
  putStrLn age

-- IO String -> String

concatUserString :: IO String
concatUserString = do
  first <- getLine
  second <- getLine
  return (first ++ second)


--putStrLn "1" `andIO` putStrLn "wtf" `andIO` getLine `andIO` putStrLn "lol"

--executeIO [getLine, putStrLn (show 1), putStrLn "there", putStrLn "myfriend"]

andIO :: IO a -> IO b -> IO b
andIO action1 action2 = action1 >> action2

executeIO :: [IO ()] -> IO ()
executeIO (a : as) = a >> (executeIO as)
executeIO [] = return ()

bind :: IO a -> (a -> IO b) -> IO b
bind = (>>=)