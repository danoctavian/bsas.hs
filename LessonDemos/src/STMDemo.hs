module STMDemo where
import System.IO
import Control.Concurrent.STM
{-
Taken from: https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency/3-software-transactional-memory
Credit goes to: Simon Peyton-Jones
-}

type Account = TVar Int

withdraw :: Account -> Int -> STM ()
withdraw acc amount = do
    bal <- readTVar acc
    writeTVar acc (bal - amount)

deposit :: Account -> Int -> STM ()
deposit acc amount = withdraw acc (- amount)

transfer :: Account -> Account -> Int -> IO ()
-- Transfer ’amount’ from account ’from’ to account ’to’
transfer from to amount 
    = atomically (do deposit to amount
                     withdraw from amount)

showAccount :: Account -> IO Int
showAccount acc = atomically (readTVar acc)

testRun = do
    from <- atomically (newTVar 200)
    to   <- atomically (newTVar 100)
    transfer from to 50
    v1 <- showAccount from
    v2 <- showAccount to
    putStrLn $ (show v1) ++ ", " ++ (show v2)



myTrans = do
    firstTVar <- atomically (newTVar 5)
    readTVar firstTVar
    createdValue <- atomically $ do
        value <- readTVar firstTVar
        
        writeTVar firstTVar 4
        return value
    putStrLn (show createdValue)
