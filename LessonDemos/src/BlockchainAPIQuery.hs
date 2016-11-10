{-# LANGUAGE DeriveGeneric #-}
module BlockchainAPIQuery where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import Data.Text (Text)
import Control.Concurrent.Async
import Data.Maybe
import Control.Monad


data LatestBlock = LatestBlock {
  block_index :: Int 
} deriving (Show, Generic)

instance FromJSON LatestBlock
instance ToJSON LatestBlock

data Block = Block {
  size :: Int,
  height :: Int,
  time :: Int,
  n_tx :: Int
} deriving (Show, Generic)

instance FromJSON Block
instance ToJSON Block

getJSONResponse :: (FromJSON j) => String -> IO j
getJSONResponse url = do
  json <- simpleHttp url 
  return $ fromJust $ decode json


fetchBlockchainDataAsync = do
  lastBlockFetch <- async $ simpleHttp "https://blockchain.info/latestblock"
  unconfirmedTransactionsFetch <- async $ simpleHttp "https://blockchain.info/unconfirmed-transactions?format=json"

  lastBlock <- wait lastBlockFetch
  putStrLn $ show lastBlock
  unconfirmedTransactions <- wait unconfirmedTransactionsFetch
  putStrLn $ show unconfirmedTransactions
  return ()

fetchBlockchainDataAsync' = do
  (lastBlock, unconfirmedTransactions) <- concurrently
      (simpleHttp "https://blockchain.info/latestblock")
      (simpleHttp "https://blockchain.info/unconfirmed-transactions?format=json")

  putStrLn $ show lastBlock
  putStrLn $ show unconfirmedTransactions

queryForLatestBlocks = do
  let baseUrl = "https://blockchain.info/"
  latestBlock <- getJSONResponse (baseUrl ++ "latestblock")
  let latestBlockIndex = block_index latestBlock
  putStrLn $ show latestBlockIndex

  let latestBlocksCount = 2
  --  async

  latestBlockData <- getJSONResponse (baseUrl ++ "/rawblock/" ++ (show $ block_index latestBlock)) :: IO Block
  putStrLn $ show latestBlockData

  latestBlocks <- forConcurrently [latestBlockIndex - latestBlocksCount..latestBlockIndex] $ \blockIndex -> do
    getJSONResponse (baseUrl ++ "/rawblock/" ++ (show blockIndex))

  putStrLn (show $ size $ head latestBlocks)
 -- <- simpleHttp (baseUrl + "rawblock/" + (show $ block_index latestBlock))
