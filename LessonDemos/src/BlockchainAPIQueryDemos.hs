{-# LANGUAGE DeriveGeneric #-}
module BlockchainAPIQueryDemos where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import Data.Text (Text)
import Control.Concurrent.Async
import Data.Maybe

data LatestBlock = LatestBlock {
  block_index :: Int 
} deriving (Show, Generic)

instance FromJSON LatestBlock
instance ToJSON LatestBlock

getJSONResponse :: (FromJSON j) => String -> IO j
getJSONResponse url = do
  json <- simpleHttp url 
  return $ fromJust $ decode json

blockhainAPIBase = "https://blockchain.info"
latestBlockURLPath = "/latestblock"
uncofirmedTransactionsURLPath = "/unconfirmed-transactions?format=json"

{-
a blockhain data block has:
  size :: Int
  height :: Int
  time :: Int
  n_tx :: Int
 -}