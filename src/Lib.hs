{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runApp
    ) where

import Debug.Trace
import Network.Wreq
import Control.Lens
import Control.Applicative
import Control.Monad
-- import Data.Map as DMap
import Data.ByteString.Lazy hiding (putStrLn, map)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson.Lens (_String, key )
import Data.Aeson (Value)

-- type Resp = Response -- (Map String Value)

type Name = String
type Code = String

data Quote = Quote
  Name-- name
  Code -- code
              -- Maybe Price
              -- Maybe MarketCap
              -- Maybe PoE
              -- Maybe ChgPct
              deriving (Show)

-- fetchQuoteData :: String -> IO (Response ByteString)
fetchQuoteData a =
  let opts = defaults & param "s" .~ [a] & param "f" .~ ["nsabrp6j1"]
  in getWith opts "http://finance.yahoo.com/d/quotes.csv"


createQuote :: String -> Quote
createQuote n = Quote n "XYZ"


byteStringToString = BS.unpack

extractBody :: Response ByteString -> String
extractBody = byteStringToString . (^. responseBody)


getQuote y = do
  r <- fetchQuoteData y
  let q = extractBody r
  return (createQuote q)


fprint :: IO Quote -> IO()
fprint q = q >>= print


runApp :: IO()
runApp =
  sequence_ . map fprint $ (fmap getQuote ["AAPL", "GOOGL"])
