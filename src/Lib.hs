{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runApp
    ) where

import Network.Wreq
import Control.Lens
import Control.Applicative
-- import Data.Map as DMap
import Data.ByteString.Lazy hiding (putStrLn, map)
import Data.Aeson.Lens (_String, key )
import Data.Aeson (Value)

-- type Resp = Response -- (Map String Value)

-- data String
-- data Code = String
-- data Price = Float
-- data MarketCap = Int
-- data PoE = Float
-- data ChgPct = Float
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

createQuote :: Name -> Quote
createQuote n = Quote n "XYZ"

-- getQuote :: String -> Quote
-- getQuote = extractQuoteData (createQuote  fetchQuoteData)

-- getQuotes :: [String] -> [Quote]
-- getQuotes t = map getQuote t

-- runApp :: IO ()

extractBody x =
  (^. responseBody) <$> fetchQuoteData x

getQuote y =
  extractBody y

other n [] = n
other n (x:xs) = other (createQuote x:n) xs

fprint :: Quote -> IO()
fprint x = print x

-- runApp :: IO()
runApp =
  sequence_ . map fprint $ (other [] ["AAPL", "GOOGL"])
  -- putStrLn "test"
  -- map putStrLn r
  -- getQuote "AAPL" >>= print

  -- putStrLn (show (doThings "AAPL"))
  -- (>>= print) <*> (^. responseBody) <$> map fetchQuoteData ["AAPL"] 

  -- let tickers = ["TNE", "WSA", "GXY"]
  -- r <- fetchQuoteData "TNE"

  -- let opts = defaults & param "s" .~ ["AAPL"] & param "f" .~ ["nsabrp6j1"]
  -- r <- getWith opts "http://finance.yahoo.com/d/quotes.csv"

  -- let opts = defaults & param "foo" .~ ["bar", "quux"]
  -- r <- getWith opts "http://httpbin.org/get"
  -- let s = r ^. responseBody
  -- putStrLn (show s)
  -- putStrLn "test"

