{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runApp
    ) where

import Network.Wreq
import Control.Lens
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

data Quote = Quote
  String -- name
  String -- code
              -- Maybe Price
              -- Maybe MarketCap
              -- Maybe PoE
              -- Maybe ChgPct
              deriving (Show)

-- fetchQuoteData :: String -> IO (Response ByteString)
fetchQuoteData a =
  let opts = defaults & param "foo" .~ ["bar", "quux"]
  in getWith opts "http://httpbin.org/get"

createQuote :: IO (Response ByteString) -> Quote
createQuote a =
  Quote "a" (show (a ^. responseStatus . statusMessage))

getQuote :: String -> Quote
getQuote = createQuote . fetchQuoteData

getQuotes :: [String] -> [Quote]
getQuotes t = map getQuote t

runApp :: IO ()
runApp = do
  let s = getQuotes ["TNE", "WSA", "GXY"]
  -- let opts = defaults & param "foo" .~ ["bar", "quux"]
  -- r <- getWith opts "http://httpbin.org/get"
  -- let s = r ^. responseBody
  putStrLn (show s)

