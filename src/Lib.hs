{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runApp
    , getQuote
    , byteStringToString
    , fetchQuoteData
    ) where

import Debug.Trace
import Network.Wreq
import Control.Lens
import Control.Applicative
import Control.Monad
-- import Data.Map as DMap
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson.Lens (_String, key )
import Data.Aeson (Value)
import qualified Data.Text as T
import qualified Data.Csv as C
import qualified Data.Vector as V

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

fetchQuoteData :: T.Text -> IO (Response BS.ByteString)
fetchQuoteData a =
  let opts = defaults & param "s" .~ [a] & param "f" .~ ["nsabrp6j1"]
  in getWith opts "http://finance.yahoo.com/d/quotes.csv"


createQuote :: String -> Quote
createQuote n = Quote n "XYZ"


byteStringToString :: BS.ByteString -> String
byteStringToString = BS.unpack

extractBody :: Response BS.ByteString -> String
extractBody = byteStringToString . (^. responseBody)

getQuote :: T.Text -> IO Quote
getQuote y = do
  r <- fetchQuoteData y
  let q = extractBody r
  return (createQuote q)


fprint :: IO Quote -> IO()
fprint = \y -> y >>= print


runApp :: IO()
runApp =
  sequence_ . map fprint $ (fmap getQuote ["AAPL", "GOOGL"])
