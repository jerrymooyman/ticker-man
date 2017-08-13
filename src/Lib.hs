{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( runApp
    , getQuote
    , byteStringToString
    , fetchQuoteData
    , parseCsv
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
import Data.Text.Encoding
import qualified Data.Csv as C
import qualified Data.Vector as V
import GHC.Generics

type Name = String
type Code = String

data Quote = Quote {
  name :: String
, symbol :: String
, ask :: Float
, bid :: Float
, pe :: Float
, pb :: Float
, mcap :: String
}
  -- Name-- name
  -- Code -- code
              -- Maybe Price
              -- Maybe MarketCap
              -- Maybe PoE
              -- Maybe ChgPct
              deriving (Show, Generic)

instance C.FromRecord Quote
instance C.ToRecord Quote
-- instance C.FromRecord Quote where
--   parseRecord v
--     | length v == 1 = Quote <$>
--                       v C..! 0
--     | otherwise = mzero

-- format string options
-- n   : name
-- s   : symbol
-- a   : ask price
-- b   : bid price
-- r   : p/e ratio
-- p6  : p/book ratio
-- j1  : market cap
fetchQuoteData :: T.Text -> IO (Response BS.ByteString)
fetchQuoteData a =
  let opts = defaults & param "s" .~ [a] & param "f" .~ ["nsabrp6j1"]
  -- let opts = defaults & param "s" .~ [a] & param "f" .~ ["nsab"]
  in getWith opts "http://finance.yahoo.com/d/quotes.csv"


createQuote :: String -> Quote
createQuote n = Quote n "xx" 0 0 0 0 "xx"


byteStringToString :: BS.ByteString -> String
byteStringToString = BS.unpack
byteStringToString' = BL.unpack

extractBody :: Response BS.ByteString -> String
extractBody = byteStringToString . (^. responseBody)
extractBody' :: Response a -> a
extractBody' = (^. responseBody)

parseCsv :: IO()
parseCsv = do
  r <- fetchQuoteData "AAPL"
  let csvData = extractBody' r
  case C.decode C.NoHeader csvData of
    Left err -> putStrLn err
    Right v -> V.forM_ v $ \ (Quote name symbol ask bid pe pb mcap) -> print $ Quote name symbol ask bid pe pb mcap
    -- Right v -> V.forM_ v $ \ (name, symbol, ask bid pe pb mcap -> return (createQuote "test")

getQuote :: T.Text -> IO Quote
getQuote y = do
  r <- fetchQuoteData y
  let csvData = extractBody r
  return (createQuote csvData)
  -- case C.decode C.NoHeader csvData of
  --   Left err -> return (createQuote "nothing")
  --   Right v -> return (createQuote "xxx")
    -- Right v -> V.forM_ v $ \ name symbol ask bid pe pb mcap -> return (createQuote "test")


fprint :: IO Quote -> IO()
fprint y = y >>= print


runApp :: IO()
runApp =
  mapM_ fprint $ fmap getQuote ["AAPL", "GOOGL"]
