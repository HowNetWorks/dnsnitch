{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Dnsnitch.HTTP where

import           Control.Monad.IO.Class  (liftIO)

import           Data.Aeson              (ToJSON)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Set                as Set
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text

import           GHC.Generics

import           Web.Scotty

import qualified Dnsnitch.Cache          as Cache


data DnsResult = DnsResult
  { ip   :: Text
  , name :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON DnsResult


lookupIp :: Cache.DnsCache -> ByteString -> IO [DnsResult]
lookupIp cache key = do
  values <- Cache.lookup cache key
  return $ map toDnsResult (Set.toList values)
  where
    toDnsResult value = DnsResult { ip = bsToText value, name = Nothing }


httpMain :: Int -> Cache.DnsCache -> IO ()
httpMain port cache = scotty port $
  get "/:key" $ do
    setHeader "Access-Control-Allow-Origin" "*"
    key <- param "key"
    values <- liftIO $ lookupIp cache (textToBS key)
    json values


-- | Convert Lazy Text to Strict UTF8 ByteString
--
-- >>> :set -XOverloadedStrings
-- >>> "test" == (textToBS . bsToText) "test"
-- True
--
textToBS :: Text.Text -> ByteString
textToBS = LBS.toStrict . Text.encodeUtf8


-- | Convert Strict UTF8 ByteString to Lazy ByteString
--
-- >>> :set -XOverloadedStrings
-- >>> "test" == (bsToText . textToBS) "test"
-- True
--
bsToText :: ByteString -> Text.Text
bsToText = Text.decodeUtf8 . LBS.fromStrict
