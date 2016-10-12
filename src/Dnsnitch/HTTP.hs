{-# LANGUAGE OverloadedStrings #-}

module Dnsnitch.HTTP where

import           Control.Monad.IO.Class  (liftIO)

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Lazy    as LBS

import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text

import           Web.Scotty

import qualified Dnsnitch.Cache          as Cache


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


httpMain :: Int -> Cache.DnsCache -> IO ()
httpMain port cache =
  scotty port $
  get "/:key" $ do
    key <- param "key"
    value <- liftIO $ Cache.lookup cache (textToBS key)
    case value of
      Just match ->
        text $ key `mappend` " " `mappend` bsToText match `mappend` "\n"
      Nothing ->
        text "nope\n"
