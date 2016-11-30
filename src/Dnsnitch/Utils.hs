{-# LANGUAGE OverloadedStrings #-}

module Dnsnitch.Utils
  ( DotOrDash(..)
  , addrToText
  , addrToByteString
  -- Convert between Text and ByteString
  , textToBS
  , bsToText
  --
  , splitWith
  --
  , iso8601
  )
where

import           Data.ByteString         (ByteString)
import           Data.ByteString.Lazy    (fromStrict, toStrict)
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text
import           Data.Time.Clock         (UTCTime)
import           Data.Time.Format        (defaultTimeLocale, formatTime)
import qualified Network.Socket          as Socket

import           Text.Printf             (printf)

data DotOrDash = Dot | Dash


-- | Convert SockAddr to Text
--
-- >>> addrToText Dot (Socket.SockAddrInet 1 0x0100007f)
-- "127.0.0.1"
--
addrToText :: DotOrDash -> Socket.SockAddr -> Text
addrToText dod (Socket.SockAddrInet _ addr) = Text.pack str
  where
    (b1, b2, b3, b4) = Socket.hostAddressToTuple addr
    str = case dod of
      Dash -> printf "%d-%d-%d-%d" b1 b2 b3 b4
      Dot  -> printf "%d.%d.%d.%d" b1 b2 b3 b4


addrToText dod (Socket.SockAddrInet6 _ _ addr _) = Text.pack str
  where
    (b1, b2, b3, b4, b5, b6, b7, b8) = Socket.hostAddress6ToTuple addr
    str = case dod of
      Dash -> printf "%x-%x-%x-%x-%x-%x-%x-%x" b1 b2 b3 b4 b5 b6 b7 b8
      Dot  -> printf "%x:%x:%x:%x:%x:%x:%x:%x" b1 b2 b3 b4 b5 b6 b7 b8

addrToText _ _ = error "Undefined type for addrToText"


-- | Convert SockAddr to ByteString
addrToByteString :: DotOrDash -> Socket.SockAddr -> ByteString
addrToByteString dod addr = toStrict (Text.encodeUtf8 (addrToText dod addr))


-- | Convert Lazy Text to Strict UTF8 ByteString
--
-- >>> :set -XOverloadedStrings
-- >>> "test" == (textToBS . bsToText) "test"
-- True
--
textToBS :: Text.Text -> ByteString
textToBS = toStrict . Text.encodeUtf8


-- | Convert Strict UTF8 ByteString to Lazy ByteString
--
-- >>> :set -XOverloadedStrings
-- >>> "test" == (bsToText . textToBS) "test"
-- True
--
bsToText :: ByteString -> Text.Text
bsToText = Text.decodeUtf8 . fromStrict


-- | Split list
--
-- >>> splitWith (/='.') "1.2.3.4"
-- ["1","2","3","4"]
--
-- >>> splitWith (/='.') "abcd.efgh."
-- ["abcd","efgh"]
--
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith pred' list = first : splitWith pred' (drop 1 rest)
  where
    (first, rest) = span pred' list


-- | Format time to ISO8601
--
-- The output is "YYYY-MM-DDTHH:MM:SS.sssZ"
--
iso8601 :: UTCTime -> Text
iso8601 utc =
  -- YYYY-MM-DDTHH:MM:SS.ssssss..."
  -- \_____ 23 chars ______/
  Text.pack $ take 23 str ++ "Z"
  where
    str = formatTime defaultTimeLocale "%FT%T%Q" utc
