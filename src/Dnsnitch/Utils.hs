{-# LANGUAGE OverloadedStrings #-}

module Dnsnitch.Utils
  ( DotOrDash(..)
  , addrToText
  , addrToByteString
  , anonymizeIP
  , unsafeToSockAddr
  -- Convert between Text and ByteString
  , textToBS
  , bsToText
  --
  , splitWith
  --
  , iso8601
  )
where

import           Control.Exception       (IOException, catch)

import           Data.ByteString         (ByteString)
import           Data.ByteString.Lazy    (fromStrict, toStrict)
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text
import           Data.Time.Clock         (UTCTime)
import           Data.Time.Format        (defaultTimeLocale, formatTime)
import qualified Network.Socket          as Socket

import           Text.Printf             (printf)

import           System.IO.Unsafe        (unsafePerformIO)

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


-- | Anonymize address
--
-- This uses same strategy as Google Analytics[0] where the last octet
-- of IPv4 addresses and the last 80 bits of IPv6 addresses are set to
-- zeros.
--
-- >>> let (Right addr) = unsafeToSockAddr "192.0.2.42"
-- >>> anonymizeIP addr
-- 192.0.2.0:0
--
-- >>> let (Right addr6) = unsafeToSockAddr "2001:db8:cafe::1:2:3"
-- >>> anonymizeIP addr6
-- [2001:db8:cafe::]:0
--
-- [0]: https://support.google.com/analytics/answer/2763052
--
anonymizeIP :: Socket.SockAddr -> Socket.SockAddr
anonymizeIP (Socket.SockAddrInet port addr) =
  Socket.SockAddrInet port anonymizedAddr
  where
    (b1, b2, b3, _) = Socket.hostAddressToTuple addr
    anonymizedAddr = Socket.tupleToHostAddress (b1, b2, b3, 0)

anonymizeIP (Socket.SockAddrInet6 port flow addr scope) =
  Socket.SockAddrInet6 port flow anonymizedAddr scope
  where
    (b1, b2, b3, _, _, _, _, _) = Socket.hostAddress6ToTuple addr
    anonymizedAddr = Socket.tupleToHostAddress6 (b1, b2, b3, 0, 0, 0, 0, 0)

anonymizeIP _ = error "Undefined type for anonymizeIP"


-- | Convert IP address from String into SockAddr
--
-- >>> unsafeToSockAddr "192.0.2.42"
-- Right 192.0.2.42:0
--
-- >>> unsafeToSockAddr "2001:db8:cafe::1:2:3"
-- Right [2001:db8:cafe::1:2:3]:0
--
-- >>> unsafeToSockAddr "hello world"
-- Left ...
--
unsafeToSockAddr :: String -> Either String Socket.SockAddr
unsafeToSockAddr ipAddr =
  let value = unsafePerformIO (execIO ipAddr)
  in
    case value of
      Left err -> Left err
      Right addr ->
        case Socket.addrAddress addr of
          ip4@Socket.SockAddrInet{}  -> Right ip4
          ip6@Socket.SockAddrInet6{} -> Right ip6
          _                          -> Left "Unsupported SockAddr instance"
  where
    execIO :: String -> IO (Either String Socket.AddrInfo)
    execIO str =
      catch (do
                addr:_ <- Socket.getAddrInfo Nothing (Just str) Nothing
                return (Right addr))
      (\e -> let _ = e :: IOException
             in return $ Left (show e))
{-# NOINLINE unsafeToSockAddr #-}


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
