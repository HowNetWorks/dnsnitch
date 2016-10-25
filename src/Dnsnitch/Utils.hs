{-# LANGUAGE OverloadedStrings #-}

module Dnsnitch.Utils
  ( DotOrDash(..)
  , addrToText
  , addrToByteString
  )
where

import           Data.ByteString         (ByteString)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text
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
