module Dnsnitch.Cache
  ( DnsCache
  , newCache
  , Cache.insert
  , Cache.lookup
  )
where

import           Data.ByteString (ByteString)
import qualified Data.Cache      as Cache
import           System.Clock    (TimeSpec (..))

type DnsCache = Cache.Cache ByteString ByteString


newCache :: Integer -> IO DnsCache
newCache seconds = Cache.newCache (Just expire)
  where
    s = fromIntegral seconds
    expire = TimeSpec { sec = s, nsec = 0 }

