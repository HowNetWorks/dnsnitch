module Dnsnitch.Cache
  ( DnsCache
  , newCache
  , Cache.insert
  , Cache.lookup
  )
where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (forever)

import           Data.ByteString    (ByteString)
import qualified Data.Cache         as Cache
import           System.Clock       (TimeSpec (..))

type DnsCache = Cache.Cache ByteString ByteString


-- | Create new DNS Cache
--
-- Cached items have default expiration of x seconds. Also, cache is
-- periodically purged of expired items after 4*n seconds.
--
newCache :: Integer -> IO DnsCache
newCache seconds = do
  cache <- Cache.newCache (Just expire)
  _ <- forkIO $ purgeTimer (4 * seconds) cache
  return cache
  where
    s = fromIntegral seconds
    expire = TimeSpec { sec = s, nsec = 0 }


-- | Purge cache periodically
--
-- Purge cache between x seconds. This should be forked into own
-- thread to be ran in background.
--
-- Data.Cache uses STM which is not fair. Purge will slow things down
-- and not get ran if DNS requests flood the cache with inserts.
--
purgeTimer :: Integer -> DnsCache -> IO ()
purgeTimer seconds cache = forever $ do
    threadDelay (1000000 * fromIntegral seconds)
    Cache.purgeExpired cache
