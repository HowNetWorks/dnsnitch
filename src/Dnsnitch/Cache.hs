module Dnsnitch.Cache (
  -- * Creating a cache
    DnsCache
  , newCache

  -- * Append items
  , append

  -- * Querying
  , lookup
  )
where

import           Prelude            hiding (lookup)

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (forever)

import qualified Data.Cache         as Cache
import           Data.Hashable      (Hashable)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Text          (Text)

import qualified Network.Socket     as Socket

import           System.Clock       (TimeSpec (..))

type DnsCache = Cache.Cache Text (Set.Set Socket.SockAddr)


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


-- | Append new element into value
--
append :: (Eq k, Hashable k, Ord v) => Cache.Cache k (Set v) -> k -> v -> IO ()
append cache key value = do
  match <- Cache.lookup cache key
  case match of
    Nothing  -> Cache.insert cache key (Set.singleton value)
    Just set -> Cache.insert cache key (Set.insert value set)


-- | Lookup elements
--
-- Lookup will delete found key from cache
--
lookup :: (Eq k, Hashable k) => Cache.Cache k (Set v) -> k -> IO (Set v)
lookup cache key = do
  values <- Cache.lookup cache key
  case values of
    Nothing  -> return Set.empty
    Just set -> do
      Cache.delete cache key
      return set


-- | Purge cache periodically
--
-- Purge cache between x seconds. This should be forked into own
-- thread to be ran in background.
--
-- Data.Cache uses STM which is not fair. Purge will slow things down
-- and not get ran if DNS requests flood the cache with inserts.
--
purgeTimer :: Integer -> DnsCache -> IO ()
purgeTimer seconds cache = forever $ do
    threadDelay (1000000 * fromIntegral seconds)
    Cache.purgeExpired cache
