module Dnsnitch ( main ) where

import           Control.Concurrent (forkIO)

import qualified Dnsnitch.Cache     as Cache
import qualified Dnsnitch.DNS       as DNS
import qualified Dnsnitch.HTTP      as HTTP


main :: Int -> Int -> IO ()
main dnsPort httpPort = do
  -- Cache with key-value expiration after N seconds
  cache <- Cache.newCache 20

  _ <- forkIO $ DNS.dnsMain dnsPort cache
  HTTP.httpMain httpPort cache
