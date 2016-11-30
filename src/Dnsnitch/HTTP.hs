{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Dnsnitch.HTTP where

import           Control.Exception      (IOException, catch)
import           Control.Monad.IO.Class (liftIO)

import           Data.Aeson             (ToJSON (..), object, (.=))
import           Data.ByteString        (ByteString)
import           Data.Maybe             (fromMaybe)
import qualified Data.Set               as Set
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy         as Text
import qualified Data.Text.Lazy.IO      as TextIO
import           Data.Time.Clock        (UTCTime (..), getCurrentTime)

import qualified Network.Socket         as Socket

import           GHC.Generics

import           Network.Wai            (remoteHost)
import           Web.Scotty

import qualified Dnsnitch.Cache         as Cache
import           Dnsnitch.Utils


newtype Result = Result [DnsResult]

instance ToJSON Result where
  toJSON (Result dnsResults) =
        object ["data" .= dnsResults]


data DnsResult = DnsResult
  { ip   :: Text
  , name :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON DnsResult


httpMain :: Int -> Cache.DnsCache -> IO ()
httpMain port cache = scotty port $
  get "/:key" $ do
    setHeader "Access-Control-Allow-Origin" "*"
    key <- param "key"
    values <- liftIO $ lookupIp cache (textToBS key)

    -- IP address from HTTP connection
    req <- request
    let requestIp = addrToText Dot (remoteHost req)

    -- IP address from X-Forwarded-For header
    xffHdr <- header "X-Forwarded-For"
    let xff = fmap (Text.takeWhile (/=',')) xffHdr
    let clientIP = fromMaybe requestIp xff

    time <- liftIO getCurrentTime
    liftIO . TextIO.putStrLn $ logLine time clientIP values

    dnsResults <- liftIO $ mapM toDnsResult values
    json (Result dnsResults)


logLine :: UTCTime -> Text -> [Socket.SockAddr] -> Text
logLine time client dns =
  Text.concat [ isoTime, " dnsnitch-http: ", client, " [", dnsList, "]" ]
  where
    isoTime = iso8601 time
    dnsList = Text.intercalate ", " (map (addrToText Dot) dns)


lookupIp :: Cache.DnsCache -> ByteString -> IO [Socket.SockAddr]
lookupIp cache key = do
  values <- Cache.lookup cache key
  return (Set.toList values)


toDnsResult :: Socket.SockAddr -> IO DnsResult
toDnsResult value = do
  let ipAddr = addrToText Dot value
  hostName <- resolveName value
  return $! DnsResult { ip = ipAddr, name = hostName }


resolveName :: Socket.SockAddr -> IO (Maybe Text)
resolveName addr = do
  -- Force getNameInfo to throw exception if hostname is can't be
  -- resolved and return Nothing. Otherwise getNameInfo would return
  -- IP address as String.
  (hostName, _) <- catch
    (Socket.getNameInfo [Socket.NI_NAMEREQD] True False addr)
    (\e -> let _ = e :: IOException
           in return (Nothing, Nothing))
  return $ fmap Text.pack hostName
