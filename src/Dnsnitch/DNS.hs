{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Dnsnitch.DNS ( dnsMain ) where

import           Control.Monad             (unless, when)

import           Data.Bits                 (clearBit, setBit, shiftR, testBit,
                                            (.&.))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as Char8 (pack, unpack)
import           Data.Either               (isLeft)
import           Data.List                 (intercalate)
import           Data.Tuple                (swap)
import           Data.Word                 (Word16, Word32)

import           Network.Socket            (HostAddress, HostAddress6)
import qualified Network.Socket            as Socket hiding (recv, recvFrom,
                                                      send, sendTo)
import qualified Network.Socket.ByteString as Socket

import           Text.Printf               (printf)

-- From cereal
import           Data.Serialize            (Get, Putter, getBytes, getWord16be,
                                            getWord32be, getWord8, isolate,
                                            putByteString, putWord16be,
                                            putWord32be, putWord8, remaining,
                                            runGet, runPut)

-- From QuickCheck
import           Test.QuickCheck           (Arbitrary (..), Gen, choose,
                                            elements, oneof, vectorOf)

import qualified Dnsnitch.Cache            as Cache


-- | Entry point for DNS
--
-- Starts listening socket(s) on port given and loops forever handling
-- DNS requests.
--
dnsMain :: Int -> Cache.DnsCache -> IO ()
dnsMain port' cache = do
  let port = fromIntegral port'

  sock <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
  Socket.bind sock (Socket.SockAddrInet port 0)

  putStrLn "Listening.."
  dnsLoop sock cache


-- | This does stuff :)
dnsLoop :: Socket.Socket -> Cache.DnsCache -> IO ()
dnsLoop sock cache = do
  (bs, from) <- Socket.recvFrom sock 65535
  let request = runGet getMessage bs

  when (isLeft request) $ do
    -- Error in parsing message
    let (Left err) = request
    putStrLn $ show from ++ ": " ++ head (lines err)
    dnsLoop sock cache

  let (Right query) = request
  putStrLn $ show from ++ ": " ++ show query

  let qRRType' = qRRType . head . questions $ query
  unless (qRRType' `elem` [A, AAAA]) $ do
    -- Unsuppoted RRType
    putStrLn $ show from ++": unsupported request type " ++ show qRRType'
    dnsLoop sock cache

  let
    (DomainName qName) = question . head . questions $ query

    -- Key used for caching. ByteString from DNS labels before keyword
    -- "dnstest". For example DNS query to
    -- "random.key.dnstest.example.com" will produce "random.key"
    -- key in cache
    cacheKey = BS.intercalate "." . takeWhile (/= "dnstest") $ qName
    cacheValue = let
      (DomainName labels) = addrToDomainName from
      in BS.intercalate "." labels

    -- DNS response will be CNAME pointing to name like
    -- 198-51-100-42.dnsresult.example.com
    responseName = addrToDomainName from `mappend` (DomainName
                   . map (\l -> if l == "dnstest" then "dnsresult" else l)
                   . dropWhile (/= "dnstest")
                   $ qName)

    responseData = IN_CNAME responseName
    dnsResponse = makeResponse query responseData
    dnsPacket = runPut . putMessage $ dnsResponse

  _ <- Socket.sendTo sock dnsPacket from

  -- Cache only keys with minimum length to avoid cache poisoning
  when (BS.length cacheKey >= 51) $ do
    Cache.insert cache cacheKey cacheValue
    putStrLn $ "Cache insert: " ++ show cacheKey ++ " " ++ show cacheValue

  dnsLoop sock cache


-- | Produce DNS response message
--
-- Given DNS Query and response RData, produces new DNS Response
-- Message
--
makeResponse :: Message -> RData -> Message
makeResponse query responseData = responseMsg
  where
    id' = msgId query
    (question':_) = questions query
    (Question qName _) = question'
    responseRR = RR { name = qName, ttl = 10, rdata = responseData }
    responseMsg = Message
      { msgId = id'
      , questions = [question']
      , answers = [responseRR]
      , authority = []
      , additional = []
      }


-- | DNS Message
--
-- Whole DNS message. Contains needed parts from DNS Header, and then
-- questions, answers, authority and additional resource records.
--
data Message = Message
  { msgId      :: Word16
  , questions  :: [Question]
  , answers    :: [RR]
  , authority  :: [RR]
  , additional :: [RR]
  } deriving (Show)

instance Arbitrary Message where
  arbitrary = do
    msgId'     <- arbitrary
    questions' <- arbitrary
    rrs        <- arbitrary
    return $! Message msgId' questions' rrs rrs rrs


-- | Get DNS Message
--
-- This parses the whole DNS message from start to finish.
--
getMessage :: Get Message
getMessage = do
  -- RFC1035 4.1.1. Header section format
  id' <- getWord16be
  bitfield <- getWord16be

  -- Test QR bit
  when (testBit bitfield 16) $
    fail "DNS Message is not Query (QR)"

  -- Test OPCODE bits
  when (bitfield `clearBit` 15 `shiftR` 11 /= 0) $
    fail "DNS Message is not Query (OPCODE)"

  -- Test RCODE bits
  when (bitfield .&. 15 /= 0) $
    fail "DNS Message indicates error (RCODE)"

  questionCount   <- getWord16be
  answerCount     <- getWord16be
  authorityCount  <- getWord16be
  additionalCount <- getWord16be

  -- RFC1035 4.1.2. Question
  questions'  <- getTimes questionCount getQuestion

  -- RFC1035 4.1.3. Resource records
  answers'    <- getTimes answerCount getRR
  authority'  <- getTimes authorityCount getRR
  additional' <- getTimes additionalCount getRR

  return $! Message id' questions' answers' authority' additional'


putMessage :: Putter Message
putMessage msg = do
  let bitfield = 0
        `setBit` 15 -- Set QR bit: Response
        `setBit` 10 -- Set AA bit: We are authority

  putWord16be (msgId msg)
  putWord16be bitfield

  let itemCount f = fromIntegral . length . f $ msg
  putWord16be . itemCount $ questions
  putWord16be . itemCount $ answers
  putWord16be . itemCount $ authority
  putWord16be . itemCount $ additional

  mapM_ putQuestion (questions msg)
  mapM_ putRR (answers msg)
  mapM_ putRR (authority msg)
  mapM_ putRR (additional msg)


-- | Domain Name (list of DNS labels)
--
-- Implemented as a list of strict ByteStrings.
data DomainName = DomainName [ByteString] deriving Eq

instance Monoid DomainName where
  mempty = DomainName mempty
  (DomainName l1) `mappend` (DomainName l2) = DomainName (l1 `mappend` l2)

instance Show DomainName where
  show (DomainName labels) = intercalate "." . map Char8.unpack $ labels

instance Arbitrary DomainName where
  -- labelCount and labelLen are chosen so that the result is under
  -- 255 bytes.
  arbitrary = do
    labelCount <- choose (1, 4)
    labels <- vectorOf labelCount label
    return $ DomainName labels
    where
      label :: Gen ByteString
      label = do
        labelLen <- choose (1, 62)
        fmap Char8.pack (vectorOf labelLen arbitrary)


-- | Get DomainName
--
-- >>> let name = makeDomainName "fine.example.com"
-- >>>     bs = runPut (putDomainName name)
-- >>> runGet getDomainName bs
-- Right fine.example.com
--
-- prop> Right xs == runGet getDomainName (runPut (putDomainName xs))
--
getDomainName :: Get DomainName
getDomainName = do
  labels <- getLabels [] True

  let size = foldl (\total label -> total + BS.length label + 1) 1 labels
  when (size > 255) (fail "DomainName too long")

  return $! DomainName labels

  where
    getLabels as False = return $! reverse as
    getLabels as True = do
      len <- fmap fromIntegral getWord8
      if | len == 0  -> getLabels as False
         | len >= 64 -> fail "DNS Label too long"
         | otherwise -> do
             bs <- getBytes len
             getLabels (bs:as) True


putDomainName :: Putter DomainName
putDomainName (DomainName labels) = do
  mapM_ putLabel labels
  putWord8 0
  where
    putLabel bs = do
      let len = (fromIntegral . BS.length) bs
      when (len == 0) $ fail "Empty label not allowed"
      when (len >= 64) $ fail "Oversized label not allowed"
      putWord8 len
      putByteString bs


-- | Resource Record Type
--
-- This is combination of both DNS Class (IN) and DNS Type.
--
data RRType
  = A
  | CNAME
  | AAAA
  -- ^ https://tools.ietf.org/html/rfc3596
  | OPT
  -- ^ https://tools.ietf.org/html/rfc6891
  | UnknownRRType Word16 Word16
  deriving (Eq, Show)

instance Arbitrary RRType where
  arbitrary = elements [A, CNAME, AAAA]


-- | List of ((DNS Class, DNS Type), RRType)
rrtypes :: [((Word16, Word16), RRType)]
rrtypes =
  [ ((1,  1), A)
  , ((1,  5), CNAME)
  , ((1, 28), AAAA)
  ]

-- | List of (RRType, (Class, Id))
rrtypes' :: [(RRType, (Word16, Word16))]
rrtypes' = map swap rrtypes


-- | Get Resource Record Type
--
-- >>> let bs = runPut (putRRType A)
-- >>> runGet getRRType bs
-- Right A
--
-- prop> Right xs == runGet getRRType (runPut (putRRType xs))
--
getRRType :: Get RRType
getRRType = do
  typ <- getWord16be
  cls <- getWord16be
  case lookup (cls, typ) rrtypes of
    Just match -> return match
    Nothing    -> return $ UnknownRRType cls typ


-- | Put Resource Record Type
--
putRRType :: Putter RRType
putRRType rrtype' =
  case lookup rrtype' rrtypes' of
    Just (cls, typ) -> do
      putWord16be typ
      putWord16be cls
    Nothing -> fail $ "RRType lookup error (" ++ show rrtype' ++ ")"


-- | DNS Question
--
-- RFC1035 4.1.2. Question section format
--
data Question = Question
  { question :: DomainName
  , qRRType  :: RRType
  }
  deriving (Eq, Show)

instance Arbitrary Question where
  arbitrary = do
    question' <- arbitrary
    rrtype'   <- arbitrary
    return $ Question question' rrtype'


-- | Get DNS Question
--
-- >>> let question = Question (makeDomainName "example.com") A
-- >>> let bs = runPut (putQuestion question)
-- >>> runGet getQuestion bs
-- Right (Question {question = example.com, qRRType = A})
--
-- prop> Right xs == runGet getQuestion (runPut (putQuestion xs))
--
getQuestion :: Get Question
getQuestion = do
  name'   <- getDomainName
  rrtype' <- getRRType
  return $! Question name' rrtype'


putQuestion :: Putter Question
putQuestion (Question name' rrtype') = do
  putDomainName name'
  putRRType rrtype'


-- | DNS Resource Record
--
-- DNS Resource Record containing the name, type (DNS Class and Type
-- combined), Time To Live and resource data.
--
data RR = RR
  { name  :: DomainName
  , ttl   :: Word32
  , rdata :: RData
  } deriving (Eq, Show)

instance Arbitrary RR where
  arbitrary = do
    name'  <- arbitrary
    ttl'   <- arbitrary
    rdata' <- arbitrary
    return $ RR name' ttl' rdata'


-- | Get Resource Record
--
-- >>> let name' = makeDomainName "outstanding.example.com"
-- >>> let rdata' = IN_CNAME (makeDomainName "excellent.example.com")
-- >>> let rr = RR { name = name', ttl = 64, rdata = rdata' }
-- >>> let bs = runPut (putRR rr)
-- >>> runGet getRR bs
-- Right (RR {name = outstanding.example.com, ttl = 64, rdata = IN_CNAME excellent.example.com})
--
-- prop> Right xs == runGet getRR (runPut (putRR xs))
--
getRR :: Get RR
getRR = do
  name'   <- getDomainName
  rrtype' <- getRRType
  ttl'    <- getWord32be
  len     <- fmap fromIntegral getWord16be
  rdata'  <- isolate len (getRData rrtype')
  return $! RR name' ttl' rdata'


putRR :: Putter RR
putRR record = do
  let rrtype' = case rdata record of
        (IN_A _)           -> A
        (IN_AAAA _)        -> AAAA
        (IN_CNAME _)       -> CNAME
        (IN_UNKNOWN typ _) -> typ
      rdata' = runPut . putRData $ rdata record
  putDomainName (name record)
  putRRType rrtype'
  putWord32be (ttl record)
  putWord16be (fromIntegral . BS.length $ rdata')
  putByteString rdata'


-- | DNS Resources Data
--
-- Type is selected from DNS RRType
--
data RData
  = IN_CNAME DomainName
  | IN_A HostAddress
  | IN_AAAA HostAddress6
  | IN_UNKNOWN RRType ByteString
  deriving (Show, Eq)

instance Arbitrary RData where
  arbitrary = oneof
    [ fmap IN_CNAME arbitrary
    , fmap IN_A arbitrary
    , fmap IN_AAAA arbitrary
    ]


-- | Get Resource Data
--
getRData :: RRType -> Get RData
getRData A     = fmap IN_A getWord32be
getRData CNAME = fmap IN_CNAME getDomainName
getRData AAAA  = do
  [a1, a2, a3, a4] <- getTimes (4::Int) getWord32be
  return $! IN_AAAA (a1, a2, a3, a4)
getRData typ = do
  len <- remaining
  bytes <- getBytes len
  return $ IN_UNKNOWN typ bytes


putRData :: Putter RData
putRData (IN_CNAME cname) = putDomainName cname
putRData (IN_A addr)      = putWord32be addr
putRData (IN_AAAA (a1, a2, a3, a4)) = do
  putWord32be a1
  putWord32be a2
  putWord32be a3
  putWord32be a4
putRData (IN_UNKNOWN _ bytes) = putByteString bytes

-- | Convert SockAddr to DomainName
addrToDomainName :: Socket.SockAddr -> DomainName
addrToDomainName (Socket.SockAddrInet _ addr) = makeDomainName str
  where
    (b1, b2, b3, b4) = Socket.hostAddressToTuple addr
    str = printf "%d-%d-%d-%d" b1 b2 b3 b4
addrToDomainName (Socket.SockAddrInet6 _ _ addr _) = makeDomainName str
  where
    (b1, b2, b3, b4, b5, b6, b7, b8) = Socket.hostAddress6ToTuple addr
    str = printf "%x-%x-%x-%x-%x-%x-%x-%x" b1 b2 b3 b4 b5 b6 b7 b8
addrToDomainName _ = error "Undefined type for addrToDomainName"


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
splitWith pred' list =
  let
    (first, rest) = span pred' list
  in
    first : splitWith pred' (drop 1 rest)


-- | Convert host name into DomainName
--
-- >>> makeDomainName "this.is.test.example.com"
-- this.is.test.example.com
--
makeDomainName :: String -> DomainName
makeDomainName domainName =
  let labels = map Char8.pack (splitWith (/='.') domainName)
  in DomainName labels


-- | Repeat getter n times
--
-- >>> runGet (getTimes 2 getWord8) (Char8.pack "1234")
-- Right [49,50]
--
getTimes :: Integral i => i -> Get g -> Get [g]
getTimes 0 _ = return []
getTimes n getter = go [] n
  where
    go as 0 = return $! reverse as
    go as m = do
      bs <- getter
      go (bs:as) (m-1)
