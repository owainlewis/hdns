{-# LANGUAGE OverloadedStrings #-}

module Network.HDNS.Handler
  ( RecordStore
  , UpstreamLookup
  , recordsFromList
  , emptyRecordStore
  , handleMessage
  , handlePacket
  , decodeFailureResponse
  , responseLimitFor
  ) where

import Data.Bits (shiftL, (.|.))
import Data.Char (toLower)
import Data.Map.Strict (Map)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Network.DNS as DNS

newtype RecordStore = RecordStore (Map DNS.Domain [DNS.ResourceRecord])
  deriving (Eq, Show)

type UpstreamLookup = DNS.Question -> IO (Either DNS.DNSError DNS.DNSMessage)

recordsFromList :: [DNS.ResourceRecord] -> RecordStore
recordsFromList =
  RecordStore . Map.fromListWith (<>) . fmap (\record -> (normalizeDomain (DNS.rrname record), [record]))

emptyRecordStore :: RecordStore
emptyRecordStore = recordsFromList []

handlePacket :: Int -> RecordStore -> Maybe UpstreamLookup -> BS.ByteString -> IO DNS.DNSMessage
handlePacket maxUdpSize store upstream packet =
  case DNS.decode packet of
    Left _ -> pure (decodeFailureResponse packet)
    Right message -> handleMessage maxUdpSize store upstream message

handleMessage :: Int -> RecordStore -> Maybe UpstreamLookup -> DNS.DNSMessage -> IO DNS.DNSMessage
handleMessage maxUdpSize store upstream request =
  limitResponse maxUdpSize request <$> handleQuery store upstream request

handleQuery :: RecordStore -> Maybe UpstreamLookup -> DNS.DNSMessage -> IO DNS.DNSMessage
handleQuery store upstream request
  | DNS.qOrR requestFlags /= DNS.QR_Query =
      pure (responseWith False request DNS.FormatErr [] False)
  | DNS.opcode requestFlags /= DNS.OP_STD =
      pure (responseWith False request DNS.NotImpl [] False)
  | DNS.rcode requestFlags /= DNS.NoErr =
      pure (responseWith False request DNS.FormatErr [] False)
  | otherwise =
      case DNS.question request of
        [question]
          | DNS.qtype question == DNS.AXFR ->
              pure (responseWith False request DNS.NotImpl [] False)
          | otherwise ->
              resolveQuestion store upstream request question
        _ ->
          pure (responseWith False request DNS.FormatErr [] False)
  where
    requestFlags = DNS.flags (DNS.header request)

resolveQuestion :: RecordStore -> Maybe UpstreamLookup -> DNS.DNSMessage -> DNS.Question -> IO DNS.DNSMessage
resolveQuestion store upstream request question =
  case localAnswer store question of
    Just records ->
      pure (responseWith True request DNS.NoErr records (hasUpstream upstream))
    Nothing
      | domainExists store (DNS.qname question) ->
          pure (responseWith True request DNS.NoErr [] (hasUpstream upstream))
      | Just lookupUpstream <- upstream ->
          forward lookupUpstream request question
      | otherwise ->
          pure (responseWith True request DNS.NameErr [] False)

forward :: UpstreamLookup -> DNS.DNSMessage -> DNS.Question -> IO DNS.DNSMessage
forward lookupUpstream request question = do
  result <- lookupUpstream question
  pure $ case result of
    Left _ ->
      responseWith False request DNS.ServFail [] True
    Right upstreamResponse ->
      normalizeForwardedResponse request question upstreamResponse

normalizeForwardedResponse :: DNS.DNSMessage -> DNS.Question -> DNS.DNSMessage -> DNS.DNSMessage
normalizeForwardedResponse request question upstreamResponse =
  upstreamResponse
    { DNS.header = (DNS.header upstreamResponse)
        { DNS.identifier = DNS.identifier (DNS.header request)
        , DNS.flags = (DNS.flags (DNS.header upstreamResponse))
            { DNS.qOrR = DNS.QR_Response
            , DNS.recDesired = DNS.recDesired (DNS.flags (DNS.header request))
            , DNS.recAvailable = True
            }
        }
    , DNS.question = [question]
    , DNS.ednsHeader = responseEdns (DNS.ednsHeader request)
    }

localAnswer :: RecordStore -> DNS.Question -> Maybe [DNS.ResourceRecord]
localAnswer (RecordStore records) question =
  case DNS.qtype question of
    DNS.ANY -> nonEmpty domainRecords
    qtype -> nonEmpty (typedRecords qtype <> cnameRecords qtype)
  where
    domainRecords = Map.findWithDefault [] (normalizeDomain (DNS.qname question)) records
    typedRecords qtype = filter ((== qtype) . DNS.rrtype) domainRecords
    cnameRecords DNS.CNAME = []
    cnameRecords _ = filter ((== DNS.CNAME) . DNS.rrtype) domainRecords

domainExists :: RecordStore -> DNS.Domain -> Bool
domainExists (RecordStore records) domain =
  Map.member (normalizeDomain domain) records

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs

responseWith :: Bool -> DNS.DNSMessage -> DNS.RCODE -> [DNS.ResourceRecord] -> Bool -> DNS.DNSMessage
responseWith authoritative request code records recursionAvailable =
  DNS.defaultResponse
    { DNS.header = DNS.DNSHeader
        { DNS.identifier = DNS.identifier requestHeader
        , DNS.flags = requestFlags
            { DNS.qOrR = DNS.QR_Response
            , DNS.authAnswer = authoritative
            , DNS.trunCation = False
            , DNS.recAvailable = recursionAvailable
            , DNS.rcode = code
            , DNS.authenData = False
            }
        }
    , DNS.ednsHeader = responseEdns (DNS.ednsHeader request)
    , DNS.question = DNS.question request
    , DNS.answer = records
    , DNS.authority = []
    , DNS.additional = []
    }
  where
    requestHeader = DNS.header request
    requestFlags = DNS.flags requestHeader

decodeFailureResponse :: BS.ByteString -> DNS.DNSMessage
decodeFailureResponse packet =
  DNS.defaultResponse
    { DNS.header = DNS.DNSHeader
        { DNS.identifier = packetIdentifier packet
        , DNS.flags = (DNS.flags (DNS.header DNS.defaultResponse))
            { DNS.authAnswer = False
            , DNS.recAvailable = False
            , DNS.rcode = DNS.FormatErr
            }
        }
    , DNS.question = []
    , DNS.answer = []
    , DNS.authority = []
    , DNS.additional = []
    }

limitResponse :: Int -> DNS.DNSMessage -> DNS.DNSMessage -> DNS.DNSMessage
limitResponse maxUdpSize request response
  | BS.length encoded <= limit = response
  | otherwise =
      response
        { DNS.header = (DNS.header response)
            { DNS.flags = (DNS.flags (DNS.header response)) { DNS.trunCation = True } }
        , DNS.answer = []
        , DNS.authority = []
        , DNS.additional = []
        , DNS.ednsHeader = responseEdns (DNS.ednsHeader request)
        }
  where
    limit = responseLimitFor maxUdpSize request
    encoded = DNS.encode response

responseLimitFor :: Int -> DNS.DNSMessage -> Int
responseLimitFor maxUdpSize request =
  max 512 (min maxUdpSize clientLimit)
  where
    clientLimit =
      case DNS.ednsHeader request of
        DNS.EDNSheader edns -> fromIntegral (DNS.ednsUdpSize edns)
        _ -> 512

responseEdns :: DNS.EDNSheader -> DNS.EDNSheader
responseEdns (DNS.EDNSheader edns) =
  DNS.EDNSheader edns
    { DNS.ednsDnssecOk = False
    , DNS.ednsOptions = []
    }
responseEdns _ = DNS.NoEDNS

packetIdentifier :: BS.ByteString -> DNS.Identifier
packetIdentifier packet =
  case BS.unpack (BS.take 2 packet) of
    [hi, lo] -> fromIntegral hi `shiftL` 8 .|. fromIntegral lo
    _ -> 0

normalizeDomain :: DNS.Domain -> DNS.Domain
normalizeDomain domain =
  case BSC.unsnoc lowered of
    Just (_, '.') -> lowered
    _ -> lowered <> "."
  where
    lowered = BSC.map toLower domain

hasUpstream :: Maybe UpstreamLookup -> Bool
hasUpstream = maybe False (const True)
