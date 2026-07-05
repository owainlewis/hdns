{-# LANGUAGE OverloadedStrings #-}

module Network.HDNS.Config
  ( LogLevel(..)
  , ServerConfig(..)
  , UpstreamConfig(..)
  , defaultServerConfig
  , usage
  , parseArgs
  , loadRecordStore
  , parseRecordLine
  ) where

import Data.Char (isSpace, toLower)
import Data.Word (Word32)
import Network.Socket (HostName)
import Text.Read (readMaybe)

import qualified Data.ByteString.Char8 as BS
import qualified Network.DNS as DNS

import Network.HDNS.Handler (RecordStore, recordsFromList)

data LogLevel
  = LogQuiet
  | LogInfo
  | LogDebug
  deriving (Eq, Ord, Show)

data UpstreamConfig = UpstreamConfig
  { upstreamHost :: HostName
  , upstreamPort :: Int
  } deriving (Eq, Show)

data ServerConfig = ServerConfig
  { configBindHost :: HostName
  , configBindPort :: Int
  , configUpstream :: Maybe UpstreamConfig
  , configLogLevel :: LogLevel
  , configRecordsFile :: Maybe FilePath
  , configMaxUdpSize :: Int
  } deriving (Eq, Show)

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { configBindHost = "127.0.0.1"
  , configBindPort = 1053
  , configUpstream = Nothing
  , configLogLevel = LogInfo
  , configRecordsFile = Nothing
  , configMaxUdpSize = 1232
  }

usage :: String
usage = unlines
  [ "hdns-exe [options]"
  , ""
  , "Options:"
  , "  --bind HOST             Bind host. Default: 127.0.0.1"
  , "  --port PORT             UDP port. Default: 1053"
  , "  --upstream HOST         Forward misses to this resolver"
  , "  --upstream-port PORT    Upstream resolver port. Default: 53"
  , "  --no-forward            Disable forwarding"
  , "  --records FILE          Local records file"
  , "  --log-level LEVEL       quiet, info, or debug. Default: info"
  , "  --max-udp-size BYTES    Server UDP response limit. Default: 1232"
  , "  --help                  Show this help"
  ]

parseArgs :: [String] -> Either String ServerConfig
parseArgs args = do
  (config, pendingPort) <- go defaultServerConfig Nothing args
  case (configUpstream config, pendingPort) of
    (Nothing, Just _) -> Left "--upstream-port requires --upstream"
    _ -> Right config
  where
    go cfg pendingPort [] = Right (cfg, pendingPort)
    go cfg pendingPort ("--bind" : host : xs) = go cfg { configBindHost = host } pendingPort xs
    go cfg pendingPort ("--port" : value : xs) = do
      port <- parsePort "--port" value
      go cfg { configBindPort = port } pendingPort xs
    go cfg pendingPort ("--upstream" : value : xs) =
      go cfg { configUpstream = Just (withPendingPort pendingPort (parseUpstream value)) } pendingPort xs
    go cfg _ ("--upstream-port" : value : xs) = do
      port <- parsePort "--upstream-port" value
      case configUpstream cfg of
        Nothing -> go cfg (Just port) xs
        Just upstream ->
          go cfg { configUpstream = Just upstream { upstreamPort = port } } (Just port) xs
    go cfg _ ("--no-forward" : xs) =
      go cfg { configUpstream = Nothing } Nothing xs
    go cfg pendingPort ("--records" : path : xs) =
      go cfg { configRecordsFile = Just path } pendingPort xs
    go cfg pendingPort ("--log-level" : value : xs) = do
      level <- parseLogLevel value
      go cfg { configLogLevel = level } pendingPort xs
    go cfg pendingPort ("--max-udp-size" : value : xs) = do
      size <- parseBoundedInt "--max-udp-size" 512 65535 value
      go cfg { configMaxUdpSize = size } pendingPort xs
    go _ _ ("--help" : _) = Left usage
    go _ _ (flag : _) = Left ("unknown option: " <> flag <> "\n\n" <> usage)

withPendingPort :: Maybe Int -> UpstreamConfig -> UpstreamConfig
withPendingPort Nothing upstream = upstream
withPendingPort (Just port) upstream = upstream { upstreamPort = port }

parseUpstream :: String -> UpstreamConfig
parseUpstream value =
  case break (== ':') value of
    (host, ':' : portText)
      | Just port <- readMaybe portText -> UpstreamConfig host port
    _ -> UpstreamConfig value 53

parseLogLevel :: String -> Either String LogLevel
parseLogLevel value =
  case map toLower value of
    "quiet" -> Right LogQuiet
    "info" -> Right LogInfo
    "debug" -> Right LogDebug
    _ -> Left "log level must be quiet, info, or debug"

parsePort :: String -> String -> Either String Int
parsePort name = parseBoundedInt name 1 65535

parseBoundedInt :: String -> Int -> Int -> String -> Either String Int
parseBoundedInt name low high value =
  case readMaybe value of
    Just n | n >= low && n <= high -> Right n
    _ -> Left (name <> " must be between " <> show low <> " and " <> show high)

loadRecordStore :: Maybe FilePath -> IO (Either String RecordStore)
loadRecordStore Nothing = pure (Right (recordsFromList []))
loadRecordStore (Just path) = do
  contents <- readFile path
  pure $ recordsFromList <$> parseRecords contents

parseRecords :: String -> Either String [DNS.ResourceRecord]
parseRecords contents =
  fmap concat . traverse parseNumberedLine . zip [1 :: Int ..] $ lines contents
  where
    parseNumberedLine (lineNo, line) =
      case parseRecordLine line of
        Right Nothing -> Right []
        Right (Just record) -> Right [record]
        Left err -> Left (show lineNo <> ": " <> err)

parseRecordLine :: String -> Either String (Maybe DNS.ResourceRecord)
parseRecordLine raw =
  case words (stripComment raw) of
    [] -> Right Nothing
    [_] -> Left "record requires at least name, type, and value"
    (name : ttlText : typText : valueParts)
      | Just ttl <- readMaybe ttlText -> Just <$> mkRecord name ttl typText valueParts
    (name : typText : valueParts) ->
      Just <$> mkRecord name 300 typText valueParts

mkRecord :: String -> Word32 -> String -> [String] -> Either String DNS.ResourceRecord
mkRecord name ttl typText valueParts =
  case map toLower typText of
    "a" -> oneValue "A" valueParts $ \value ->
      DNS.ResourceRecord domain DNS.A DNS.classIN ttl . DNS.RD_A <$> parseRead "IPv4" value
    "aaaa" -> oneValue "AAAA" valueParts $ \value ->
      DNS.ResourceRecord domain DNS.AAAA DNS.classIN ttl . DNS.RD_AAAA <$> parseRead "IPv6" value
    "cname" -> oneValue "CNAME" valueParts $ \value ->
      Right (DNS.ResourceRecord domain DNS.CNAME DNS.classIN ttl (DNS.RD_CNAME (normalizeDomain value)))
    "txt" ->
      case valueParts of
        [] -> Left "TXT record requires text"
        _ -> Right (DNS.ResourceRecord domain DNS.TXT DNS.classIN ttl (DNS.RD_TXT (BS.pack (stripQuotes (unwords valueParts)))))
    _ -> Left ("unsupported local record type: " <> typText)
  where
    domain = normalizeDomain name

oneValue :: String -> [String] -> (String -> Either String DNS.ResourceRecord) -> Either String DNS.ResourceRecord
oneValue typ parts build =
  case parts of
    [value] -> build value
    [] -> Left (typ <> " record requires a value")
    _ -> Left (typ <> " record requires one value")

parseRead :: Read a => String -> String -> Either String a
parseRead label value =
  maybe (Left ("invalid " <> label <> ": " <> value)) Right (readMaybe value)

stripComment :: String -> String
stripComment = trim . takeWhile (/= '#')

stripQuotes :: String -> String
stripQuotes value =
  case value of
    '"' : rest | not (null rest) && last rest == '"' -> init rest
    _ -> value

normalizeDomain :: String -> DNS.Domain
normalizeDomain =
  BS.pack . ensureDot . map toLower . trim
  where
    ensureDot value
      | null value = "."
      | last value == '.' = value
      | otherwise = value <> "."

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse
