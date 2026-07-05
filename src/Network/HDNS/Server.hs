{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.HDNS.Server
  ( start
  , startWithConfig
  ) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (bracket)
import Control.Monad (forever, void, when)
import Network.Socket hiding (openSocket, shutdown)
import Network.Socket.ByteString (recvFrom, sendAllTo)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (Handler(Catch), installHandler, keyboardSignal, softwareTermination)

import qualified Data.ByteString as BS
import qualified Network.DNS as DNS

import Network.HDNS.Config
import Network.HDNS.Handler (RecordStore, UpstreamLookup, handlePacket)

start :: IO ()
start = do
  args <- getArgs
  when ("--help" `elem` args) $ do
    putStr usage
    exitSuccess
  case parseArgs args of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right config -> do
      loaded <- loadRecordStore (configRecordsFile config)
      case loaded of
        Left err -> hPutStrLn stderr ("failed to load records: " <> err) >> exitFailure
        Right store -> startWithConfig config store

startWithConfig :: ServerConfig -> RecordStore -> IO ()
startWithConfig config store =
  withSocketsDo $ do
    addr <- resolveAddr (configBindHost config) (configBindPort config)
    withResolver config $ \resolver ->
      bracket (openSocket addr) close $ \sock -> do
        bind sock (addrAddress addr)
        logInfo config ("listening on " <> show (addrAddress addr))
        logInfo config ("forwarding " <> upstreamDescription config)
        runLoop config store resolver sock

withResolver :: ServerConfig -> (Maybe DNS.Resolver -> IO a) -> IO a
withResolver config action =
  case configUpstream config of
    Nothing -> action Nothing
    Just upstream -> do
      seed <- DNS.makeResolvSeed DNS.defaultResolvConf
        { DNS.resolvInfo = DNS.RCHostPort (upstreamHost upstream) (fromIntegral (upstreamPort upstream))
        , DNS.resolvConcurrent = True
        }
      DNS.withResolver seed (action . Just)

resolveAddr :: HostName -> Int -> IO AddrInfo
resolveAddr host port = do
  let passive = host == "*" || host == "0.0.0.0" || host == "::"
      hints = defaultHints
        { addrFlags = [AI_PASSIVE | passive]
        , addrSocketType = Datagram
        }
      node = if passive then Nothing else Just host
  addrs <- getAddrInfo (Just hints) node (Just (show port))
  case addrs of
    addr : _ -> pure addr
    [] -> ioError (userError ("no address found for " <> host <> ":" <> show port))

openSocket :: AddrInfo -> IO Socket
openSocket addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  pure sock

runLoop :: ServerConfig -> RecordStore -> Maybe DNS.Resolver -> Socket -> IO ()
runLoop config store resolver sock = do
  stopSignal <- newEmptyMVar
  installShutdownHandlers stopSignal
  let upstream = fmap upstreamLookup resolver
      receiveLimit = min 65535 (max 512 (configMaxUdpSize config))
      loop = forever $ do
        received <- recvFrom sock receiveLimit
        void . forkFinally (servePacket config store upstream sock received) $
          either (logDebug config . ("request failed: " <>) . show) (const (pure ()))
  loopThread <- forkFinally loop $ \_ -> void (tryPutMVar stopSignal ())
  takeMVar stopSignal
  logInfo config "shutting down"
  killThread loopThread

servePacket :: ServerConfig -> RecordStore -> Maybe UpstreamLookup -> Socket -> (BS.ByteString, SockAddr) -> IO ()
servePacket config store upstream sock (packet, peer) = do
  response <- handlePacket (configMaxUdpSize config) store upstream packet
  let encoded = DNS.encode response
  sendAllTo sock encoded peer
  logDebug config ("served " <> show (BS.length packet) <> " bytes from " <> show peer)

upstreamLookup :: DNS.Resolver -> DNS.Question -> IO (Either DNS.DNSError DNS.DNSMessage)
upstreamLookup resolver question =
  DNS.lookupRaw resolver (DNS.qname question) (DNS.qtype question)

installShutdownHandlers :: MVar () -> IO ()
installShutdownHandlers stopSignal = do
  let requestShutdown = void (tryPutMVar stopSignal ())
  void $ installHandler keyboardSignal (Catch requestShutdown) Nothing
  void $ installHandler softwareTermination (Catch requestShutdown) Nothing

upstreamDescription :: ServerConfig -> String
upstreamDescription config =
  case configUpstream config of
    Nothing -> "disabled"
    Just upstream -> upstreamHost upstream <> ":" <> show (upstreamPort upstream)

logInfo :: ServerConfig -> String -> IO ()
logInfo config = logAt config LogInfo

logDebug :: ServerConfig -> String -> IO ()
logDebug config = logAt config LogDebug

logAt :: ServerConfig -> LogLevel -> String -> IO ()
logAt config level message =
  when (configLogLevel config >= level) $
    putStrLn ("hdns: " <> message)
