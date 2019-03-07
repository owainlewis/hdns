{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Network.HDNS.Server
  ( start )
  where

import Control.Concurrent (forkFinally)
import Control.Monad(forever, when)
import Control.Concurrent(forkIO, killThread, myThreadId)

import Network.Socket.ByteString

import Network.Socket hiding (recvFrom)

import qualified Network.DNS as DNS
import qualified Data.ByteString as BS

resolveAddr :: Int -> IO AddrInfo
resolveAddr port = do
    let hints = defaultHints
              { addrFlags = [ AI_ADDRCONFIG
                            , AI_PASSIVE
                            ]
              , addrSocketType = Datagram
              }
    (addr :_) <- getAddrInfo (Just hints) Nothing (Just . show $ port)
    return addr

-- | Start the DNS server by binding to a network socket and forking each request
--   to a specific DNS handler
start :: IO ()
start = withSocketsDo $ do
    addr <- resolveAddr 53
    listenOn addr
    where
        listenOn :: AddrInfo -> IO ()
        listenOn addr = do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            bind sock (addrAddress addr)
            putStrLn $ "Starting DNS server on " ++ show (addrAddress addr)
            dnsLoop sock
        dnsLoop :: Socket -> IO ()
        dnsLoop sock = forever $ do
            (msg, addr) <- recvFrom sock 65535
            forkIO $ dnsHandler sock msg addr
            return ()

-- | Handling incoming DNS message requests
--
dnsHandler :: Socket -> BS.ByteString -> SockAddr -> IO ()
dnsHandler sock packet addr = do
    let dnsMsg = DNS.decode packet
    case dnsMsg of
        Left e -> print "Error parsing DNS message" >> abort
        Right m -> do
          result <- handleMessage m
          let response = DNS.encode result
          sendAllTo sock response addr
    where
        abort = myThreadId >>= killThread

-- | A handler that accepts a DNS message, does work and returns the response
--   to return.
--
handleMessage :: Monad m => DNS.DNSMessage -> m DNS.DNSMessage
handleMessage m = return m
