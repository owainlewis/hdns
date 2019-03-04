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

import qualified Network.DNS.Decode as Decode
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
            (packet, _) <- recvFrom sock 65535
            forkIO $ dnsHandler sock packet
            return ()

-- | Handling incoming DNS message requests
dnsHandler :: Socket -> BS.ByteString -> IO ()
dnsHandler sock packet = do
    let dnsMsg = Decode.decode packet
    case dnsMsg of
        Left e -> print "Error parsing DNS message" >> abort
        Right m -> print . show $ m
    where
        abort = myThreadId >>= killThread
