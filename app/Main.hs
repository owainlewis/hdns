module Main where

import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SBS

import Data.ByteString(ByteString)

import Control.Monad(forever)

import Control.Concurrent(forkIO)

import qualified Network.DNS.Decode as DNSDecode

main :: IO ()
main = Socket.withSocketsDo $ do
         (server:_) <- Socket.getAddrInfo Nothing (Just "localhost") (Just "53")
         sock <- Socket.socket (Socket.addrFamily server) Socket.Datagram Socket.defaultProtocol
         Socket.bind sock (Socket.addrAddress server) >> return sock
         putStrLn "Server started ..."
         dnsLoop sock

-- | Receive UDP packets and fork a new process to respond to DNS query
dnsLoop :: Socket.Socket -> IO ()
dnsLoop sock = forever $ do
  packet <- SBS.recvFrom sock 65535
  _ <- forkIO $ dnsHandler sock packet
  return ()

dnsHandler :: Socket.Socket -> (ByteString, Socket.SockAddr) -> IO ()
dnsHandler socket packet = do
  let (bs, from) = packet
  print $ DNSDecode.decode bs
