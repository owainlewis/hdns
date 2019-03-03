module Network.HDNS.Server
  ( start )
  where

import Control.Concurrent (forkFinally)
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString
import Control.Monad(forever)

resolveAddr :: Int -> IO AddrInfo
resolveAddr port = do
    let hints = defaultHints
              { addrFlags = [ AI_ADDRCONFIG
                            , AI_PASSIVE
                            ]
              , addrSocketType = Datagram
              }
    addr:_ <- getAddrInfo (Just hints) Nothing (Just . show $ port)
    return addr

start :: IO ()
start = withSocketsDo $ do
    addrinfo <- resolveAddr 53
    sock <- socket (addrFamily addrinfo) Datagram defaultProtocol
    bind sock (addrAddress addrinfo)
    forever $ do
      (conn, peer) <- accept sock
      putStrLn $ "Connection from " ++ show peer
