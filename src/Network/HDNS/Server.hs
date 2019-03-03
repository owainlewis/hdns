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

--
--
--loop sock conn def 0
  -- where
  --   loop sock conn def' n = do
  --     (bs, addr) <- recvFrom sock (bufSize def')
  --     _ <- forkIO $ handlePacket conf def' conn sock addr bs n
  --     case n of
  --       255 -> loop sock conn def' 0
  --       _ -> loop sock conn def' (n+1)
