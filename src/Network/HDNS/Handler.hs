module Network.HDNS.Handler ( ) where

import Network.DNS.Lookup
import Network.DNS.Resolver
import Network.DNS.Types(DNSError)
import Data.IP (IPv4, IPv6)

import qualified Data.ByteString.Char8 as BS

lookup :: IO (Either DNSError [IPv4])
lookup = do
  let hostname = BS.pack "www.example.com"
      conf = defaultResolvConf { resolvInfo = RCHostName "8.8.8.8" }
  rs <- makeResolvSeed conf
  withResolver rs $ \resolver -> lookupA resolver hostname
