{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Network.HDNS.Server as Server

main :: IO ()
main = Server.start
