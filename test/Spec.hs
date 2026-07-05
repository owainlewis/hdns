{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec

import qualified Data.ByteString as BS
import qualified Network.DNS as DNS

import Network.HDNS.Config
import Network.HDNS.Handler

main :: IO ()
main = hspec $ do
  describe "record parsing" $ do
    it "parses local A records with an explicit TTL" $ do
      parseRecordLine "example.test. 60 A 192.0.2.10" `shouldBe`
        Right (Just (DNS.ResourceRecord "example.test." DNS.A DNS.classIN 60 (DNS.RD_A (read "192.0.2.10"))))

    it "ignores blank lines and comments" $ do
      parseRecordLine "  # local development zone" `shouldBe` Right Nothing

  describe "argument parsing" $ do
    it "allows upstream port before upstream host" $ do
      configUpstream <$> parseArgs ["--upstream-port", "5353", "--upstream", "127.0.0.1"] `shouldBe`
        Right (Just (UpstreamConfig "127.0.0.1" 5353))

    it "rejects upstream port without upstream host" $ do
      parseArgs ["--upstream-port", "5353"] `shouldSatisfy` either (const True) (const False)

  describe "handleMessage" $ do
    it "answers local A records and preserves the request id" $ do
      response <- handleMessage 1232 store Nothing (query 42 (DNS.Question "example.test." DNS.A))
      DNS.identifier (DNS.header response) `shouldBe` 42
      DNS.qOrR (DNS.flags (DNS.header response)) `shouldBe` DNS.QR_Response
      DNS.rcode (DNS.flags (DNS.header response)) `shouldBe` DNS.NoErr
      DNS.question response `shouldBe` [DNS.Question "example.test." DNS.A]
      DNS.answer response `shouldBe`
        [DNS.ResourceRecord "example.test." DNS.A DNS.classIN 60 (DNS.RD_A (read "192.0.2.10"))]

    it "returns NODATA when a local name exists with another type" $ do
      response <- handleMessage 1232 store Nothing (query 7 (DNS.Question "example.test." DNS.AAAA))
      DNS.rcode (DNS.flags (DNS.header response)) `shouldBe` DNS.NoErr
      DNS.answer response `shouldBe` []

    it "returns NXDOMAIN in local-only mode for unknown names" $ do
      response <- handleMessage 1232 store Nothing (query 8 (DNS.Question "missing.test." DNS.A))
      DNS.rcode (DNS.flags (DNS.header response)) `shouldBe` DNS.NameErr

    it "forwards misses and rewrites the upstream id" $ do
      let upstream _ = pure (Right (DNS.makeResponse 999 (DNS.Question "other.test." DNS.A) []))
      response <- handleMessage 1232 emptyRecordStore (Just upstream) (query 55 (DNS.Question "other.test." DNS.A))
      DNS.identifier (DNS.header response) `shouldBe` 55
      DNS.question response `shouldBe` [DNS.Question "other.test." DNS.A]
      DNS.recAvailable (DNS.flags (DNS.header response)) `shouldBe` True

    it "does not add EDNS to forwarded responses when the client did not ask for it" $ do
      let upstream _ = pure (Right ((DNS.makeResponse 999 (DNS.Question "other.test." DNS.A) [])
            { DNS.ednsHeader = DNS.EDNSheader DNS.defaultEDNS
            }))
      response <- handleMessage 1232 emptyRecordStore (Just upstream) (queryNoEdns 55 (DNS.Question "other.test." DNS.A))
      DNS.ednsHeader response `shouldBe` DNS.NoEDNS

    it "rejects multi-question messages as malformed" $ do
      let request = (query 9 (DNS.Question "one.test." DNS.A))
            { DNS.question =
                [ DNS.Question "one.test." DNS.A
                , DNS.Question "two.test." DNS.A
                ]
            }
      response <- handleMessage 1232 store Nothing request
      DNS.rcode (DNS.flags (DNS.header response)) `shouldBe` DNS.FormatErr

    it "marks oversized responses as truncated" $ do
      let largeStore = recordsFromList
            [ DNS.ResourceRecord "large.test." DNS.TXT DNS.classIN 60 (DNS.RD_TXT (BS.replicate 900 120))
            ]
      response <- handleMessage 512 largeStore Nothing (query 10 (DNS.Question "large.test." DNS.TXT))
      DNS.trunCation (DNS.flags (DNS.header response)) `shouldBe` True
      DNS.answer response `shouldBe` []

  describe "decodeFailureResponse" $ do
    it "returns FormErr and preserves the packet id when present" $ do
      let response = decodeFailureResponse (BS.pack [0x12, 0x34, 98, 97, 100])
      DNS.identifier (DNS.header response) `shouldBe` 0x1234
      DNS.rcode (DNS.flags (DNS.header response)) `shouldBe` DNS.FormatErr

store :: RecordStore
store = recordsFromList
  [ DNS.ResourceRecord "example.test." DNS.A DNS.classIN 60 (DNS.RD_A (read "192.0.2.10"))
  , DNS.ResourceRecord "alias.test." DNS.CNAME DNS.classIN 60 (DNS.RD_CNAME "example.test.")
  ]

query :: DNS.Identifier -> DNS.Question -> DNS.DNSMessage
query ident question =
  DNS.makeQuery ident question mempty

queryNoEdns :: DNS.Identifier -> DNS.Question -> DNS.DNSMessage
queryNoEdns ident question =
  DNS.makeQuery ident question (DNS.ednsEnabled DNS.FlagClear)
