{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Map
import Data.Maybe
import Data.OpenSRS.ToXML
import Data.OpenSRS.Types
import Data.Time
import Test.Hspec
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Manipulators
import Text.XmlHtml

-- | API configuration to use in these (non-integration) tests.
testConfig :: SRSConfig
testConfig = SRSConfig "https://horizon.opensrs.net:55443" "janedoe" "0123456789abcdef" "127.0.0.1" True

testDomain1 :: IO Domain
testDomain1 = do
    t <- getCurrentTime
    let t' = addUTCTime ((86400 * 365) :: NominalDiffTime) t
    return $ Domain "foo.com" True contacts (Just t) True (Just t) (Just "5534") True (Just t') nameservers
  where
    contacts = fromList [
        ("owner", testc),
        ("admin", testc),
        ("billing", testc),
        ("tech", testc)
        ]
    testc = Contact (Just "Jane")
                    (Just "Doe")
                    (Just "Frobozz Pty Ltd")
                    (Just "jane.doe@frobozz.com.au")
                    (Just "+61.299999999")
                    Nothing
                    (Just "Frobozz Pty Ltd")
                    (Just "Level 50")
                    (Just "1 George Street")
                    (Just "Sydney")
                    (Just "NSW")
                    (Just "2000")
                    (Just "AU")
    nameservers = [
        Nameserver (Just "ns1.anchor.net.au") (Just "0") (Just "127.0.0.1"),
        Nameserver (Just "ns2.anchor.net.au") (Just "0") (Just "127.0.0.2") ]

testDoc1 :: String
testDoc1 = "<!DOCTYPE OPS_envelope SYSTEM \"ops.dtd\"><OPS_envelope><header><version>0.9</version></header><body><data_block><dt_assoc><item key=\"protocol\">XCP</item><item key=\"action\">SET</item><item key=\"object\">COOKIE</item><item key=\"registrant_ip\">127.0.0.1</item><item key=\"attributes\"><dt_assoc><item key=\"domain\">foo.com</item><item key=\"reg_username\">webmaster</item><item key=\"reg_password\">myLovelyHorse</item></dt_assoc></item></dt_assoc></data_block></body></OPS_envelope>"

reqXML :: SRSRequest -> String
reqXML = BSL8.unpack . toLazyByteString . render . requestXML

suite :: Spec
suite = do
    describe "XML" $ do
        it "can get a string using a tag as a source" $
            getText (parseTags testDoc1) "<version>" `shouldBe` "0.9"

        it "treats quotes in getText queries the same" $ do
            getText (parseTags testDoc1) "<item key='domain'>" `shouldBe` "foo.com"
            getText (parseTags testDoc1) "<item key=\"domain\">" `shouldBe` "foo.com"

        it "can get items within tree" $ do
            let xmlt = tagTree $ parseTags testDoc1
            let items = flattenTree . kidsWith "item" $ topMatching "<item key='attributes'>" xmlt
            getText' items "<item key='domain'>" `shouldBe` "foo.com"
            getText' items "<item key='reg_password'>" `shouldBe` "myLovelyHorse"

    describe "Domains" $ do
        it "Can be marshalled into a registration request" $ do
            d <- testDomain1
            let req = RegisterDomain testConfig d False Nothing Nothing
                                     False False True True 1
                                     (fromJust $ makeUsername "janedoe")
                                     (fromJust $ makePassword "imasecret")
                                     NewRegistration Nothing
            let rxml = reqXML req
            rxml `shouldContain` "<OPS_envelope>"

        it "Can be marshalled into an update request" $ do
            d <- testDomain1
            let req = UpdateDomain testConfig d
            let rxml = reqXML req
            rxml `shouldContain` "<OPS_envelope>"

main :: IO ()
main = hspec suite
