{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Either.Utils
import Data.Map
import Data.OpenSRS
import Data.OpenSRS.Types
import Data.Time
import Test.Hspec
import Test.Hspec.Expectations
import Text.XmlHtml

import TestConfig

suite :: Spec
suite = do
    describe "Domains" $ do
        it "looks up a valid free domain" $ do
            let req = LookupDomain testConfig lookupDomainAvailable
            res <- doRequest req
            res `shouldBe` (Right $ DomainAvailabilityResult $ Available lookupDomainAvailable)

        it "looks up a valid taken domain" $ do
            let req = LookupDomain testConfig lookupDomainUnavailable
            res <- doRequest req
            res `shouldBe` (Right $ DomainAvailabilityResult $ Unavailable lookupDomainUnavailable)

        it "gets a valid existing domain" $ do
            let req = GetDomain testConfig getDomainOwned
            res <- doRequest req
            case res of
                Right (DomainResult d) -> do
                    (domainName d) `shouldBe` getDomainOwned
                Left e                      -> error $ e
                _                           -> error "This should never happen."

        it "gets a valid existing domain that we don't own" $ do
            let req = GetDomain testConfig getDomainNotOurs
            res <- doRequest req
            (fromLeft res) `shouldContain` "415: Authentication Error."

-- | Explicitly pass a test.
pass :: Expectation
pass = return ()

main :: IO ()
main = do
    res <- hspec suite
    return ()

