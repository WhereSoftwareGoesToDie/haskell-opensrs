{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Either.Utils
import Data.Map
import Data.Maybe
import Data.OpenSRS
import Data.OpenSRS.Types
import Data.Time
import Test.Hspec
import Test.Hspec.Expectations
import Text.XmlHtml

import TestConfig

suite :: Spec
suite = do
    describe "Domain Lookup/Get" $ do
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

    describe "Domain Registration" $ do
        it "registers a domain" $ do
            (a, b, c, d, e, f, g, h, i, j, k, l, m) <- testRegistrationDomain
            let req = RegisterDomain testConfig a b c d e f g h i j k l m
            res <- doRequest req
            putStrLn $ show res
            case res of
                Right (DomainRegistrationResult d) -> do
                    putStrLn $ show d
                    pass
                Left e                      -> error $ e
                _                           -> error "This should never happen."

    describe "Passwords" $ do
        it "can change to a valid password" $ do
            let pwd = fromJust $ makePassword validPassword
            (unPassword pwd) `shouldBe` validPassword
            let req = ChangeDomainPassword testConfig passwordTestDomain pwd
            res <- doRequest req
            case res of
                Right (GenericSuccess _) -> pass
                Left e                   -> error $ e
                _                        -> error "This should never happen."
        
        it "cannot use an invalid password" $ do
            let pwd = makePassword invalidPassword
            pwd `shouldBe` Nothing

        it "can send passwords to a domain contact" $ do
            let req = SendDomainPassword testConfig passwordTestDomain "admin" False
            res <- doRequest req
            case res of
                Right (GenericSuccess _) -> pass
                Left e                   -> error $ e
                _                        -> error "This should never happen."

-- | Explicitly pass a test.
pass :: Expectation
pass = return ()

main :: IO ()
main = do
    res <- hspec suite
    return ()

