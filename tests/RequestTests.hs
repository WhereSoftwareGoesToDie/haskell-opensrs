{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BSL8
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
            let req = LookupDomain testConfig lookupDomain
            res <- doRequest req
            putStrLn $ show res
            pass

        it "gets a valid existing domain" $ do
            let req = GetDomain testConfig getDomain
            res <- doRequest req
            putStrLn $ show res
            pass

-- | Explicitly pass a test.
pass :: Expectation
pass = return ()

main :: IO ()
main = do
    res <- hspec suite
    return ()

