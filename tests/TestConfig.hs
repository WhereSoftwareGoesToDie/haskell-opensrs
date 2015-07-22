{-# LANGUAGE OverloadedStrings #-}

module TestConfig where

import Data.Map
import Data.Maybe
import Data.OpenSRS.Types
import Data.Time

-- | Testing contacts
testContacts :: Map String Contact
testContacts = fromList [("owner", testc),
                         ("admin", testc),
                         ("billing", testc),
                         ("tech", testc)]
  where
    testc = Contact (Just "Jane")
                    (Just "Doe")
                    (Just "Frobozz Pty Ltd")
                    (Just "jane.doe@anchor.com.au")
                    (Just "+61.299999999")
                    Nothing
                    (Just "Frobozz Pty Ltd")
                    (Just "Level 50")
                    (Just "1 George Street")
                    (Just "Sydney")
                    (Just "NSW")
                    (Just "2000")
                    (Just "AU")

-- | Testing nameservers
testNameservers :: [Nameserver]
testNameservers = [
    Nameserver (Just "ns1.example.com") (Just "0") (Just "127.0.0.1"),
    Nameserver (Just "ns2.example.com") (Just "0") (Just "127.0.0.2") ]

-- | Valid password
validPassword :: String
validPassword = "automatedTest124"

-- | Invalid password
invalidPassword :: String
invalidPassword = "Hi; I am trash!"
