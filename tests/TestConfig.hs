{-# LANGUAGE OverloadedStrings #-}

module TestConfig where

import Data.Map
import Data.Maybe
import Data.OpenSRS.Types
import Data.Time

-- Please change all the settings and domains below so you can test against
-- your environment.

testConfig :: SRSConfig
testConfig = SRSConfig "https://horizon.opensrs.net:55443" "janedoe" "0123456789abcdef" "127.0.0.1"

-- | Lookup domain for an available domain
lookupDomainAvailable :: String
lookupDomainAvailable = "thisdomainshouldnotexist-argleblargle.com"

-- | Lookup domain for an unavailable domain
lookupDomainUnavailable :: String
lookupDomainUnavailable = "adomainyouownalready.com"

-- | Get domain we own
getDomainOwned :: String
getDomainOwned = "adomainyouownalready.com"

-- | Get domain we don't own
getDomainNotOurs :: String
getDomainNotOurs = "google.com"

-- | Get domain we don't own
renewTest :: (String, String, Int)
renewTest = ("automatedtest-domainrenewal-123.com", "12345", 2015)

-- | Registration test
testRegistrationDomain :: IO (Domain, Bool, Maybe String, Maybe String, Bool, Bool, Bool, Bool, Int, SRSUsername, Password, RegistrationType, Maybe (Map String (Map String String)))
testRegistrationDomain = do
    d <- domain
    return (d,
            True, Nothing, Nothing,
            False, False, False, True,
            2, un, pwd,
            NewRegistration,
            Nothing)
  where
    domain = do
        t <- getCurrentTime
        let t'  = Just t
        let t'' = Just $ addUTCTime ((86400 * 365) :: NominalDiffTime) t
        return $ Domain  "testingdomainregistration124foo.com"
                         True
                         testContacts
                         t'
                         True
                         t'
                         (Just "5534")
                         True
                         t''
                         testNameservers
    un = fromJust $ makeUsername "webmaster"
    pwd = fromJust $ makePassword "aTestingPassWord123"

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
                    (Just "geoffrey.roberts@anchor.com.au")
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

-- | Password test domain
passwordTestDomain :: String
passwordTestDomain = "automatedtest-passwordchange123.com"

-- | Valid password
validPassword :: String
validPassword = "chumbleSpuzz123"

-- | Invalid password
invalidPassword :: String
invalidPassword = "Hi; I do not work!"

-- | Cookie test credentials
cookieTest :: (String, SRSUsername, Password)
cookieTest = ("automatedtest-cookietest-123.com",
	(fromJust $ makeUsername "webmaster"),
	(fromJust $ makePassword "aVerySimplePassword"))

-- | Modify tests
modifyTest :: String
modifyTest = "automatedtest-whoisprivacy-123.com"
