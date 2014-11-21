{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Char
import Data.Either.Utils
import Data.Map
import Data.Maybe
import Data.OpenSRS
import Data.OpenSRS.Types
import Data.Time
import Data.UUID
import Data.UUID.V5
import System.Locale (defaultTimeLocale)
import Test.Hspec
import Test.Hspec.Expectations
import Text.XmlHtml

import TestConfig

makeDomainName :: String
makeDomainName = toString (generateNamed namespaceURL seed) ++ ".com"
  where
    seed = Prelude.map (fromIntegral . ord) "buttfart"

makeDomain :: IO Domain
makeDomain = do
    let dname = makeDomainName
    t <- getCurrentTime
    let t' = addUTCTime ((86400 * 365) :: NominalDiffTime) t
    return $ Domain dname True testContacts (Just t) True (Just t) (Just "5534") True (Just t') testNameservers

suite :: Spec
suite = do
    describe "Domain List" $ do
        it "can list domains with expiry in a given period" $ do
            t <- getCurrentTime
            let sd = addUTCTime ((-86400 * 365 * 20) :: NominalDiffTime) t
            let ed = addUTCTime ((86400 * 365 * 20) :: NominalDiffTime) t
            let req = ListDomainsByExpiry testConfig sd ed 0 5000
            res <- doRequest req
            case res of
                Right (DomainListResult (DomainList count items _)) -> do
                    (length items) `shouldBe` count
                Left e                      -> error $ e
                _                           -> error "This should never happen."

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
            -- putStrLn $ show res
            case res of
                Right (DomainRegistrationResult d) -> do
                    -- putStrLn $ show d
                    pass
                Left e                      -> error $ e
                _                           -> error "This should never happen."

    describe "Domain Registration" $ do
        it "renews a randomly created domain" $ do
            domain <- makeDomain
            let changeContact = False
            let comments      = Nothing
            let encoding      = Nothing
            let lock          = False
            let park          = False
            let whois         = False
            let handleNow     = True
            let regPeriod     = 1
            let username      = fromJust $ makeUsername "webmaster"
            let password      = fromJust $ makePassword "testPassword123"
            let regType       = NewRegistration
            let tldData       = Nothing

            -- Stage 1: Register domain
            let req = RegisterDomain testConfig domain changeContact comments encoding lock park whois handleNow regPeriod username password regType tldData
            res <- doRequest req
            putStrLn $ show res
            case res of
                Right (DomainRegistrationResult _) -> do
                    -- Stage 2: Get domain
                    let req = GetDomain testConfig (domainName domain)
                    res <- doRequest req
                    putStrLn $ show res
                    case res of
                        Right (DomainResult d2) -> do
                            -- Stage 3: Renew domain
                            let expyear = read $ formatTime defaultTimeLocale "%Y" $ fromJust $ domainExpireDate d2
                            let req = RenewDomain testConfig (domainName d2) (domainAutoRenew d2) (fromJust $ domainAffiliateID d2) expyear True 1
                            res <- doRequest req
                            putStrLn $ show res
                            case res of
                                Right (DomainRenewalResult d) -> do
                                    putStrLn $ show d
                                    pass
                                Left e                      -> error $ e
                                _                           -> error "This should never happen."

                        Left e                      -> error $ e
                        _                           -> error "This should never happen."

                Left e                      -> error $ e
                _                           -> error "This should never happen."

        it "renews a domain" $ do
            let (dname, affid, expyear) = renewTest
            let req = RenewDomain testConfig dname False affid expyear True 1
            res <- doRequest req
            -- putStrLn $ show res
            case res of
                Right (DomainRenewalResult d) -> do
                    -- putStrLn $ show d
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

    describe "Cookies" $ do
        it "can get a cookie for a valid domain logon" $ do
            let (d, u, p) = cookieTest
            let req = SetCookie testConfig d u p
            res <- doRequest req
            case res of
                Right (CookieResult _) -> pass
                Left e                 -> error $ e
                _                      -> error "This should never happen."

        it "gets a domain using a cookie" $ do
            let (d, u, p) = cookieTest
            let req = SetCookie testConfig d u p
            res <- doRequest req
            case res of
                Right (CookieResult jar) -> do
                    let req2 = GetDomainWithCookie testConfig d (cookieStr jar)
                    res2 <- doRequest req2
                    case res2 of
                        Right (DomainResult d') -> do
                            (domainName d') `shouldBe` d
                        Left e                      -> error $ e
                        _                           -> error "This should never happen."
                Left e                 -> error $ e
                _                      -> error "This should never happen."

    describe "Domain Updates" $ do
        it "can set whois privacy" $ do
            -- @TODO: toggle on and off
            let req = ModifyDomain testConfig modifyTest False (fromList [("data", "whois_privacy_state"), ("state", "enable")]) Nothing
            res <- doRequest req
            res `shouldBe` (Right $ GenericSuccess "Whois Privacy successfully enabled")

-- | Explicitly pass a test.
pass :: Expectation
pass = return ()

main :: IO ()
main = do
    res <- hspec suite
    return ()

