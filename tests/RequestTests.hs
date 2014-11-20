{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either.Utils
import Data.Map
import Data.Maybe
import Data.OpenSRS
import Data.Time
import Test.Hspec

import TestConfig

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
            res <- doRequest $ GetDomain testConfig getDomainOwned
            case res of
                Right (DomainResult d) ->
                    domainName d `shouldBe` getDomainOwned
                Left e -> error e
                _      -> error "Got an unexpected response to a GET DOMAIN."

        it "gets a valid existing domain that we don't own" $ do
            res <- doRequest $ GetDomain testConfig getDomainNotOurs
            fromLeft res `shouldContain` "415: Authentication Error."

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
                Left e -> error e
                _      -> error "Got unexpected response to RegisterDomain."

    describe "Domain Registration" $ do
        it "renews a domain" $ do
            let (dname, affid, expyear) = renewTest
            res <- doRequest $ RenewDomain testConfig dname False affid expyear True 1
            putStrLn $ show res
            case res of
                Right (DomainRenewalResult d) -> do
                    putStrLn $ show d
                    pass
                Left e -> error e
                _      -> error "Got unexpected response to RenewDomain."

    describe "Passwords" $ do
        it "can change to a valid password" $ do
            let pwd = fromJust $ makePassword validPassword
            (unPassword pwd) `shouldBe` validPassword
            let req = ChangeDomainPassword testConfig passwordTestDomain pwd
            res <- doRequest req
            case res of
                Right (GenericSuccess _) -> pass
                Left e -> error e
                _      -> error "Got unexpected response to ChangeDomainPassword."

        it "cannot use an invalid password" $ do
            let pwd = makePassword invalidPassword
            pwd `shouldBe` Nothing

        it "can send passwords to a domain contact" $ do
            res <- doRequest $ SendDomainPassword testConfig passwordTestDomain "admin" False
            case res of
                Right (GenericSuccess _) -> pass
                Left e -> error e
                _      -> error "Unexpected response to SendDomainPassword."

    describe "Cookies" $ do
        it "can get a cookie for a valid domain logon" $ do
            let (d, u, p) = cookieTest
            let req = SetCookie testConfig d u p
            res <- doRequest req
            case res of
                Right (CookieResult _) -> pass
                Left e -> error e
                _      -> error "Unexpected response to SetCookie."

        it "gets a domain using a cookie" $ do
            let (d, u, p) = cookieTest
            res <- doRequest $ SetCookie testConfig d u p
            case res of
                Right (CookieResult jar) -> do
                    res2 <- doRequest $ GetDomainWithCookie testConfig d (cookieStr jar)
                    case res2 of
                        Right (DomainResult d') -> do
                            (domainName d') `shouldBe` d
                        Left e -> error e
                        _      -> error "Unexpected response to GetDomainWithCookie."

                Left e -> error e
                _      -> error "This should never happen."

    describe "Domain Updates" $ do
        it "can set whois privacy" $ do
            res <- doRequest $ ModifyDomain testConfig modifyTest False
                (fromList
                    [ ("data", "whois_privacy_state")
                    , ("state", "enable")
                    ]) Nothing
            res `shouldBe` (Right $ GenericSuccess "Whois Privacy successfully enabled")

            res2 <- doRequest $ ModifyDomain testConfig modifyTest False
                (fromList
                    [ ("data", "whois_privacy_state")
                    , ("state", "disable")
                    ]) Nothing
            res2 `shouldBe` (Right $ GenericSuccess "Whois Privacy successfully disabled")


-- | Explicitly pass a test.
pass :: Expectation
pass = return ()

main :: IO ()
main = hspec suite
