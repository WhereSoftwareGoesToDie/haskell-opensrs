{-# LANGUAGE OverloadedStrings #-}

-- | RequestTests runs as a standalone executable so that developers can change
-- TestConfig to whatever settings they want.

module Main where

import Blaze.ByteString.Builder
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Char
import Data.Either.Utils
import Data.Map
import Data.Maybe
import Data.OpenSRS
import Data.OpenSRS.Types
import Data.Time
import Data.Tuple.Sequence
import Data.UUID
import Data.UUID.V5
import System.Environment
import System.Locale (defaultTimeLocale)
import Test.Hspec
import Test.Hspec.Expectations
import Text.XmlHtml

import TestConfig

-- To run a full suite, you must define the following environment variables:
-- * SRS Configuration
--   * SRS_HOST
--   * SRS_USER
--   * SRS_KEY
--   * SRS_IP
-- * Domain availability tests
--   * SRSTEST_DOMAINLOOKUP_FREE
--   * SRSTEST_DOMAINLOOKUP_TAKEN
--   * SRSTEST_DOMAINGET_OURS
--   * SRSTEST_DOMAINGET_NOTOURS
-- * Domain registration and renewal tests
--   * SRSTEST_DOMAINREGISTER_NAME
--   * SRSTEST_DOMAINREGISTER_NAME2
--   * SRSTEST_DOMAINRENEW_NAME
--   * SRSTEST_DOMAINRENEW_AFFID
--   * SRSTEST_DOMAINRENEW_EXPYEAR
-- * Cookie tests
--   * SRSTEST_COOKIE_DOMAINGET_DOMAIN
--   * SRSTEST_COOKIE_DOMAINGET_USER
--   * SRSTEST_COOKIE_DOMAINGET_PASS
-- * Domain password tests
--   * SRSTEST_DOMAINPASSWORD_SEND
-- * Domain update tests
--   * SRSTEST_DOMAINMODIFY_WHOISPRIV_DOMAIN
--   * SRSTEST_DOMAINMODIFY_WHOISPRIV_ENABLE (True || False)

makeDomain :: String -> IO Domain
makeDomain dname = do
    t <- getCurrentTime
    let t' = addUTCTime ((86400 * 365) :: NominalDiffTime) t
    return $ Domain dname True testContacts (Just t) True (Just t) (Just "5534") True (Just t') testNameservers

suite :: Spec
suite = do
    describe "Domain List" .
        it "can list domains with expiry in a given period" $ let
            run cfg = do
                t <- getCurrentTime
                let sd = addUTCTime ((-86400 * 365 * 20) :: NominalDiffTime) t
                let ed = addUTCTime ((86400 * 365 * 20) :: NominalDiffTime) t
                res <- doRequest $ ListDomainsByExpiry cfg sd ed 0 5000
                case res of
                    Right (DomainListResult (DomainList count items _)) ->
                        length items `shouldBe` count
                    Left e                      -> error e
                    _                           -> error "This should never happen."
            in withSRSConfig run

    describe "Domain Lookup/Get" $ do
        it "looks up a valid free domain" $ let
            run cfg d = do
                res <- doRequest $ LookupDomain cfg d
                res `shouldBe` (Right . DomainAvailabilityResult $ Available d)
            in withSRSConfigArgs (lookupEnv "SRSTEST_DOMAINLOOKUP_FREE") run

        it "looks up a valid taken domain" $ let
            run cfg d = do
                res <- doRequest $ LookupDomain cfg d
                res `shouldBe` (Right . DomainAvailabilityResult $ Unavailable d)
            in withSRSConfigArgs (lookupEnv "SRSTEST_DOMAINLOOKUP_TAKEN") run

        it "gets a valid existing domain" $ let
            run cfg d = do
                res <- doRequest $ GetDomain cfg d
                case res of
                    Right (DomainResult dr) ->
                        domainName dr `shouldBe` d
                    Left e                      -> error e
                    _                           -> error "This should never happen."
            in withSRSConfigArgs (lookupEnv "SRSTEST_DOMAINGET_OURS") run

        it "gets a valid existing domain that we don't own" $ withSRSConfigArgs
            (lookupEnv "SRSTEST_DOMAINGET_NOTOURS")
            (\cfg d -> do
                res <- doRequest $ GetDomain cfg d
                fromLeft res `shouldContain` "415: Authentication Error.")

    describe "Domain Registration" .
        it "registers a domain" $ let
            run cfg d = do
                domain <- makeDomain d
                res <- doRequest $ RegisterDomain cfg domain True
                                                  Nothing Nothing False False
                                                  False True 2
                                                  un pwd NewRegistration Nothing
                case res of
                    Right DomainRegistrationResult{} -> pass
                    Left e                           -> error e
                    _                                -> error "This should never happen."
            un = fromJust $ makeUsername "webmaster"
            pwd = fromJust $ makePassword "aTestingPassWord123"
            in withSRSConfigArgs (lookupEnv "SRSTEST_DOMAINREGISTER_NAME") run

    describe "Domain Registration/Renewal" $ do
        it "registers and renews a new domain" $ let
            run cfg d = do
                domain <- makeDomain d
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
                let req = RegisterDomain cfg domain changeContact comments encoding lock park whois handleNow regPeriod username password regType tldData
                res <- doRequest req
                case res of
                    Right DomainRegistrationResult{} -> do
                        -- Stage 2: Get domain
                        let req = GetDomain cfg (domainName domain)
                        res <- doRequest req
                        case res of
                            Right (DomainResult d2) -> do
                                -- Stage 3: Renew domain
                                let expyear = read . formatTime defaultTimeLocale "%Y" . fromJust $ domainExpireDate d2
                                let req = RenewDomain cfg (domainName d2) (domainAutoRenew d2) (fromJust $ domainAffiliateID d2) expyear True 1
                                res <- doRequest req
                                case res of
                                    Right (DomainRenewalResult drr) -> do
                                        print drr
                                        pass
                                    Left e                      -> error e
                                    _                           -> error "This should never happen."

                            Left e                      -> error e
                            _                           -> error "This should never happen."

                    Left e                      -> error e
                    _                           -> error "This should never happen."
            in withSRSConfigArgs (lookupEnv "SRSTEST_DOMAINREGISTER_NAME2") run

        it "renews a domain" $ let
            run cfg (d,a,y) = do
                res <- doRequest $ RenewDomain cfg d False a y True 1
                case res of
                    Right (DomainRenewalResult dr) -> do
                        print dr
                        pass
                    Left e                         -> error e
                    _                              -> error "This should never happen."
            getSettings = do
                d <- lookupEnv "SRSTEST_DOMAINRENEW_NAME"
                a <- lookupEnv "SRSTEST_DOMAINRENEW_AFFID"
                y <- lookupEnv "SRSTEST_DOMAINRENEW_EXPYEAR"
                return $ sequenceT (d, a, fmap toInt y)
            toInt x = read x :: Int
            in withSRSConfigArgs getSettings run

    describe "Passwords" $ do
        it "can change to a valid password" $ let
            run cfg d = do
                let un = fromJust $ makeUsername "webmaster"
                let pwd = fromJust $ makePassword validPassword
                unPassword pwd `shouldBe` validPassword
                res <- doRequest $ ChangeDomainOwnership cfg d un pwd
                case res of
                    Right (GenericSuccess _) -> pass
                    Left e                   -> error e
                    _                        -> error "This should never happen."
            in withSRSConfigArgs (lookupEnv "SRSTEST_DOMAINPASSWORD_SEND") run

        it "cannot use an invalid password" $ do
            let pwd = makePassword invalidPassword
            pwd `shouldBe` Nothing

        it "can send passwords to a domain contact" $ let
            run cfg d = do
                res <- doRequest $ SendDomainPassword cfg d "owner" False
                case res of
                    Right (GenericSuccess _) -> pass
                    Left e                   -> error e
                    _                        -> error "This should never happen."
            in withSRSConfigArgs (lookupEnv "SRSTEST_DOMAINPASSWORD_SEND") run

    describe "Cookies" $ do
        it "can get a cookie for a valid domain logon" $ let
            run cfg (d,u,p) = do
                res <- doRequest $ SetCookie cfg d u p
                case res of
                    Right (CookieResult _) -> pass
                    Left e                 -> error e
                    _                      -> error "This should never happen."
            in withSRSConfigArgs getCookieSettings run

        it "gets a domain using a cookie" $ let
            run cfg (d,u,p) = do
                res <- doRequest $ SetCookie cfg d u p
                case res of
                    Right (CookieResult jar) -> do
                        res2 <- doRequest (GetDomainWithCookie cfg d $ cookieStr jar)
                        case res2 of
                            Right (DomainResult d') -> domainName d' `shouldBe` d
                            Left e                  -> error e
                            _                       -> error "This should never happen."
                    Left e                 -> error e
                    _                      -> error "This should never happen."
            in withSRSConfigArgs getCookieSettings run

    describe "Domain Updates" .
        it "can set whois privacy" $ let
            run cfg (d,s) = do
                let s' = if s then "enable" else "disable"
                let req = ModifyDomain cfg d False (fromList [("data", "whois_privacy_state"), ("state", s')]) Nothing
                res <- doRequest req
                if s
                    then res `shouldBe` (Right $ GenericSuccess "Whois Privacy successfully enabled")
                    else res `shouldBe` (Right $ GenericSuccess "Whois Privacy successfully disabled")
            getSettings = do
                d <- liftIO $ lookupEnv "SRSTEST_DOMAINMODIFY_WHOISPRIV_DOMAIN"
                e <- liftIO $ lookupEnv "SRSTEST_DOMAINMODIFY_WHOISPRIV_ENABLE"
                return $ sequenceT (d, (== "True") <$> e)
            in withSRSConfigArgs getSettings run

-- | Get configuration
getSRSConfig :: IO (Maybe SRSConfig)
getSRSConfig = do
    host <- lookupEnv "SRS_HOST"
    user <- lookupEnv "SRS_USER"
    key  <- lookupEnv "SRS_KEY"
    ip   <- lookupEnv "SRS_IP"
    return $ case sequenceT (host,user,key,ip) of
        Just (h,u,k,i) -> Just (SRSConfig h u k i False)
        _              -> Nothing

-- | Wrap an expectation in a check for config.
-- If we don't have config, we pass (defer) the expectation.
withSRSConfig :: (SRSConfig -> Expectation) -> Expectation
withSRSConfig fn = do
    cfg <- liftIO getSRSConfig
    case cfg of
        Just c -> fn c
        _      -> do
            liftIO $ putStrLn "WARNING: No OpenSRS configuration in use."
            pass

-- | Wrap an expectation in a check for config and arguments.
-- If we don't have config or arguments, we pass (defer) the expectation.
withSRSConfigArgs
    :: IO (Maybe a)
    -> (SRSConfig -> a -> Expectation)
    -> Expectation
withSRSConfigArgs getArgs fn = do
    cfg  <- liftIO getSRSConfig
    args <- liftIO getArgs
    case sequenceT (cfg, args) of
        Just (c, a) -> fn c a
        _           -> do
            liftIO $ putStrLn "WARNING: Cannot get OpenSRS configuration and/or test arguments."
            pass

getCookieSettings :: IO (Maybe (String, SRSUsername, Password))
getCookieSettings = do
    d <- liftIO $ lookupEnv "SRSTEST_COOKIE_DOMAINGET_DOMAIN"
    u <- liftIO $ lookupEnv "SRSTEST_COOKIE_DOMAINGET_USER"
    p <- liftIO $ lookupEnv "SRSTEST_COOKIE_DOMAINGET_PASS"
    return $ sequenceT (d, join $ fmap makeUsername u, join $ fmap makePassword p)

-- | Explicitly pass a test.
pass :: Expectation
pass = return ()

main :: IO ()
main = do
    res <- hspec suite
    return ()

