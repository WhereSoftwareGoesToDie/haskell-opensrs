{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.OpenSRS where

import Control.Lens
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, split, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.CaseInsensitive as CI
import Data.Hash.MD5
import Data.List
import Data.Map
import Data.Maybe

import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (HeaderName)
import Network.Wreq
import Network.Wreq.Types

import Data.Char
import Data.String (IsString)
import qualified Data.Text as Text
import Data.Time
import System.Locale (defaultTimeLocale)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity
import Text.HTML.TagSoup.Tree
import Text.StringLike (StringLike, fromString, toString)

import Blaze.ByteString.Builder
import Text.XmlHtml

--------------------------------------------------------------------------------

import Data.OpenSRS.Types
import Data.OpenSRS.Types.XmlPost

import Text.HTML.TagSoup.Manipulators

import Data.OpenSRS.ToXML

--------------------------------------------------------------------------------
-- | Main method for requesting
doRequest :: SRSRequest -> IO (Either String SRSResult)
-- doRequest r@(AllDomains {}) =
--     doRequest' (DomainListResult . parseDomainList) r
-- doRequest r@(ListDomains {}) =
--     doRequest' (DomainListResult . parseDomainList) r
doRequest r@(ListDomainsByExpiry {}) =
    doRequest' (DomainListResult . parseDomainList) r
doRequest r@(GetDomain {..}) =
    doRequest' (DomainResult . parseDomain requestDomainName) r
doRequest r@(LookupDomain {..}) =
    doRequest' (DomainAvailabilityResult . parseDomainAvailability requestDomainName) r
doRequest r@(RenewDomain {..}) =
    doRequest' (DomainRenewalResult . parseDomainRenewal requestDomainName) r
doRequest r@(RegisterDomain {..}) =
    doRequest' (DomainRegistrationResult . parseDomainRegistration requestDomain) r
doRequest r@(UpdateDomain {}) =
    doRequest' (GenericSuccess . parseSuccess) r
doRequest r@(ChangeDomainPassword {}) =
    doRequest' (GenericSuccess . parseSuccess) r
doRequest r@(SendDomainPassword {}) =
    doRequest' (GenericSuccess . parseSuccess) r
doRequest r@(SetCookie {..}) =
    doRequest' (CookieResult . parseCookie requestDomainName) r
doRequest _ = return $ Left "This OpenSRS request type has not been implemented yet."

-- | Internal method to p[erform a request and then process it.
doRequest' :: (String -> SRSResult) -> SRSRequest -> IO (Either String SRSResult)
doRequest' parser r = do
    res <- postRequest r
    let unpackedb = BSL8.unpack (res^.responseBody)
    -- putStrLn unpackedb
    let resp = parseResponse unpackedb
    return $ if srsSuccess resp
        then Right $ parser unpackedb
        else Left $ responseError resp

-- | Transforms a SRSResponse into an error string
responseError :: SRSResponse -> String
responseError resp = show (srsResponseCode resp) ++ ": " ++ srsResponseText resp

-- | Posts request to OpenSRS
postRequest :: SRSRequest -> IO (Response BSL8.ByteString)
postRequest req = postWith opts (srsEndpoint $ requestConfig req) postBody
  where
    opts = defaults
            & header "X-Username" .~ [pack $ srsUsername $ requestConfig req]
            & header "X-Signature" .~ [pack sig]
            & header "Content-Length" .~ [pack $ show $ BSL8.length ps]
    sig = md5Wrap (srsKey $ requestConfig req) (BSL8.unpack ps)
    postBody = XmlPost ps
    ps = toLazyByteString $ render $ requestXML req

-- | MD5 signature generator for OpenSRS
md5Wrap :: String -> String -> String
md5Wrap pk content = md5pack (md5pack (content ++ pk) ++ pk)
  where
    md5pack = md5s . Str

--------------------------------------------------------------------------------
-- | Parse SRSResponse so we know if it's good or not
parseResponse :: String -> SRSResponse
parseResponse s = SRSResponse
    (gt "<item key='is_success'>" == "1")
    (gt "<item key='response_text'>")
    (read $ gt "<item key='response_code'>")
  where
    xml = parseTags s
    gt = getText xml

--------------------------------------------------------------------------------
-- | Parse domain list
parseDomainList :: String -> DomainList
parseDomainList s = DomainList (read $ gt "<item key='total'>")
                               (Prelude.map makeDomain domainItems)
                               (gt "<item key='remainder'>" == "1")
  where
    xml = parseTags s
    gt = getText xml
    xmlt = tagTree xml
    -- domains
    domainItems = kidsWith "item" $ topMatching "<item key='exp_domains'>" $ topMatching "<item key='attributes'>" xmlt
    makeDomain set =
        let
            gtc = getText' . flattenTree $ [set]
        in DomainListDomain (gtc "<item key='name'>")
                            (toDate $ gtc "<item key='expiredate'>")
                            (gtc "<item key='f_auto_renew'>" == "Y")
                            (gtc "<item key='f_let_expire'>" == "Y")
    toDate = fromJust . parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

--------------------------------------------------------------------------------
-- | Extract domain data
parseDomain :: DomainName -> String -> Domain
parseDomain dn s = Domain dn
    (gt "<item key='auto_renew'>" == "1")
    (fromList . Prelude.map makeContact $ contactSets)
    (toUTC' $ gt "<item key='registry_updatedate'>")
    (Just $ gt "<item key='sponsoring_rsp'>")
    (toUTC' $ gt "<item key='registry_createdate'>")
    (Just $ gt "<item key='affiliate_id'>")
    (toUTC' $ gt "<item key='expiredate'>")
    (gt "<item key='let_expire'>" == "1")
    (toUTC' $ gt "<item key='registry_expiredate'>")
    (Prelude.map makeNameserver nameserverSets)
  where
    xml = parseTags s
    gt  = getText xml
    xmlt = tagTree xml
    -- contacts
    contactSets = kidsWith "item" $ topMatching "<item key='contact_set'>" $ topMatching "<item key='attributes'>" xmlt
    makeContact set =
        let
            gtc = getText' . flattenTree $ [set]
            k = fromJust $ Data.List.lookup "key" $ currentTagAttr set
            v = Contact (Just $ gtc "<item key='first_name'>")
                    (Just $ gtc "<item key='last_name'>")
                    (Just $ gtc "<item key='org_name'>")
                    (Just $ gtc "<item key='email'>")
                    (Just $ gtc "<item key='phone'>")
                    (Just $ gtc "<item key='fax'>")
                    (Just $ gtc "<item key='address1'>")
                    (Just $ gtc "<item key='address2'>")
                    (Just $ gtc "<item key='address3'>")
                    (Just $ gtc "<item key='city'>")
                    (Just $ gtc "<item key='state'>")
                    (Just $ gtc "<item key='postal_code'>")
                    (Just $ gtc "<item key='country'>")
        in (k, v)
    -- namespaces
    nameserverSets = kidsWith "dt_assoc" $ kidsWith "item" $ topMatching "<item key='nameserver_list'>" $ topMatching "<item key='attributes'>" xmlt
    makeNameserver set =
        let
            gtc = getText' . flattenTree $ [set]
        in Nameserver (Just $ gtc "<item key='name'>")
                      (Just $ gtc "<item key='sortorder'>")
                      (Just $ gtc "<item key='ipaddress'>")

--------------------------------------------------------------------------------
-- | Extract domain availability data
parseDomainAvailability :: DomainName -> String -> DomainAvailability
parseDomainAvailability dn s =
    case getText xml "<item key='status'>" of
        "available" -> Available dn
        _           -> Unavailable dn
  where
    xml = parseTags s

--------------------------------------------------------------------------------
-- | Extract domain renewal status
parseDomainRenewal :: DomainName -> String -> DomainRenewal
parseDomainRenewal dn s =
    case gt "<item key='response_code'>" of
        "200" -> Renewed dn (gt "<item key='admin_email'>")
                            (gt "<item key='auto_renew'>" == "1")
                            (gt "<item key='order_id'>")
                            (Just $ gt "<item key='queue_request_id'>")
                            (Just $ gt "<item key='id'>")
                            (gt "<item key='registration_expiration_date'>")
        "480" -> NotRenewed dn "Renewals not enabled for this TLD"
        "555" -> NotRenewed dn "Domain already renewed"
        "541" -> NotRenewed dn "Provided expiration year does not match registry value"
        "400" -> case gt "<item key='response_text'>" of
            "Fatal Server Error" -> NotRenewed dn "Fatal error at registry"
            _                    -> NotRenewed dn "Renewal request already submitted, cannot renew until request completed"
        _     -> NotRenewed dn (gt "<item key='response_text'>")
  where
    xml = parseTags s
    gt  = getText xml

--------------------------------------------------------------------------------
-- | Extract domain registration status
parseDomainRegistration :: Domain -> String -> DomainRegistration
parseDomainRegistration d s =
    DomainRegistration (gs "<item key='async_reason'>")
                       (gs "<item key='error'>")
                       (gs "<item key='forced_pending'>")
                       (gt "<item key='id'>")
                       (gs "<item key='queue_request_id'>")
                       (gt "<item key='registration_code'>")
                       (gt "<item key='registration_text'>")
                       (gs "<item key='transfer_id'>")
                       (gt "<item key='whois_privacy_state'>")
  where
    xml = parseTags s
    gt  = getText xml
    gs m = case gt m of
        "" -> Nothing
        x  -> Just x

--------------------------------------------------------------------------------
-- | Extract status for methods that only require a success/failure response
parseCookie :: DomainName -> String -> SRSCookieJar
parseCookie dn s = SRSCookieJar dn
                                (gt "<item key='cookie'>")
                                (read $ gt "<item key='domain_count'>")
                                (toDate $ gt "<item key='expiredate'>")
                                (gt "<item key='f_owner'>" == "1")
                                (gt "<item key='last_access_time'>")
                                (gt "<item key='last_ip'>")
                                (gt "<item key='permission'>")
                                (read $ gt "<item key='waiting_requests_no'>")
  where
    xml = parseTags s
    gt  = getText xml
    toDate = fromJust . parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

--------------------------------------------------------------------------------
-- | Extract status for methods that only require a success/failure response
parseSuccess :: String -> String
parseSuccess s = getText (parseTags s) "<item key='response_text'>"
