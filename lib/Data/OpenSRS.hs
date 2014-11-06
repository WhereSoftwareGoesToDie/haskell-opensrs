{-# LANGUAGE OverloadedStrings #-}

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
-- Actually check for something

-- usage examples
-- let config = SRSConfig "https://horizon.opensrs.net:55443" "myusername" "mykey" "127.0.0.1"

-- let req = LookupDomain config "bigfatchunkybear.info"
-- res <- doRequest req
-- res

-- let req2 = GetDomain config "hobbseetest.com"
-- res2 <- doRequest req2
-- res2

doRequest :: SRSRequest -> IO (Either String SRSResult)
doRequest r@(GetDomain config domainName) = do
    res <- postRequest r
    let b = res^.responseBody
    let resp = parseResponse $ BSL8.unpack b
    return $ if srsSuccess resp
        then Right $ DomainResult $ parseDomain domainName $ BSL8.unpack b
        else Left $ responseError resp
doRequest r@(LookupDomain config domainName) = do
    res <- postRequest r
    let b = res^.responseBody
    let resp = parseResponse $ BSL8.unpack b
    return $ if srsSuccess resp
        then Right $ DomainAvailabilityResult $ parseDomainAvailability domainName $ BSL8.unpack b
        else Left $ responseError resp
doRequest r@(RenewDomain config domainName _ _ _ _ _) = do
    res <- postRequest r
    let b = res^.responseBody
    let resp = parseResponse $ BSL8.unpack b
    return $ if srsSuccess resp
        then Right $ DomainRenewalResult $ parseDomainRenewal domainName $ BSL8.unpack b
        else Left $ responseError resp
doRequest r@(UpdateDomain config _) = do
    res <- postRequest r
    let b = res^.responseBody
    let resp = parseResponse $ BSL8.unpack b
    return $ if srsSuccess resp
        then Right $ GenericSuccess $ parseSuccess $ BSL8.unpack b
        else Left $ responseError resp

responseError :: SRSResponse -> String
responseError resp = show (srsResponseCode resp) ++ ": " ++ srsResponseText resp

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

md5Wrap :: String -> String -> String
md5Wrap pk content = md5pack (md5pack (content ++ pk) ++ pk)
  where
    md5pack = md5s . Str





--------------------------------------------------------------------------------
-- Parse SRSResponse so we know if it's shit or not

parseResponse :: String -> SRSResponse
parseResponse s = SRSResponse
    (gt "<item key='is_success'>" == "1")
    (gt "<item key='response_text'>")
    (read $ gt "<item key='response_code'>")
  where
    xml = parseTags s
    gt = getText xml

--------------------------------------------------------------------------------
-- Extract domain data



parseDomain :: String -> String -> Domain
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
-- Extract domain availability data

parseDomainAvailability :: String -> String -> DomainAvailability
parseDomainAvailability dn s =
    case getText xml "<item key='status'>" of
        "available" -> Available dn
        _           -> Unavailable dn
  where
    xml = parseTags s

--------------------------------------------------------------------------------
parseDomainRenewal :: String -> String -> DomainRenewal
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
parseSuccess :: String -> String
parseSuccess s = getText (parseTags s) "<item key='response_text'>"
