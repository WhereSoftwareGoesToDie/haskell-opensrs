{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.OpenSRS (
    doRequest,
    requestXML,

    DomainName,

    SRSRequest (..),
    SRSResponse (..),
    DomainAvailability (..),
    DomainRenewal (..),
    DomainRegistration (..),
    RegistrationType (..),
    SRSResult (..),

    toUTC,
    toUTC',

    SRSConfig (..),

    DomainList (..),
    DomainListDomain (..),

    Domain (..),
    Contact (..),
    Nameserver (..),
    TLDData,

    XmlPost (..),
    Postable,

    SRSUsername,
    makeUsername,
    unUsername,

    Password,
    makePassword,
    unPassword,

    SRSCookie,
    SRSCookieJar (..)
) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Hash.MD5
import Data.List
import Data.Map
import Data.Maybe
import Data.Monoid
import Data.Time
import Data.Tuple.Sequence
import Debug.Trace

import Network.Wreq
import Network.Wreq.Types


import System.Locale (defaultTimeLocale)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

import Blaze.ByteString.Builder
import Text.XmlHtml

--------------------------------------------------------------------------------

import Data.OpenSRS.Types

import Text.HTML.TagSoup.Manipulators
import Text.HTML.TagSoup.Pretty

import Data.OpenSRS.ToXML

--------------------------------------------------------------------------------
-- | Submit a request to the OpenSRS API for processing.
doRequest :: SRSRequest -> IO (Either String SRSResult)
-- doRequest r@(AllDomains {}) =
--     doRequest' (DomainListResult . parseDomainList) r
-- doRequest r@(ListDomains {}) =
--     doRequest' (DomainListResult . parseDomainList) r

doRequest r@(ListDomainsByExpiry {}) =
    doRequest' (fmap DomainListResult . parseDomainList)
                "Could not parse DomainList from result" r

doRequest r@(GetDomain {..}) =
    doRequest' (fmap DomainResult . parseDomain requestDomainName)
                "Could not parse Domain from result" r

doRequest r@(GetDomainWithCookie {..}) =
    doRequest' (fmap DomainResult . parseDomain requestDomainName)
                "Could not parse Domain from result" r

doRequest r@(GetDomainTldData {..}) =
    doRequest' (fmap TldDataResult . parseTldDataDict)
                "Could not parse TLDData from result" r

doRequest r@(LookupDomain {..}) =
    doRequest' (fmap DomainAvailabilityResult . parseDomainAvailability requestDomainName)
                "Could not parse DomainAvailability from result" r

doRequest r@(RenewDomain {..}) =
    doRequest' (fmap DomainRenewalResult . parseDomainRenewal requestDomainName)
                "Could not parse DomainRenewal from result" r

doRequest r@(RegisterDomain {..}) =
    doRequest' (fmap DomainRegistrationResult . parseDomainRegistration requestDomain)
                "Could not parse DomainRegistration from result" r

doRequest r@(ModifyDomain {}) =
    doRequest' (fmap GenericSuccess . parseSuccess)
                "Could not get response text from result" r

doRequest r@(UpdateDomain {}) =
    doRequest' (fmap GenericSuccess . parseSuccess)
                "Could not get response text from result" r

doRequest r@(ChangeDomainOwnership {}) =
    doRequest' (fmap GenericSuccess . parseSuccess)
                "Could not get response text from result" r

doRequest r@(SendDomainPassword {}) =
    doRequest' (fmap GenericSuccess . parseSuccess)
                "Could not get response text from result" r

doRequest r@(SetCookie {..}) =
    doRequest' (fmap CookieResult . parseCookie requestDomainName)
                "Could not parse SRSCookieJar from result" r

doRequest _ = return $ Left "This OpenSRS request type has not been implemented yet."

-- | Internal method to perform a request and then process it.
-- Handles cases where responses cannot be parsed.
doRequest'
    :: (String -> Maybe SRSResult) -- ^ Result parser
    -> String -- ^ Error message if result cannot be parsed
    -> SRSRequest -- ^ Request
    -> IO (Either String SRSResult) -- ^ Either a result or an error string
doRequest' parser e r = do
    res <- postRequest r
    let unpackedb = BSL8.unpack (res ^. responseBody)
    void $ tryDebug (requestConfig r) unpackedb
    let resp = parseResponse unpackedb
    return $ if srsSuccess resp
        then case parser unpackedb of
            Just x -> Right x
            _      -> Left e
        else Left $ responseError resp

-- | Transforms a SRSResponse into an error string
responseError :: SRSResponse -> String
responseError resp = concat [show $ srsResponseCode resp,
                             ": ",
                             srsResponseText resp]

-- | Posts request to OpenSRS
postRequest :: SRSRequest -> IO (Response BSL8.ByteString)
postRequest req = do
    void $ tryDebug (requestConfig req) (BSL8.unpack . toLazyByteString . render . requestXML $ req)
    postWith opts (srsEndpoint $ requestConfig req) postBody
  where
    opts = defaults
            & header "X-Username" .~ [pack . srsUsername $ requestConfig req]
            & header "X-Signature" .~ [pack sig]
            & header "Content-Length" .~ [pack . show $ BSL8.length ps]
    sig = md5Wrap (srsKey $ requestConfig req) (BSL8.unpack ps)
    postBody = XmlPost ps
    ps = toLazyByteString . render $ requestXML req

-- | MD5 signature generator for OpenSRS
md5Wrap :: String -> String -> String
md5Wrap pk content = md5pack (md5pack (content <> pk) <> pk)
  where
    md5pack = md5s . Str

-- | Debug an XML string.
tryDebug :: SRSConfig -> String -> IO ()
tryDebug cfg s = when (srsDebug cfg) . traceIO $ prettyXML "  " s

--------------------------------------------------------------------------------
-- | Internal XML helper: Turns XML string into a list of XML TagTrees
toXmlTree :: String -> [TagTree String]
toXmlTree = tagTree . parseTags

--------------------------------------------------------------------------------
-- | Parse SRSResponse so we know if it's good or not
-- If we can parse the response code, we return an SRSResponse.
-- If we can't, we return an SRSResponseFailure with the code set to -1.
parseResponse :: String -> SRSResponse
parseResponse s = let
    xml = parseTags s
    responseCodeS    = getText xml "<item key='response_code'>"
    responseSuccessS = getText xml "<item key='is_success'>"
    responseTextS    = getText xml "<item key='response_text'>"
    in case readInteger responseCodeS of
        Just code -> SRSResponse (responseSuccessS == "1") responseTextS code
        Nothing   -> SRSResponseFailure False ("Could not parse response code from \"" <> responseCodeS <> "\"") (-1)

--------------------------------------------------------------------------------
-- | Parse domain list
parseDomainList :: String -> Maybe DomainList
parseDomainList s =
    case readInteger totalS of
        Just t  -> Just $ DomainList t (fmap makeDomain domainItems) (remainderS == "1")
        Nothing -> Nothing
  where
    xml = parseTags s
    xmlt = toXmlTree s
    -- metadata
    totalS     = getText xml "<item key='total'>"
    remainderS = getText xml "<item key='remainder'>"
    -- domains
    domainItems = kidsWith "item" . topMatching "<item key='exp_domains'>" $ topMatching "<item key='attributes'>" xmlt
    makeDomain domains =
        let
            gtc = getText' . flattenTree $ [domains]
        in DomainListDomain (gtc "<item key='name'>")
                            (toDate $ gtc "<item key='expiredate'>")
                            (gtc "<item key='f_auto_renew'>" == "Y")
                            (gtc "<item key='f_let_expire'>" == "Y")
    toDate = fromJust . parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

--------------------------------------------------------------------------------
-- | Extract domain data
parseDomain :: DomainName -> String -> Maybe Domain
parseDomain dn s = Just $ Domain dn
    (autoRenewS == "1")
    (fromList . fmap makeContact $ contactSets)
    (toUTC' updateDateS)
    (sponsorRSPS == "1")
    (toUTC' createDateS)
    (Just $ getText xml "<item key='affiliate_id'>")
    (letExpireS == "1")
    (toUTC' expireDateS)
    (fmap makeNameserver nameserverSets)
  where
    xml = parseTags s
    xmlt = toXmlTree s
    -- metadata
    autoRenewS  = getText xml "<item key='auto_renew'>"
    updateDateS = getText xml "<item key='registry_updatedate'>"
    createDateS = getText xml "<item key='registry_createdate'>"
    letExpireS  = getText xml "<item key='let_expire'>"
    expireDateS = getText xml "<item key='registry_expiredate'>"
    sponsorRSPS = getText xml "<item key='sponsoring_rsp'>"
    -- contacts
    contactSets = kidsWith "item" . topMatching "<item key='contact_set'>" $ topMatching "<item key='attributes'>" xmlt
    makeContact contacts =
        let
            gtc = getText' . flattenTree $ [contacts]
            k = fromJust . Data.List.lookup "key" $ currentTagAttr contacts
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
    nameserverSets = kidsWith "dt_assoc" . kidsWith "item" . topMatching "<item key='nameserver_list'>" $ topMatching "<item key='attributes'>" xmlt
    makeNameserver nameservers =
        let
            gtc = getText' . flattenTree $ [nameservers]
        in Nameserver (Just $ gtc "<item key='name'>")
                      (Just $ gtc "<item key='sortorder'>")
                      (Just $ gtc "<item key='ipaddress'>")

--------------------------------------------------------------------------------
-- | Extract domain availability data
parseDomainAvailability :: DomainName -> String -> Maybe DomainAvailability
parseDomainAvailability dn s =
    Just $ case getText xml "<item key='status'>" of
        "available" -> Available dn
        _           -> Unavailable dn
  where
    xml = parseTags s

--------------------------------------------------------------------------------
-- | Extract domain renewal status
parseDomainRenewal :: DomainName -> String -> Maybe DomainRenewal
parseDomainRenewal dn s =
    case readInteger responseCodeS of
        Nothing  -> Nothing
        Just 200 -> Just $ Renewed dn (getText xml "<item key='admin_email'>")
                                      (getText xml "<item key='auto_renew'>" == "1")
                                      (getText xml "<item key='order_id'>")
                                      (Just $ getText xml "<item key='queue_request_id'>")
                                      (Just $ getText xml "<item key='id'>")
                                      (getText xml "<item key='registration expiration date'>")
        Just 480 -> Just $ NotRenewed dn 480 "Renewals not enabled for this TLD"
        Just 555 -> Just $ NotRenewed dn 555 "Domain already renewed"
        Just 541 -> Just $ NotRenewed dn 541 "Provided expiration year does not match registry value"
        Just 400 -> Just $ case responseTextS of
            "Fatal Server Error" -> NotRenewed dn 400 "Fatal error at registry"
            _                    -> NotRenewed dn 400 "Renewal request already submitted, cannot renew until request completed"
        x        -> Just $ NotRenewed dn (fromJust x) responseTextS
  where
    responseCodeS = getText xml "<item key='response_code'>"
    responseTextS = getText xml "<item key='response_text'>"
    xml           = parseTags s

--------------------------------------------------------------------------------
-- | Extract domain registration status
parseDomainRegistration :: Domain -> String -> Maybe DomainRegistration
parseDomainRegistration _ s =
    Just $ DomainRegistration (gs "<item key='async_reason'>")
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
-- | Extract TLD Data as a key/value dictionary.
parseTldDataDict :: String -> Maybe TLDData
parseTldDataDict s = Just . fromList . fmap (makeTld . return) $ tldroots
  where
    xml = parseTags s
    xmlt = tagTree xml
    -- top level TLD items
    tldroots = kidsWith "item" $ topMatching "<item key='tld_data'>" xmlt
    makeTld t =
        let
            k = fromJust . Data.List.lookup "key" . currentTagAttr $ head t
            kids = kidsWith "item" $ topMatching "dt_assoc" t
            v = fromList $ fmap (makeItems . return) kids
        in (k, v)
    -- 2nd level
    makeItems t =
        let
            k = fromJust . Data.List.lookup "key" . currentTagAttr $ head t
            v = itemInnerValue $ flattenTree t
        in (k, v)

--------------------------------------------------------------------------------
-- | Extract status for methods that only require a success/failure response
parseCookie :: DomainName -> String -> Maybe SRSCookieJar
parseCookie dn s = 
    case sequenceT (readInteger domainCountS, readInteger waitingRequestsS, toDate expireDateS) of
        Just (dc, wr, ed) -> Just $ SRSCookieJar dn
                                                 (gt "<item key='cookie'>")
                                                 dc
                                                 ed
                                                 (gt "<item key='f_owner'>" == "1")
                                                 (toDate lastAccessS)
                                                 (gt "<item key='last_ip'>")
                                                 (gt "<item key='permission'>")
                                                 wr
        _ -> Nothing
  where
    xml = parseTags s
    gt  = getText xml
    -- metadata
    domainCountS     = gt "<item key='domain_count'>"
    waitingRequestsS = gt "<item key='waiting_requests_no'>"
    expireDateS      = gt "<item key='expiredate'>"
    lastAccessS      = gt "<item key='last_access_time'>"
    -- try and parse date
    toDate = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

--------------------------------------------------------------------------------
-- | Extract status for methods that only require a success/failure response
parseSuccess :: String -> Maybe String
parseSuccess s = Just $ getText (parseTags s) "<item key='response_text'>"
