{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.OpenSRS.ToXML (requestXML) where

import Data.Char
import Data.Map
import Data.Maybe
import Data.Monoid
import Data.OpenSRS.Types
import qualified Data.Text as Text
import Data.Time
-- import System.Locale (defaultTimeLocale)
import Text.XmlHtml

{-# ANN module ("HLint: ignore Use =<<" :: String) #-}

----------------------------------------
-- | Actually generates request XML
requestXML :: SRSRequest -> Document
requestXML AllDomains{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "GET" "DOMAIN" (srsIpAddress requestConfig)
        [("type", "list")]
requestXML ListDomains{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "GET" "DOMAIN" (srsIpAddress requestConfig)
        [("type", "list"),
         ("page", show requestPage),
         ("limit", show requestLimit)]
requestXML ListDomainsByExpiry{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "GET_DOMAINS_BY_EXPIREDATE" "DOMAIN" (srsIpAddress requestConfig)
        [("exp_from", dateStr requestStartDate),
         ("exp_to", dateStr requestEndDate),
         ("page", show requestPage),
         ("limit", show requestLimit)]
    dateStr = formatTime defaultTimeLocale "%Y-%m-%d"
requestXML GetDomain{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "GET" "DOMAIN" (srsIpAddress requestConfig)
        [("domain", requestDomainName),
         ("type", "all_info"),
         ("limit", "10")]
requestXML GetDomainTldData{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "GET" "DOMAIN" (srsIpAddress requestConfig)
        [("domain", requestDomainName),
         ("type", "tld_data"),
         ("limit", "10")]
requestXML GetDomainWithCookie{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ cookieRequest "GET" "DOMAIN" (srsIpAddress requestConfig) requestCookie
        [("type", "all_info")]
requestXML LookupDomain{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "LOOKUP" "DOMAIN" (srsIpAddress requestConfig)
        [("domain", requestDomainName),
         ("no_cache", "1")]
requestXML RenewDomain{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "RENEW" "DOMAIN" (srsIpAddress requestConfig)
        [("domain", requestDomainName),
         ("auto_renew", boolVal requestAutoRenew),
         ("affiliate_id", requestAffiliateID),
         ("currentexpirationyear", show requestExpiryYear),
         ("handle", boolVal' "process" "save" requestHandleNow),
         ("period", show requestPeriod)]
requestXML ModifyDomain{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest' "MODIFY" "DOMAIN" (srsIpAddress requestConfig) <>
        attributes (
            [ itemNode "domain" requestDomainName
            , itemNode "affect_domains" $ boolVal requestAffectLinked
            ] <> itemMap requestData <> tldData)
    tldData = case requestTldData of
        Nothing -> []
        Just tld' -> [itemParent "tld_data" [tag "dt_assoc" $
            itemParentMap (\vmap -> [tag "dt_assoc" $ itemMap vmap]) tld'
            ]]
requestXML UpdateDomain{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $
        genericRequest' "UPDATE_ALL_INFO" "DOMAIN" (srsIpAddress requestConfig) <> attributes
            (domainToNodes requestDomain)
requestXML RegisterDomain{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $
        genericRequest' "SW_REGISTER" "DOMAIN" (srsIpAddress requestConfig) <> attributes
            (domainToNodes requestDomain <> tldData <> [
                itemNode "change_contact" $ boolVal requestChangeContact,
                mayPair "comments" requestComments,
                mayPair "encoding_type" requestEncoding,
                itemNode "f_lock_domain" $ boolVal requestLock,
                itemNode "f_parkp" $ boolVal requestPark,
                itemNode "f_whois_privacy" $ boolVal requestWhoisPrivacy,
                itemNode "handle" $ boolVal' "process" "save" requestHandleNow,
                itemNode "period" $ show requestPeriod,
                itemNode "reg_username" $ show requestUsername,
                itemNode "reg_password" $ show requestPassword,
                itemNode "reg_type" $ show requestRegType
                ])
    tldData = case requestTldData of
        Nothing -> []
        Just tld' -> [itemParent "tld_data" [tag "dt_assoc" $
            itemParentMap (\vmap -> [tag "dt_assoc" $ itemMap vmap]) tld'
            ]]
requestXML ChangeDomainOwnership{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "CHANGE" "OWNERSHIP" (srsIpAddress requestConfig)
        [("domain", requestDomainName),
         ("username", show requestUsername),
         ("password", show requestPassword)]
requestXML SendDomainPassword{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "SEND_PASSWORD" "DOMAIN" (srsIpAddress requestConfig)
        [("domain_name", requestDomainName),
         ("send_to", requestSendTo),
         ("sub_user", boolVal requestToSubuser)]
requestXML SetCookie{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "SET" "COOKIE" (srsIpAddress requestConfig)
        [("domain", requestDomainName),
         ("reg_username", show requestUsername),
         ("reg_password", show requestPassword)]

-- | domain
domainToNodes :: Domain -> [Node]
domainToNodes domain = [
    itemNode "domain" (domainName domain),
    itemParent "contact_set" [
        tag "dt_assoc" (concatMap contactToNodes . toList $ domainContactSet domain)
    ],
    itemParent "nameserver_list" [
        tag "dt_array" (concatMap nsToNodes . zip [0..] $ domainNameservers domain)
    ]]

-- | contacts
contactToNodes :: (String, Contact) -> [Node]
contactToNodes (k, c) = [
    itemParent k [
        tag "dt_assoc" [
            mayPair "first_name" $ contactFirstName c,
            mayPair "last_name" $ contactLastName c,
            mayPair "org_name" $ contactOrgName c,
            mayPair "email" $ contactEmail c,
            mayPair "phone" $ contactPhone c,
            mayPair "fax" $ contactFax c,
            mayPair "address1" $ contactAddress1 c,
            mayPair "address2" $ contactAddress2 c,
            mayPair "address3" $ contactAddress3 c,
            mayPair "city" $ contactCity c,
            mayPair "state" $ contactState c,
            mayPair "postal_code" $ contactPostalCode c,
            mayPair "country" $ contactCountry c
        ]]]

-- | nameservers
nsToNodes :: (Int, Nameserver) -> [Node]
nsToNodes (k, ns) = [
    itemParent (show k) [
        tag "dt_assoc" [
            mayPair "name" $ nsName ns,
            mayPair "sortorder" $ nsSortorder ns,
            mayPair "ipaddress" $ nsIPAddr ns
        ]]]

-- | preps some generic request parameters
genericRequest :: String -> String -> String -> [(String, String)] -> [Node]
genericRequest action object ip attr =
    genericRequest' action object ip <> makeAttr attr

-- | preps some cookie-dependent request parameters
cookieRequest :: String -> String -> String -> SRSCookie -> [(String, String)] -> [Node]
cookieRequest action object ip cookie attr =
    cookieRequest' action object ip cookie <> makeAttr attr

makeAttr :: [(String, String)] -> [Node]
makeAttr attr = case attr of
    [] -> []
    _  -> attributes $ fmap attrMap attr
  where
    attrMap (k,v) = itemNode k v

genericRequest' :: String -> String -> String -> [Node]
genericRequest' action object ip = [
    itemNode "protocol" "XCP",
    itemNode "action" (fmap toUpper action),
    itemNode "object" (fmap toUpper object),
    itemNode "registrant_ip" ip ]

cookieRequest' :: String -> String -> String -> SRSCookie -> [Node]
cookieRequest' action object ip cookie = [
    itemNode "protocol" "XCP",
    itemNode "action" (fmap toUpper action),
    itemNode "object" (fmap toUpper object),
    itemNode "registrant_ip" ip,
    itemNode "cookie" cookie ]

-- | Writes the standard doctype
doctype :: Maybe DocType
doctype = Just $ DocType (Text.pack "OPS_envelope") (System (Text.pack "ops.dtd")) NoInternalSubset

-- | Packs up a key/value pair node
itemNode :: String -> String -> Node
itemNode k v = Element (Text.pack "item") [(Text.pack "key", Text.pack k)] [TextNode $ Text.pack v]

-- | Packs up a key pair node with children nodes
itemParent :: String -> [Node] -> Node
itemParent k = Element (Text.pack "item") [(Text.pack "key", Text.pack k)]

-- | Straight map to items
itemMap :: Map String String -> [Node]
itemMap = fmap (uncurry itemNode) . toList

-- | Map of parent nodes
itemParentMap :: (a -> [Node]) -> Map String a -> [Node]
itemParentMap fn = fmap (\(k, v) -> itemParent k (fn v)) . toList

-- | Simple tag with children
tag :: String -> [Node] -> Node
tag tn = Element (Text.pack tn) []

-- | Wrap up in attributes tag
attributes :: [Node] -> [Node]
attributes n = [itemParent "attributes" [tag "dt_assoc" n]]

-- | Pack a maybe string value into an itemNode
mayPair :: String -> Maybe String -> Node
mayPair k v = itemNode k $ fromMaybe "" v

-- | Makes a quick and dirty boolean value
boolVal :: Bool -> String
boolVal = boolVal' "1" "0"

-- | Makes a boolean value based on true/false strings
boolVal' :: String -> String -> Bool -> String
boolVal' t f b = if b then t else f

-- | Wraps the entire request
wrapRequest :: [Node] -> [Node]
wrapRequest nodes = [tag "OPS_envelope" [
    tag "header" [ tag "version" [TextNode $ Text.pack "0.9"] ],
    tag "body" [ tag "data_block" [ tag "dt_assoc" nodes ]]
    ]]
