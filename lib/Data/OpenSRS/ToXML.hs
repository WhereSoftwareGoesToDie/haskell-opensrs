{-# LANGUAGE OverloadedStrings #-}

module Data.OpenSRS.ToXML (requestXML) where

import Blaze.ByteString.Builder
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, split, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.CaseInsensitive as CI
import Data.Char
import Data.Hash.MD5
import Data.List
import Data.Map
import Data.Maybe
import Data.OpenSRS.Types
import Data.OpenSRS.Types.Config
import Data.String (IsString)
import qualified Data.Text as Text
import Data.Time
import System.Locale (defaultTimeLocale)
import Text.StringLike (StringLike, fromString, toString)
import Text.XmlHtml

----------------------------------------
-- | Actually generates request XML
requestXML :: SRSRequest -> Document
requestXML (AllDomains c) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "GET" "DOMAIN" (srsIpAddress c)
        [("type", "list")]
requestXML (ListDomains c p l) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "GET" "DOMAIN" (srsIpAddress c)
        [("type", "list"),
         ("page", show p),
         ("limit", show l)]
requestXML (ListDomainsByExpiry c ds de p l) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "GET_DOMAINS_BY_EXPIREDATE" "DOMAIN" (srsIpAddress c)
        [("exp_from", dateStr ds),
         ("exp_to", dateStr de),
         ("page", show p),
         ("limit", show l)]
    dateStr = formatTime defaultTimeLocale "%Y-%m-%d"
requestXML (GetDomain c domainName) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "GET" "DOMAIN" (srsIpAddress c)
        [("domain", domainName),
         ("type", "all_info"),
         ("limit", "10")]
requestXML (GetDomainTldData c domainName) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "GET" "DOMAIN" (srsIpAddress c)
        [("domain", domainName),
         ("type", "tld_data"),
         ("limit", "10")]
requestXML (GetDomainWithCookie c _ cookie) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ cookieRequest "GET" "DOMAIN" (srsIpAddress c) cookie
        [("type", "all_info")]
requestXML (LookupDomain c domainName) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "LOOKUP" "DOMAIN" (srsIpAddress c)
        [("domain", domainName),
         ("no_cache", "1")]
requestXML (RenewDomain c domainName autoRenew affiliateID currentExp handleNow period) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "RENEW" "DOMAIN" (srsIpAddress c)
        [("domain", domainName),
         ("auto_renew", auto_renew),
         ("affiliate_id", affiliateID),
         ("currentexpirationyear", show currentExp),
         ("handle", handle),
         ("period", show period)]
    auto_renew = if autoRenew then "1" else "0"
    handle = if handleNow then "process" else "save"
requestXML (ModifyDomain c domainName affect_domains rdata tld) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest' "MODIFY" "DOMAIN" (srsIpAddress c) ++ (attributes $ [
        itemNode "domain" domainName,
        itemNode "affect_domains" $ boolVal affect_domains
        ] ++ (itemMap rdata) ++ tldData)
    tldData = case tld of
        Nothing -> []
        Just tld' -> [itemParent "tld_data" [tag "dt_assoc" $
            itemParentMap (\vmap -> [tag "dt_assoc" $ itemMap vmap]) tld'
            ]]
requestXML (UpdateDomain c domain) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $
        genericRequest' "UPDATE_ALL_INFO" "DOMAIN" (srsIpAddress c) ++ (attributes $ domainToNodes domain)
requestXML (RegisterDomain c domain cc comments enc lock park priv handle period username password regtype tld) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $
        genericRequest' "SW_REGISTER" "DOMAIN" (srsIpAddress c) ++ (attributes $ domainToNodes domain ++ tldData ++ [
                itemNode "change_contact" $ boolVal cc,
                mayPair "comments" comments,
                mayPair "encoding_type" enc,
                itemNode "f_lock_domain" $ boolVal lock,
                itemNode "f_parkp" $ boolVal park,
                itemNode "f_whois_privacy" $ boolVal priv,
                itemNode "handle" $ boolVal handle,
                itemNode "period" $ show period,
                itemNode "reg_username" $ show username,
                itemNode "reg_password" $ show password,
                itemNode "reg_type" $ show regtype
                ])
    tldData = case tld of
        Nothing -> []
        Just tld' -> [itemParent "tld_data" [tag "dt_assoc" $
            itemParentMap (\vmap -> [tag "dt_assoc" $ itemMap vmap]) tld'
            ]]
requestXML (ChangeDomainPassword c domainName password) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "CHANGE" "PASSWORD" (srsIpAddress c)
        [("domain", domainName),
         ("reg_password", show password)]
requestXML (SendDomainPassword c domainName sendTo subUser) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "CHANGE" "PASSWORD" (srsIpAddress c)
        [("domain_name", domainName),
         ("send_to", sendTo),
         ("sub_user", boolVal subUser)]
requestXML (SetCookie c d u p) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "SET" "COOKIE" (srsIpAddress c)
        [("domain", d),
         ("reg_username", show u),
         ("reg_password", show p)]
requestXML _ = error "Not implemented yet"

-- | domain
domainToNodes :: Domain -> [Node]
domainToNodes domain = [
    itemNode "domain" (domainName domain),
    itemParent "contact_set" [
        tag "dt_assoc" (concatMap contactToNodes $ toList $ domainContactSet domain)
    ],
    itemParent "nameserver_list" [
        tag "dt_array" (concatMap nsToNodes $ zip [0..] $ domainNameservers domain)
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
genericRequest action object ip attr = genericRequest' action object ip ++ (makeAttr attr)

-- | preps some cookie-dependent request parameters
cookieRequest :: String -> String -> String -> SRSCookie -> [(String, String)] -> [Node]
cookieRequest action object ip cookie attr = cookieRequest' action object ip cookie ++ (makeAttr attr)

makeAttr :: [(String, String)] -> [Node]
makeAttr attr = case attr of
    [] -> []
    x  -> attributes $ Prelude.map attrMap attr
  where
    attrMap (k,v) = itemNode k v

genericRequest' :: String -> String -> String -> [Node]
genericRequest' action object ip = [
    itemNode "protocol" "XCP",
    itemNode "action" (Prelude.map toUpper action),
    itemNode "object" (Prelude.map toUpper object),
    itemNode "registrant_ip" ip ]

cookieRequest' :: String -> String -> String -> SRSCookie -> [Node]
cookieRequest' action object ip cookie = [
    itemNode "protocol" "XCP",
    itemNode "action" (Prelude.map toUpper action),
    itemNode "object" (Prelude.map toUpper object),
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
itemMap = Prelude.map (uncurry itemNode) . toList

-- | Map of parent nodes
itemParentMap :: (a -> [Node]) -> Map String a -> [Node]
itemParentMap fn = Prelude.map (\(k, v) -> itemParent k (fn v)) . toList

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
boolVal b = if b then "1" else "0"

-- | Wraps the entire request
wrapRequest :: [Node] -> [Node]
wrapRequest nodes = [tag "OPS_envelope" [
    tag "header" [ tag "version" [TextNode $ Text.pack "0.9"] ],
    tag "body" [ tag "data_block" [ tag "dt_assoc" nodes ]]
    ]]
