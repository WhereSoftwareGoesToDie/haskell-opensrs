{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.OpenSRS.ToXML (requestXML) where

import Data.Char
import Data.Map
import Data.Maybe
import Data.OpenSRS.Types
import qualified Data.Text as Text
import Data.Time
import System.Locale (defaultTimeLocale)
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
requestXML GetDomain{..} = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "GET" "DOMAIN" (srsIpAddress requestConfig)
        [("domain", requestDomainName),
         ("type", "all_info"),
         ("limit", "10")]
requestXML (GetDomainTldData c domain_name) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "GET" "DOMAIN" (srsIpAddress c)
        [("domain", domain_name),
         ("type", "tld_data"),
         ("limit", "10")]
requestXML (GetDomainWithCookie c _ cookie) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ cookieRequest "GET" "DOMAIN" (srsIpAddress c) cookie
        [("type", "all_info")]
requestXML (LookupDomain c domain_name) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "LOOKUP" "DOMAIN" (srsIpAddress c)
        [("domain", domain_name),
         ("no_cache", "1")]
requestXML (RenewDomain c domain_name autoRenew affiliateID currentExp handle_now period) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "RENEW" "DOMAIN" (srsIpAddress c)
        [("domain", domain_name),
         ("auto_renew", boolVal autoRenew),
         ("affiliate_id", affiliateID),
         ("currentexpirationyear", show currentExp),
         ("handle", boolVal' "process" "save" handle_now),
         ("period", show period)]
requestXML (ModifyDomain c domain_name affect_domains rdata tld) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest' "MODIFY" "DOMAIN" (srsIpAddress c) ++
        attributes (
            [ itemNode "domain" domain_name
            , itemNode "affect_domains" $ boolVal affect_domains
            ] ++ itemMap rdata ++ tldData)
    tldData = case tld of
        Nothing -> []
        Just tld' -> [itemParent "tld_data" [tag "dt_assoc" $
            itemParentMap (\vmap -> [tag "dt_assoc" $ itemMap vmap]) tld'
            ]]
requestXML (UpdateDomain c domain) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $
        genericRequest' "UPDATE_ALL_INFO" "DOMAIN" (srsIpAddress c) ++ attributes
            (domainToNodes domain)
requestXML (RegisterDomain c domain cc comments enc lock park priv handle period username password regtype tld) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $
        genericRequest' "SW_REGISTER" "DOMAIN" (srsIpAddress c) ++ attributes
            (domainToNodes domain ++ tldData ++ [
                itemNode "change_contact" $ boolVal cc,
                mayPair "comments" comments,
                mayPair "encoding_type" enc,
                itemNode "f_lock_domain" $ boolVal lock,
                itemNode "f_parkp" $ boolVal park,
                itemNode "f_whois_privacy" $ boolVal priv,
                itemNode "handle" $ boolVal' "process" "save" handle,
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
requestXML (ChangeDomainOwnership c domain_name username password) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "CHANGE" "OWNERSHIP" (srsIpAddress c)
        [("domain", domain_name),
         ("username", show username),
         ("password", show password)]
requestXML (SendDomainPassword c domain_name sendTo subUser) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "SEND_PASSWORD" "DOMAIN" (srsIpAddress c)
        [("domain_name", domain_name),
         ("send_to", sendTo),
         ("sub_user", boolVal subUser)]
requestXML (SetCookie c d u p) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "SET" "COOKIE" (srsIpAddress c)
        [("domain", d),
         ("reg_username", show u),
         ("reg_password", show p)]

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
genericRequest action object ip attr =
    genericRequest' action object ip ++ makeAttr attr

-- | preps some cookie-dependent request parameters
cookieRequest :: String -> String -> String -> SRSCookie -> [(String, String)] -> [Node]
cookieRequest action object ip cookie attr =
    cookieRequest' action object ip cookie ++ makeAttr attr

makeAttr :: [(String, String)] -> [Node]
makeAttr attr = case attr of
    [] -> []
    _  -> attributes $ Prelude.map attrMap attr
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
