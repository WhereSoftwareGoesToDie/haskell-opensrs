{-# LANGUAGE OverloadedStrings #-}

module Data.OpenSRS.ToXML (requestXML) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, split, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.CaseInsensitive as CI
import Data.Hash.MD5
import Data.List
import Data.Map
import Data.Maybe

import Data.Char
import Data.String (IsString)
import qualified Data.Text as Text
import Text.StringLike (StringLike, fromString, toString)

import Blaze.ByteString.Builder
import Text.XmlHtml

--------------------------------------------------------------------------------

import Data.OpenSRS.Types
import Data.OpenSRS.Types.Config

----------------------------------------
-- | Actually generates request XML
requestXML :: SRSRequest -> Document
requestXML (GetDomain (SRSConfig _ _ _ ip) domainName) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "GET" "DOMAIN" ip $
        [("domain", domainName),
         ("type", "all_info"),
         ("limit", "10")]
requestXML (LookupDomain (SRSConfig _ _ _ ip) domainName) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "LOOKUP" "DOMAIN" ip $
        [("domain", domainName),
         ("no_cache", "1")]
requestXML (RenewDomain (SRSConfig _ _ _ ip) domainName autoRenew affiliateID currentExp handleNow period) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $ genericRequest "LOOKUP" "DOMAIN" ip $
        [("domain", domainName),
         ("auto_renew", auto_renew),
         ("affiliate_id", affiliateID),
         ("currentexpirationyear", show currentExp),
         ("handle", handle),
         ("period", show period)]
    auto_renew = if autoRenew then "1" else "0"
    handle = if handleNow then "process" else "save"
requestXML (UpdateDomain (SRSConfig _ _ _ ip) domain) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $
        genericRequest' "UPDATE_ALL_INFO" "DOMAIN" ip ++ [itemParent "attributes" [
            tag "dt_assoc" $ domainToNodes domain ]]
requestXML (RegisterDomain (SRSConfig _ _ _ ip) domain cc comments enc lock park priv handle period username password regtype tld) = XmlDocument UTF8 doctype nodes
  where
    nodes = wrapRequest $
        genericRequest' "SW_REGISTER" "DOMAIN" ip ++ [itemParent "attributes" [
            tag "dt_assoc" $ (domainToNodes domain) ++ tldData ++ [
                itemNode "change_contact" $ boolVal cc,
                mayPair "comments" comments,
                mayPair "encoding_type" enc,
                itemNode "f_lock_domain" $ boolVal lock,
                itemNode "f_parkp" $ boolVal park,
                itemNode "f_whois_privacy" $ boolVal priv,
                itemNode "handle" $ boolVal handle,
                itemNode "period" $ show period,
                itemNode "reg_username" username,
                itemNode "reg_password" password,
                itemNode "reg_type" regtype
                ]]]
    tldData = case tld of
        Nothing -> []
        Just tld' -> [itemParent "tld_data" [tag "dt_assoc" $
            itemParentMap (\vmap -> [tag "dt_assoc" $ itemMap vmap]) tld'
            ]]

    boolVal b = if b then "1" else "0"

-- | domain
domainToNodes :: Domain -> [Node]
domainToNodes domain = [
    itemNode "domain" (domainName domain),
    itemParent "contact_set" [
        tag "dt_assoc" (concat $ Prelude.map contactToNodes $ toList $ domainContactSet domain)
    ],
    itemParent "nameserver_list" [
        tag "dt_array" (concat $ Prelude.map nsToNodes $ zip [0..] $ domainNameservers domain)
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
genericRequest action object ip attr = (genericRequest' action object ip) ++ attr'
  where
    attr' = case attr of
        [] -> []
        x  -> [ itemParent "attributes" [ tag "dt_assoc" $ Prelude.map attrMap attr ]]
    attrMap (k,v) = itemNode k v

genericRequest' :: String -> String -> String -> [Node]
genericRequest' action object ip = [
    itemNode "protocol" "XCP",
    itemNode "action" (Prelude.map toUpper action),
    itemNode "object" (Prelude.map toUpper object),
    itemNode "registrant_ip" ip ]

-- | Writes the standard doctype
doctype :: Maybe DocType
doctype = Just $ DocType (Text.pack "OPS_envelope") (System (Text.pack "ops.dtd")) NoInternalSubset

-- | Packs up a key/value pair node
itemNode :: String -> String -> Node
itemNode k v = Element (Text.pack "item") [(Text.pack "key", Text.pack k)] [TextNode $ Text.pack v]

-- | Packs up a key pair node with children nodes
itemParent :: String -> [Node] -> Node
itemParent k c = Element (Text.pack "item") [(Text.pack "key", Text.pack k)] c

-- | Straight map to items
itemMap :: Map String String -> [Node]
itemMap = Prelude.map (\(k, v) -> itemNode k v) . toList

-- | Map of parent nodes
itemParentMap :: (a -> [Node]) -> Map String a -> [Node]
itemParentMap fn = Prelude.map (\(k, v) -> itemParent k (fn v)) . toList

-- | Simple tag with children
tag :: String -> [Node] -> Node
tag tn c = Element (Text.pack tn) [] c

-- | Pack a maybe string value into an itemNode
mayPair :: String -> Maybe String -> Node
mayPair k v = itemNode k $ maybe "" id v

-- | Wraps the entire request
wrapRequest :: [Node] -> [Node]
wrapRequest nodes = [tag "OPS_envelope" [
    tag "header" [ tag "version" [TextNode $ Text.pack "0.9"] ],
    tag "body" [ tag "data_block" [ tag "dt_assoc" nodes ]]
    ]]
