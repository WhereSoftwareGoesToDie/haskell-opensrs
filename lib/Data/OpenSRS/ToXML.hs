{-# LANGUAGE OverloadedStrings #-}

module Data.OpenSRS.ToXML (requestXML) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack, split)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.CaseInsensitive as CI
import Data.Hash.MD5
import Data.List
import Data.Map
import Data.Maybe

import Data.Char
import Data.String (IsString)
import qualified Data.Text as Text
import Text.StringLike (fromString, toString, StringLike)

import Text.XmlHtml
import Blaze.ByteString.Builder

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
            tag "dt_assoc" [
                itemNode "domain" (domainName domain),
                itemParent "contact_set" [
                    tag "dt_assoc" (concat $ Prelude.map contactToNodes $ toList $ domainContactSet domain)
                    ],
                itemParent "nameserver_list" [
                    tag "dt_array" (concat $ Prelude.map nsToNodes $ zip [0..] $ domainNameservers domain)
                    ]
                ]]]
    -- contacts
    contactToNodes (k, c) = [
        itemParent k [
            tag "dt_assoc" [
                mkPair "first_name" contactFirstName c,
                mkPair "last_name" contactLastName c,
                mkPair "org_name" contactOrgName c,
                mkPair "email" contactEmail c,
                mkPair "phone" contactPhone c,
                mkPair "fax" contactFax c,
                mkPair "address1" contactAddress1 c,
                mkPair "address2" contactAddress2 c,
                mkPair "address3" contactAddress3 c,
                mkPair "city" contactCity c,
                mkPair "state" contactState c,
                mkPair "postal_code" contactPostalCode c,
                mkPair "country" contactCountry c
            ]]]
    mkPair k fn c = itemNode k $ maybe "" id (fn c)
    -- nameservers
    nsToNodes (k, ns) = [
        itemParent (show k) [
            tag "dt_assoc" [
                mkPair "name" nsName ns,
                mkPair "sortorder" nsSortorder ns,
                mkPair "ipaddress" nsIPAddr ns
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

-- | Simple tag with children
tag :: String -> [Node] -> Node
tag tn c = Element (Text.pack tn) [] c

-- | Wraps the entire request
wrapRequest :: [Node] -> [Node] 
wrapRequest nodes = [tag "OPS_envelope" [
    tag "header" [ tag "version" [TextNode $ Text.pack "0.9"] ],
    tag "body" [ tag "data_block" [ tag "dt_assoc" nodes ]]
    ]]
