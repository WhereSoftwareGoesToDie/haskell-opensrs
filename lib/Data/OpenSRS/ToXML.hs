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

-- | preps some generic request parameters
genericRequest :: String -> String -> String -> [(String, String)] -> [Node]
genericRequest action object ip attr = [
    itemNode "protocol" "XCP",
    itemNode "action" (Prelude.map toUpper action),
    itemNode "object" (Prelude.map toUpper object),
    itemNode "registrant_ip" ip] ++ attr'
  where
    attr' = case attr of
        [] -> []
        x  -> [Element (Text.pack "item") [(Text.pack "key", Text.pack "attributes")] [
            Element (Text.pack "dt_assoc") [] (Prelude.map attrMap attr)]]
    attrMap (k,v) = itemNode k v

-- | Writes the standard doctype
doctype :: Maybe DocType
doctype = Just $ DocType (Text.pack "OPS_envelope") (System (Text.pack "ops.dtd")) NoInternalSubset

-- | Packs up a key/value pair node
itemNode :: String -> String -> Node
itemNode k v = Element (Text.pack "item") [(Text.pack "key", Text.pack k)] [TextNode $ Text.pack v]

-- | Wraps the entire request
wrapRequest :: [Node] -> [Node] 
wrapRequest nodes = [Element (Text.pack "OPS_envelope") [] [
    Element (Text.pack "header") [] [
        Element (Text.pack "version") [] [TextNode $ Text.pack "0.9"]],
    Element (Text.pack "body") [] [
        Element (Text.pack "data_block") [] [
            Element (Text.pack "dt_assoc") [] nodes ]]]]
