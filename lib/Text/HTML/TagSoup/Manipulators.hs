{-# LANGUAGE OverloadedStrings #-}

module Text.HTML.TagSoup.Manipulators where

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

import Data.Char
import Data.String (IsString)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity
import Text.HTML.TagSoup.Tree
import Text.StringLike (StringLike, fromString, toString)

--------------------------------------------------------------------------------
-- | Get inner text of the next tag fitting this matcher
getText :: [Tag String] -> String -> String
getText xml matcher = itemInnerValue $ head $ sections (~== (matcher :: String)) xml

getText' :: (Eq a, StringLike a, Show a) => [Tag a] -> String -> String
getText' xml matcher = itemInnerValue' $ head $ sections (~== (matcher :: String)) xml

itemInnerValue :: [Tag String] -> String
itemInnerValue x = fromTagText (x !! 1)

itemInnerValue' :: (Eq a, StringLike a, Show a) => [Tag a] -> String
itemInnerValue' x = tagIdent (x !! 1)

topMatching :: (Eq a, StringLike a, Show a) => String -> [TagTree a] -> [TagTree a]
topMatching matcher = concat . Prelude.map (topMatching' matcher)

matchingKids :: (Eq a, StringLike a, Show a) => String -> [TagTree a] -> [TagTree a]
matchingKids matcher = concat . Prelude.map (matchingKids' matcher)

kidsWith :: (Eq a, StringLike a, Show a) => String -> [TagTree a] -> [TagTree a]
kidsWith matcher = concat . Prelude.map (topMatching matcher) . Prelude.map getKids

topMatching' :: (Eq a, StringLike a, Show a) => String -> TagTree a -> [TagTree a]
topMatching' matcher l@(TagLeaf _) = matchingKids' matcher l
topMatching' matcher b@(TagBranch _ _ kids) = case matchingKids' matcher b of
    [] -> concat $ Prelude.map (topMatching' matcher) kids
    x  -> [b]

matchingKids' :: (Eq a, StringLike a, Show a) => String -> TagTree a -> [TagTree a]
matchingKids' matcher t = case isMatch t matcher of
    True  -> [t]
    False -> []

-- | Determine if we can match this tag by full tag description or tag name
-- @TODO better matchers
isMatch :: (Eq a, StringLike a, Show a) => TagTree a -> String -> Bool
isMatch t matcher = (currentTag t) ~== matcher || (TagText $ currentTagText t) ~== (TagText matcher)

-- | Get Tag object (no children) for any TagTree
currentTag :: TagTree a -> Tag a
currentTag (TagLeaf t) = t
currentTag (TagBranch t a _) = head . flattenTree $ [TagBranch t a []]

-- | Get Tag identifier for any TagTree
currentTagText :: (Show a, StringLike a) => TagTree a -> String
currentTagText x = tagIdent $ currentTag x

-- | Get Attributes for any TagTree
currentTagAttr :: (Show a, StringLike a) => TagTree a -> [(String, String)]
currentTagAttr (TagLeaf _) = []
currentTagAttr (TagBranch _ a _) = Prelude.map (bimap toString toString) a

-- | Identify any Tag
tagIdent :: (Show a, StringLike a) => Tag a -> String
tagIdent t = toString $ tagIdent' t

tagIdent' :: (Show a, StringLike a) => Tag a -> a
tagIdent' (TagOpen x _)     = x
tagIdent' (TagClose x)      = x
tagIdent' (TagText x)       = x
tagIdent' (TagComment x)    = x
tagIdent' (TagWarning x)    = x
tagIdent' (TagPosition _ _) = error "Nope"

-- | Get children of any TagTree
getKids :: TagTree a -> [TagTree a]
getKids (TagLeaf _) = []
getKids (TagBranch _ _ k) = k
