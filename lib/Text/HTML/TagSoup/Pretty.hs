{-# LANGUAGE OverloadedStrings #-}

module Text.HTML.TagSoup.Pretty (
    prettyXML
) where

-- Prettyprints XML strings. Used for debugging and inspection.

import Data.List
import Data.Monoid
import Data.Text hiding (concat, unwords)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike

-- | Prettyprint a valid XML string, with a string that acts as a
-- line delimiter.
prettyXML
	:: String -- ^ Line delimiter
	-> String -- ^ XML string
	-> String -- ^ Prettyprinted XML string
prettyXML tabOut = (=<<) (prettyTree tabOut) . tagTree . parseTags

-- | Prettyprints a single XML tag tree.
prettyTree
	:: (StringLike s)
	=> String -- ^ Line delimiter
	-> TagTree s -- ^ Single XML tag tree
	-> String -- ^ Prettyprinted XML string 
prettyTree = prettyTree' ""

-- | Prettyprints a single XML tag tree, using a specified prefix at the start
-- of each line.
prettyTree'
	:: (StringLike s)
	=> String -- ^ Prefix for current line
	-> String -- ^ Line delimiter
	-> TagTree s -- ^ Single XML tag tree
	-> String -- ^ Prettyprinted XML string 
prettyTree' prefix tabOut (TagLeaf t) = case renderTag t of
    "" -> ""
    x  -> concat [prefix, x, "\n"]
prettyTree' prefix tabOut (TagBranch n a tr) = concat [
    prefix, renderTag $ TagOpen n a, "\n",
    (=<<) (prettyTree' (prefix <> tabOut) tabOut) tr,
    prefix, renderTag $ TagClose n, "\n"]

-- | Renders a TagSoup XML tag as a string.
renderTag :: (StringLike s)
	=> Tag s -- ^ TagSoup XML tag
	-> String -- ^ String representation of tag
renderTag (TagOpen s a)  = concat ["<", toString s, attrs, ">"]
  where
  	attrs = case renderAttrs a of
  		"" -> ""
  		x  -> " " <> x
renderTag (TagClose s)   = concat ["</", toString s, ">"]
renderTag (TagText s)    = if strNull s then "" else stripStr $ toString s
renderTag (TagComment s) = concat ["<!-- ", toString s, " -->"]
renderTag _              = ""

-- | Renders a set of TagSoup XML attributes as a string.
renderAttrs :: (StringLike s)
	=> [Attribute s] -- ^ List of TagSoup XML attributes
	-> String -- ^ String representation of attributes
renderAttrs = unwords . fmap renderAttr
  where
    renderAttr (k,v) = case (strNull k, strNull v) of
    	(True, True)  -> ""
    	(False, True) -> toString k
    	(True, False) -> concat ["\"", toString v, "\""]
    	_             -> concat [toString k, "=\"", toString v, "\""]

-- | Strips whitespace from strings.
stripStr :: String -> String
stripStr = unpack . strip . pack
