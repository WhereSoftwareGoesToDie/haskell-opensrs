{-# LANGUAGE OverloadedStrings #-}

module Text.HTML.TagSoup.Pretty (
    prettyXML
) where

import Data.List
import Data.Monoid
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike

prettyXML :: String -> String -> String
prettyXML tabOut = (=<<) (prettyTree tabOut) . tagTree . parseTags

prettyTree :: (StringLike s) => String -> TagTree s -> String
prettyTree = prettyTree' ""

prettyTree' :: (StringLike s) => String -> String -> TagTree s -> String
prettyTree' prefix tabOut (TagLeaf t) = case renderTag t of
    "" -> ""
    x  -> concat [prefix, x, "\n"]
prettyTree' prefix tabOut (TagBranch n a tr) = concat [
    prefix, renderTag $ TagOpen n a, "\n",
    (=<<) (prettyTree' (prefix <> tabOut) tabOut) tr,
    prefix, renderTag $ TagClose n, "\n"]

renderTag :: (StringLike s) => Tag s -> String
renderTag (TagOpen s a)  = concat ["<", toString s, attrs, ">"]
  where
  	attrs = case renderAttrs a of
  		"" -> ""
  		x  -> " " <> x
renderTag (TagClose s)   = concat ["</", toString s, ">"]
renderTag (TagText s)    = toString s
renderTag (TagComment s) = concat ["<!-- ", toString s, " -->"]
renderTag _              = ""

renderAttrs :: (StringLike s) => [Attribute s] -> String
renderAttrs = unwords . fmap renderAttr
  where
    renderAttr (k,v) = case (strNull k, strNull v) of
    	(True, True)  -> ""
    	(False, True) -> toString k
    	(True, False) -> concat ["\"", toString v, "\""]
    	_             -> concat [toString k, "=\"", toString v, "\""]
