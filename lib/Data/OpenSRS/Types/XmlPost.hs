{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.OpenSRS.Types.XmlPost where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.CaseInsensitive as CI

import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (HeaderName)
import Network.Wreq
import Network.Wreq.Types

--------------------------------------------------------------------------------
-- | Data Type to encapsulate
data XmlPost = XmlPost { xmlData :: BSL8.ByteString }

instance Show XmlPost where
    show = BSL8.unpack . xmlData

instance Postable XmlPost where
    postPayload = xmlPostPayload

xmlPostPayload :: XmlPost -> HTTP.Request -> IO HTTP.Request
xmlPostPayload x req = return $ req {
        HTTP.requestBody = HTTP.RequestBodyLBS $ xmlData x,
        HTTP.requestHeaders = h
    }
  where
    h = (HTTP.requestHeaders req) ++ eh
    eh = [(CI.mk $ pack "Content-Type", pack "text/xml"),
          (CI.mk $ pack "Accept-Encoding", pack "")]
