{-# LANGUAGE OverloadedStrings #-}

module Data.OpenSRS.Types.DomainList where

import Data.OpenSRS.Types.Common
import Data.Time

--------------------------------------------------------------------------------
data DomainList = DomainList {
    domainListCount :: Int,
    domainListItems :: [DomainListDomain],
    domainListMore  :: Bool
} deriving (Eq, Show)

data DomainListDomain = DomainListDomain {
    dldName       :: DomainName,
    dldExpireDate :: UTCTime,
    dldAutoRenew  :: Bool,
    dldLetExpire  :: Bool
} deriving (Eq, Show)
