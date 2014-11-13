{-# LANGUAGE OverloadedStrings #-}

module Data.OpenSRS.Types.Domain where

import Data.Map
import Data.OpenSRS.Types.Common
import Data.Time

--------------------------------------------------------------------------------
-- | Domain Record
data Domain = Domain {
    domainName          :: DomainName,
    domainAutoRenew     :: Bool,
    domainContactSet    :: Map String Contact,
    domainUpdateDate    :: Maybe UTCTime,
    domainSponsoringRsp :: Bool,
    domainCreateDate    :: Maybe UTCTime,
    domainAffiliateID   :: Maybe String,
    domainLetExpire     :: Bool,
    domainExpireDate    :: Maybe UTCTime,
    domainNameservers   :: [Nameserver]
} deriving (Show, Eq)

-- | Domain Contacts
data Contact = Contact {
    contactFirstName  :: Maybe String,
    contactLastName   :: Maybe String,
    contactOrgName    :: Maybe String,
    contactEmail      :: Maybe String,
    contactPhone      :: Maybe String,
    contactFax        :: Maybe String,
    contactAddress1   :: Maybe String,
    contactAddress2   :: Maybe String,
    contactAddress3   :: Maybe String,
    contactCity       :: Maybe String,
    contactState      :: Maybe String,
    contactPostalCode :: Maybe String,
    contactCountry    :: Maybe String
} deriving (Show, Eq)

-- | Domain Nameservers
data Nameserver = Nameserver {
    nsName      :: Maybe String,
    nsSortorder :: Maybe String,
    nsIPAddr    :: Maybe String
} deriving (Show, Eq)
