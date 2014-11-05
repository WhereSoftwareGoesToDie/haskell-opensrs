module Data.OpenSRS.Types (
    SRSRequest (..),
    SRSResponse (..),
    DomainAvailability (..),
    DomainRenewal (..),
    SRSResult (..),

    requestConfig,

    toUTC,
    toUTC',

    SRSConfig (..),

    Domain (..),
    Contact (..),
    Nameserver (..),

    XmlPost (..),
    Postable,
    Show
) where

import Data.OpenSRS.Types.Common
import Data.OpenSRS.Types.Config
import Data.OpenSRS.Types.DomainGet
import Data.OpenSRS.Types.XmlPost
import Network.Wreq.Types (Postable)

--------------------------------------------------------------------------------
-- | OpenSRS Request
data SRSRequest = GetDomain {
    gdConfig :: SRSConfig,
    gdName   :: DomainName
} | LookupDomain {
    ldConfig :: SRSConfig,
    ldName   :: DomainName
} | RenewDomain {
    rdConfig         :: SRSConfig,
    rdName           :: DomainName,
    rdAutoRenew      :: Bool,
    rdAffiliateID    :: String,
    rdCurrentExpYear :: Int,
    rdHandleNow      :: Bool,
    rdPeriod         :: Int
} deriving (Eq, Show)

requestConfig :: SRSRequest -> SRSConfig
requestConfig (GetDomain c _) = c
requestConfig (LookupDomain c _) = c
requestConfig (RenewDomain c _ _ _ _ _ _) = c

--------------------------------------------------------------------------------
-- | OpenSRS Response
data SRSResponse = SRSResponse {
    srsSuccess      :: Bool,
    srsResponseText :: String,
    srsResponseCode :: Int
} deriving (Eq, Show)

data SRSResult = DomainResult Domain
               | DomainAvailabilityResult DomainAvailability
               | DomainRenewalResult DomainRenewal deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Domain availability
data DomainAvailability = Available DomainName | Unavailable DomainName deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Domain renewal
data DomainRenewal = Renewed {
    renewalName          :: DomainName,
    renewalAdminEmail    :: String,
    renewalAutoRenew     :: Bool,
    renewalOrderID       :: String,
    renewalQRID          :: Maybe String,
    renewalID            :: Maybe String,
    renewalNewExpiration :: String
} | NotRenewed {
    noRenewalName   :: DomainName,
    noRenewalReason :: String
} deriving (Show, Eq)
