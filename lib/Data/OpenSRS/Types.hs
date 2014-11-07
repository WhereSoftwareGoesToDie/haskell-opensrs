module Data.OpenSRS.Types (
    SRSRequest (..),
    SRSResponse (..),
    DomainAvailability (..),
    DomainRenewal (..),
    DomainRegistration (..),
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
    Show,

    Password,
    makePassword,
    unPassword
) where

import Data.Map
import Data.OpenSRS.Types.Common
import Data.OpenSRS.Types.Config
import Data.OpenSRS.Types.Domain
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
} | UpdateDomain {
    udConfig :: SRSConfig,
    udDomain :: Domain
} | RegisterDomain {
    rgConfig        :: SRSConfig,
    rgDomain        :: Domain,
    rgChangeContact :: Bool,
    rgComments      :: Maybe String,
    rgEncoding      :: Maybe String,
    rgLock          :: Bool,
    rgPark          :: Bool,
    rgWhoisPrivacy  :: Bool,
    rgHandleNow     :: Bool,
    rgPeriod        :: Int,
    rgUsername      :: String,
    rgPassword      :: String,
    rgType          :: String,
    rgTldData       :: Maybe (Map String (Map String String))
} | ChangeDomainPassword {
    chpwdConfig   :: SRSConfig,
    chpwdName     :: DomainName,
    chpwdPassword :: Password
} | SendDomainPassword {
    snpwdConfig  :: SRSConfig,
    snpwdName    :: DomainName,
    snpwdContact :: String,
    snpwdSubuser :: Bool
} deriving (Eq, Show)

requestConfig :: SRSRequest -> SRSConfig
requestConfig (GetDomain c _) = c
requestConfig (LookupDomain c _) = c
requestConfig (RenewDomain c _ _ _ _ _ _) = c
requestConfig (UpdateDomain c _) = c
requestConfig (RegisterDomain c _ _ _ _ _ _ _ _ _ _ _ _ _) = c
requestConfig (ChangeDomainPassword c _ _) = c
requestConfig (SendDomainPassword c _ _ _) = c

--------------------------------------------------------------------------------
-- | OpenSRS Response
data SRSResponse = SRSResponse {
    srsSuccess      :: Bool,
    srsResponseText :: String,
    srsResponseCode :: Int
} deriving (Eq, Show)

data SRSResult = DomainResult { resultGetDomain :: Domain }
               | DomainAvailabilityResult { resultGetAvailability :: DomainAvailability }
               | DomainRenewalResult { resultGetRenewal :: DomainRenewal }
               | DomainRegistrationResult { resultGetRegistration :: DomainRegistration }
               | GenericSuccess { resultGetMessage :: String } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Domain availability
data DomainAvailability = Available DomainName | Unavailable DomainName deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Domain registration
data DomainRegistration = DomainRegistration {
    registrationAsync         :: Maybe String,
    registrationError         :: Maybe String,
    registrationForcedPending :: Maybe String,
    registrationID            :: String,
    registrationQRID          :: Maybe String,
    registrationCode          :: String,
    registrationText          :: String,
    registrationTransferID    :: Maybe String,
    registrationWhois         :: String
} deriving (Show, Eq)

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
