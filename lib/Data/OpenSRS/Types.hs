module Data.OpenSRS.Types (
    SRSRequest (..),
    SRSResponse (..),
    DomainAvailability (..),
    DomainRenewal (..),
    DomainRegistration (..),
    SRSResult (..),

    toUTC,
    toUTC',

    SRSConfig (..),

    DomainList (..),
    DomainListDomain (..),

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
import Data.Time
import Network.Wreq.Types (Postable)

--------------------------------------------------------------------------------
-- | OpenSRS Request
data SRSRequest = AllDomains {
    requestConfig        :: SRSConfig
} | ListDomains {
    requestConfig        :: SRSConfig,
    requestPage          :: Int,
    requestLimit         :: Int
} | ListDomainsByExpiry {
    requestConfig        :: SRSConfig,
    requestStartDate     :: UTCTime,
    requestEndDate       :: UTCTime,
    requestPage          :: Int,
    requestLimit         :: Int
} | GetDomain {
    requestConfig        :: SRSConfig,
    requestDomainName    :: DomainName
} | LookupDomain {
    requestConfig        :: SRSConfig,
    requestDomainName    :: DomainName
} | RenewDomain {
    requestConfig        :: SRSConfig,
    requestDomainName    :: DomainName,
    requestAutoRenew     :: Bool,
    requestAffiliateID   :: String,
    requestExpiryYear    :: Int,
    requestHandleNow     :: Bool,
    requestPeriod        :: Int
} | UpdateDomain {
    requestConfig        :: SRSConfig,
    requestDomain        :: Domain
} | RegisterDomain {
    requestConfig        :: SRSConfig,
    requestDomain        :: Domain,
    requestChangeContact :: Bool,
    requestComments      :: Maybe String,
    requestEncoding      :: Maybe String,
    requestLock          :: Bool,
    requestPark          :: Bool,
    requestWhoisPrivacy  :: Bool,
    requestHandleNow     :: Bool,
    requestPeriod        :: Int,
    requestUsername      :: String,
    requestPassword      :: Password,
    requestRegType       :: String,
    requestTldData       :: Maybe (Map String (Map String String))
} | ChangeDomainPassword {
    requestConfig        :: SRSConfig,
    requestDomainName    :: DomainName,
    requestPassword      :: Password
} | SendDomainPassword {
    requestConfig        :: SRSConfig,
    requestDomainName    :: DomainName,
    requestSendTo        :: String,
    requestToSubuser     :: Bool
} | SetCookie {
    requestConfig        :: SRSConfig
} deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | OpenSRS Response
data SRSResponse = SRSResponse {
    srsSuccess      :: Bool,
    srsResponseText :: String,
    srsResponseCode :: Int
} deriving (Eq, Show)

data SRSResult = DomainListResult { resultGetList :: DomainList }
               | DomainResult { resultGetDomain :: Domain }
               | DomainAvailabilityResult { resultGetAvailability :: DomainAvailability }
               | DomainRenewalResult { resultGetRenewal :: DomainRenewal }
               | DomainRegistrationResult { resultGetRegistration :: DomainRegistration }
               | GenericSuccess { resultGetMessage :: String } deriving (Eq, Show)

--------------------------------------------------------------------------------
data DomainList = DomainList {
    domainListCount :: Int,
    domainListItems :: [DomainListDomain],
    domainListMore  :: Bool
} deriving (Eq, Show)

data DomainListDomain = DomainListDomain {
    dldName          :: DomainName,
    dldExpireDate    :: UTCTime,
    dldAutoRenew     :: Bool,
    dldLetExpire     :: Bool
} deriving (Eq, Show)

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
