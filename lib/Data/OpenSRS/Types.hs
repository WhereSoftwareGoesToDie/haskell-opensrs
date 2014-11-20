module Data.OpenSRS.Types (
    DomainName,

    SRSRequest (..),
    SRSResponse (..),
    DomainAvailability (..),
    DomainRenewal (..),
    DomainRegistration (..),
    RegistrationType (..),
    SRSResult (..),

    toUTC,
    toUTC',

    SRSConfig (..),

    DomainList (..),
    DomainListDomain (..),

    Domain (..),
    Contact (..),
    Nameserver (..),
    TLDData,

    XmlPost (..),
    Postable,
    Show,

    SRSUsername,
    makeUsername,
    unUsername,

    Password,
    makePassword,
    unPassword,

    SRSCookie,
    SRSCookieJar (..)
) where

import Data.Map
import Data.OpenSRS.Types.Common
import Data.OpenSRS.Types.Config
import Data.OpenSRS.Types.Domain
import Data.OpenSRS.Types.DomainList
import Data.OpenSRS.Types.XmlPost
import Data.Time
import Network.Wreq.Types (Postable)

--------------------------------------------------------------------------------
-- | OpenSRS Request
data SRSRequest = AllDomains {
    requestConfig :: SRSConfig
} | ListDomains {
    requestConfig :: SRSConfig,
    requestPage   :: Int,
    requestLimit  :: Int
} | ListDomainsByExpiry {
    requestConfig    :: SRSConfig,
    requestStartDate :: UTCTime,
    requestEndDate   :: UTCTime,
    requestPage      :: Int,
    requestLimit     :: Int
} | GetDomain {
    requestConfig     :: SRSConfig,
    requestDomainName :: DomainName
} | GetDomainWithCookie {
    requestConfig     :: SRSConfig,
    requestDomainName :: DomainName,
    requestCookie     :: SRSCookie
} | GetDomainTldData {
    requestConfig     :: SRSConfig,
    requestDomainName :: DomainName
} | LookupDomain {
    requestConfig     :: SRSConfig,
    requestDomainName :: DomainName
} | RenewDomain {
    requestConfig      :: SRSConfig,
    requestDomainName  :: DomainName,
    requestAutoRenew   :: Bool,
    requestAffiliateID :: String,
    requestExpiryYear  :: Int,
    requestHandleNow   :: Bool,
    requestPeriod      :: Int
} | ModifyDomain {
    requestConfig       :: SRSConfig,
    requestDomainName   :: DomainName,
    requestAffectLinked :: Bool,
    requestData         :: Map String String,
    requestTldData      :: Maybe TLDData
} | UpdateDomain {
    requestConfig :: SRSConfig,
    requestDomain :: Domain
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
    requestUsername      :: SRSUsername,
    requestPassword      :: Password,
    requestRegType       :: RegistrationType,
    requestTldData       :: Maybe TLDData
} | ChangeDomainPassword {
    requestConfig     :: SRSConfig,
    requestDomainName :: DomainName,
    requestPassword   :: Password
} | SendDomainPassword {
    requestConfig     :: SRSConfig,
    requestDomainName :: DomainName,
    requestSendTo     :: String,
    requestToSubuser  :: Bool
} | SetCookie {
    requestConfig     :: SRSConfig,
    requestDomainName :: DomainName,
    requestUsername   :: SRSUsername,
    requestPassword   :: Password
} deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Registration types
data RegistrationType = Landrush
                      | NewRegistration
                      | PremiumRegistration
                      | Transfer
                      | Sunrise deriving (Eq)

instance Show RegistrationType where
    show Landrush            = "landrush"
    show NewRegistration     = "new"
    show PremiumRegistration = "premium"
    show Transfer            = "transfer"
    show Sunrise             = "sunrise"

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
               | CookieResult { resultGetCookieJar :: SRSCookieJar }
               | TldDataResult { resultGetTldData :: TLDData }
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
    noRenewalCode   :: Int,
    noRenewalReason :: String
} deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Cookies
type SRSCookie = String

data SRSCookieJar = SRSCookieJar {
    cookieName        :: DomainName,
    cookieStr         :: SRSCookie,
    cookieDomainCount :: Int,
    cookieExpiry      :: UTCTime,
    cookieIsOwner     :: Bool,
    cookieLastAccess  :: Maybe UTCTime,
    cookieLastIP      :: String,
    cookiePermission  :: String,
    cookieWaitingReqs :: Int
} deriving (Show, Eq)

