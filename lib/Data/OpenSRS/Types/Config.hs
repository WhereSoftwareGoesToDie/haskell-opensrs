module Data.OpenSRS.Types.Config where

import Data.OpenSRS.Types.Common

--------------------------------------------------------------------------------
-- | OpenSRS Config
data SRSConfig = SRSConfig {
    srsEndpoint  :: String,
    srsUsername  :: String,
    srsKey       :: String,
    srsIpAddress :: String
} deriving (Eq, Show)
