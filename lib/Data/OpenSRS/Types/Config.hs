module Data.OpenSRS.Types.Config where

--------------------------------------------------------------------------------
-- | OpenSRS Config
data SRSConfig = SRSConfig {
    srsEndpoint  :: String,
    srsUsername  :: String,
    srsKey       :: String,
    srsIpAddress :: String
} deriving (Eq, Show)
