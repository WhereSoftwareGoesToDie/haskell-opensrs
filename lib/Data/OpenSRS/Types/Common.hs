{-# LANGUAGE OverloadedStrings #-}

module Data.OpenSRS.Types.Common (
    toUTC,
    toUTC',

    DomainName,

    Password,
    makePassword,
    unPassword
) where

import Data.List
import Data.String.Utils
import Data.Time

--------------------------------------------------------------------------------
-- | Converts strings to UTCTime
toUTC :: Maybe String -> Maybe UTCTime
toUTC Nothing = Nothing
toUTC (Just ds) = toUTC' ds

toUTC' :: String -> Maybe UTCTime
toUTC' = maybeRead

type DomainName = String

-- | Wrapper around strings representing passwords.
-- We need this because OpenSRS limits the types of characters represented in
-- domain passwords. (See p.985 of API docs)
newtype Password = Password { unPassword :: String } deriving (Eq)

-- | Construct a Password from a string.
makePassword :: String -> Maybe Password
makePassword p = if all (`elem` allowed) p
    then Just $ Password p
    else Nothing
  where
    allowed = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "[]()!@\\$^,.~|=-+_{}#"

instance Show Password where
    show (Password p) = p
