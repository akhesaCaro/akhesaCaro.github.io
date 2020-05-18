#!/usr/bin/env stack
{- stack
  script
  --resolver lts-13.19
  --nix-packages "postgresql"
  --package safe-money
  --package postgresql-simple
  --package text
  --package uuid
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables#-}

import Money
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Data.Text
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Data.UUID
import Data.Proxy

main :: IO ()
main = undefined

data Product =
    Product { productId :: Int
            , label :: Text
            , price :: Discrete "EUR" "cent"
            , description :: Text
            } deriving Show

data ProductJSON = ProductJSON
  { productId :: Int
  , label :: Text
  , amount :: Integer
  , currency :: Text
  , description :: Text
  } deriving (Generic, FromRow, ToRow)

toProductJSON :: Product -> ProductJSON
toProductJSON Product{..} =
  let amount = toInteger price
      currency = discreteCurrency price
  in ProductJSON{..}

data Errors = NOT_SUPPORTED_CURRENCY

fromProductJSON :: ProductJSON -> Either Errors Product -- I have a better "fail faster" way that I will talk in another article.
fromProductJSON ProductJSON {..} =
  case fromSomeDiscrete sd of
    Nothing -> Left NOT_SUPPORTED_CURRENCY
    Just (price :: Discrete "EUR" "cent") -> Right Product{..}
  where unitScale = scale (Proxy :: Proxy (UnitScale "EUR" "cent"))  --Used scale
        sd = mkSomeDiscrete currency unitScale amount
