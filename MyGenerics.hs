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

instance ToRow Product where
 toRow product =
   [ toField (productId product)
   , toField (label product)
   , toField (toInteger $ price product)
   , toField (discreteCurrency $ price product)
   , toField (description product)
   ]

instance FromRow Product where
  fromRow = do
   let unitScale = scale (Proxy :: Proxy (UnitScale "EUR" "cent"))  --Used scale
   sd <- mkSomeDiscrete <$> field <*> pure unitScale <*> field      --Function for serialization
   case fromSomeDiscrete sd of                                      --Transformation to Maybe Discrete
     Nothing -> fail "Currency not supported"
     Just val -> Product <$> field <*> field <*> pure val <*> field --Product creation
