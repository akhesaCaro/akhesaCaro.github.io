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
    Product { productId :: UUID
            , label :: Text
            , price :: Discrete "EUR" "cent"
            , description :: Text
            }

instance ToRow Product where
 toRow product =
   [ toField (productId product)
   , toField (label product)
   , toField (toInteger $ price product)
   , toField (description product)
   ]

instance FromRow Product where
  fromRow = do
   (amount, currency) <- (,) <$> field <*> field
   let msd = fromSomeDiscrete $ mkSomeDiscrete currency sc amount
       sc = scale (Proxy :: Proxy (UnitScale "EUR" "cent"))
   Product <$> field <*> field <*> maybe (fail "Currency not supported") pure msd <*> field
