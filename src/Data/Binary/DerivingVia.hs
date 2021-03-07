{-# LANGUAGE TypeOperators, FlexibleContexts, UndecidableInstances #-}

module Data.Binary.DerivingVia
( Alternatively
) where

import Control.Applicative
import Data.Binary
import GHC.Generics

newtype Alternatively a = Alternatively { getAlt :: a }

class GAltBinary grecord where
  gAltGet :: Get (grecord p)
  gAltPut :: grecord p -> Put

instance Binary grecord => GAltBinary (K1 i grecord) where
  gAltGet = K1 <$> get
  gAltPut = put . unK1

instance GAltBinary grecord => GAltBinary (M1 i t grecord) where
  gAltGet = M1 <$> gAltGet
  gAltPut = gAltPut . unM1

instance (GAltBinary l, GAltBinary r) => GAltBinary (l :*: r) where
  gAltGet = (:*:) <$> gAltGet <*> gAltGet
  gAltPut (l :*: r) = gAltPut l <> gAltPut r

instance (GAltBinary l, GAltBinary r) => GAltBinary (l :+: r) where
  gAltGet = L1 <$> gAltGet
        <|> R1 <$> gAltGet
  gAltPut (L1 l) = gAltPut l
  gAltPut (R1 r) = gAltPut r

instance (Generic a, GAltBinary (Rep a)) => Binary (Alternatively a) where
  get = Alternatively . to <$> gAltGet
  put = gAltPut . from . getAlt
