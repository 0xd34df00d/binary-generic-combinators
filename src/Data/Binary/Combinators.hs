{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, GADTs #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Binary.Combinators
( Some(..)
, CountedBy(..)
, SkipCount(..)
, SkipByte(..)
, MatchBytes
) where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Get(lookAhead)
import Data.Functor
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Numeric

newtype Some a = Some { getSome :: [a] } deriving (Eq, Ord)

instance Show a => Show (Some a) where
  show = show . getSome

instance Binary a => Binary (Some a) where
  get = Some <$> some get
  put = mapM_ put . getSome


newtype CountedBy ty a = CountedBy { getCounted :: [a] } deriving (Eq, Ord)

instance Show a => Show (CountedBy ty a) where
  show = show . getCounted

instance (Integral ty, Binary ty, Binary a) => Binary (CountedBy ty a) where
  get = do cnt :: ty <- get
           CountedBy <$> replicateM (fromIntegral cnt) get
  put (CountedBy xs) = put (fromIntegral $ length xs :: ty) >> mapM_ put xs


data SkipCount ty (n :: Nat) = SkipCount deriving (Eq, Ord, Show)

instance (Num ty, Binary ty, KnownNat n) => Binary (SkipCount ty n) where
  get   = replicateM_ (fromIntegral $ natVal (Proxy :: Proxy n)) (get :: Get ty) $> SkipCount
  put _ = replicateM_ (fromIntegral $ natVal (Proxy :: Proxy n)) $ put (0 :: ty)


data SkipByte (n :: Nat) = SkipByte deriving (Eq, Ord, Show)

instance (KnownNat n) => Binary (SkipByte n) where
  get   = do nextByte <- lookAhead get
             if nextByte /= expected
             then pure SkipByte
             else (get :: Get Word8) >> get
    where
      expected :: Word8
      expected = fromIntegral $ natVal (Proxy :: Proxy n)
  put _ = pure ()


data MatchBytes :: Symbol -> [Nat] -> Type where
  ConsumeNil  :: MatchBytes ctx '[]
  ConsumeCons :: KnownNat n => Proxy n -> MatchBytes ctx ns -> MatchBytes ctx (n ': ns)

deriving instance Eq (MatchBytes s ns)
deriving instance Ord (MatchBytes s ns)

instance Binary (MatchBytes ctx '[]) where
  get = pure ConsumeNil
  put _ = pure ()

instance (KnownSymbol ctx, KnownNat n, Binary (MatchBytes ctx ns)) => Binary (MatchBytes ctx (n : ns)) where
  get = do byte <- get
           when (byte /= expected) $ fail $ "Unexpected byte 0x" <> showHex byte ", expected 0x"
                                                                 <> showHex expected " when parsing "
                                                                 <> symbolVal (Proxy :: Proxy ctx)
           ConsumeCons Proxy <$> get
    where
      expected :: Word8
      expected = fromInteger $ natVal (Proxy :: Proxy n)

  put (ConsumeCons proxy ns) = put theByte >> put ns
    where
      theByte :: Word8
      theByte = fromInteger $ natVal proxy

instance Show (MatchBytes ctx ns) where
  show bs = "Marker [ " <> go bs <> "]"
    where
      go :: MatchBytes ctx' ns' -> String
      go ConsumeNil = ""
      go (ConsumeCons proxy ns) = "0x" <> showHex (natVal proxy) " " <> go ns
