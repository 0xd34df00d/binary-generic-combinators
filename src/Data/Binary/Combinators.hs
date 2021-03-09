{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, GADTs #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveTraversable #-}

module Data.Binary.Combinators
( Many(..)
, Some(..)
, CountedBy(..)
, SkipCount(..)
, SkipByte(..)
, MatchBytes
, matchBytes
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
import Test.QuickCheck


newtype Many a = Many { getMany :: [a] } deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Many a) where
  show = show . getMany

instance Binary a => Binary (Many a) where
  get = Many <$> many get
  put = mapM_ put . getMany

instance Arbitrary a => Arbitrary (Many a) where
  arbitrary = Many <$> arbitrary
  shrink (Many xs) = Many <$> shrink xs


newtype Some a = Some { getSome :: [a] } deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Some a) where
  show = show . getSome

instance Binary a => Binary (Some a) where
  get = Some <$> some get
  put = mapM_ put . getSome

instance Arbitrary a => Arbitrary (Some a) where
  arbitrary = Some . getNonEmpty <$> arbitrary
  shrink (Some xs) = Some <$> filter (not . null) (shrink xs)


newtype CountedBy ty a = CountedBy { getCounted :: [a] } deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (CountedBy ty a) where
  show = show . getCounted

instance (Integral ty, Binary ty, Binary a) => Binary (CountedBy ty a) where
  get = do cnt :: ty <- get
           CountedBy <$> replicateM (fromIntegral cnt) get
  put (CountedBy xs) = put (fromIntegral $ length xs :: ty) >> mapM_ put xs

instance Arbitrary a => Arbitrary (CountedBy ty a) where
  arbitrary = CountedBy <$> arbitrary
  shrink (CountedBy xs) = CountedBy <$> shrink xs


data SkipCount ty (n :: Nat) = SkipCount deriving (Eq, Ord, Show)

instance (Num ty, Binary ty, KnownNat n) => Binary (SkipCount ty n) where
  get   = replicateM_ (fromIntegral $ natVal (Proxy :: Proxy n)) (get :: Get ty) $> SkipCount
  put _ = replicateM_ (fromIntegral $ natVal (Proxy :: Proxy n)) $ put (0 :: ty)

instance Arbitrary (SkipCount ty n) where
  arbitrary = pure SkipCount


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

instance Arbitrary (SkipByte n) where
  arbitrary = pure SkipByte


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

class MatchBytesSing ctx ns where
  matchBytesSing :: MatchBytes ctx ns

instance MatchBytesSing ctx '[] where
  matchBytesSing = ConsumeNil

instance (KnownNat n, MatchBytesSing ctx ns) => MatchBytesSing ctx (n ': ns) where
  matchBytesSing = ConsumeCons Proxy matchBytesSing

matchBytes :: MatchBytesSing ctx ns => MatchBytes ctx ns
matchBytes = matchBytesSing

instance MatchBytesSing ctx ns => Arbitrary (MatchBytes ctx ns) where
  arbitrary = pure matchBytes
