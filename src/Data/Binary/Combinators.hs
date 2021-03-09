{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, GADTs #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveTraversable #-}
{-# LANGUAGE Safe #-}

{-| Description : Combinators and utility types for making 'GHC.Generics.Generic'-based 'Binary' instances more expressive.

This module defines a bunch of types to be used as fields of records with 'GHC.Generics.Generic'-based deriving of 'Binary' instances.
For example:

@
data MyFileFormat = MyFileFormat
  { header :: MatchBytes "my format header" '[ 0xd3, 0x4d, 0xf0, 0x0d ]
  , slack :: SkipByte 0xff
  , reserved :: SkipCount Word8 4
  , subElements :: Some MyElement
  } deriving (Generic, Binary)
@
-}

module Data.Binary.Combinators
( Many(..)
, Some(..)
, CountedBy(..)
, SkipCount(..)
, SkipByte(..)
, MatchBytes
, matchBytes
, MatchByte
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


-- | Zero or more elements of @a@, parsing as long as the parser for @a@ succeeds.
--
-- @Many Word8@ will consume all your input!
newtype Many a = Many { getMany :: [a] } deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Many a) where
  show = show . getMany

instance Binary a => Binary (Many a) where
  get = Many <$> many get
  put = mapM_ put . getMany

instance Arbitrary a => Arbitrary (Many a) where
  arbitrary = Many <$> arbitrary
  shrink (Many xs) = Many <$> shrink xs


-- | One or more elements of @a@, parsing as long as the parser for @a@ succeeds.
--
-- @Some Word8@ will consume all your non-empty input!
newtype Some a = Some { getSome :: [a] } deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Some a) where
  show = show . getSome

instance Binary a => Binary (Some a) where
  get = Some <$> some get
  put = mapM_ put . getSome

instance Arbitrary a => Arbitrary (Some a) where
  arbitrary = Some . getNonEmpty <$> arbitrary
  shrink (Some xs) = Some <$> filter (not . null) (shrink xs)


-- | First, parse the elements count as type @ty@. Then, parse exactly as many elements of type @a@.
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


-- | Parse out and skip @n@ elements of type @ty@.
--
-- Serializing this type produces no bytes.
data SkipCount ty (n :: Nat) = SkipCount deriving (Eq, Ord, Show)

instance (Num ty, Binary ty, KnownNat n) => Binary (SkipCount ty n) where
  get   = replicateM_ (fromIntegral $ natVal (Proxy :: Proxy n)) (get :: Get ty) $> SkipCount
  put _ = replicateM_ (fromIntegral $ natVal (Proxy :: Proxy n)) $ put (0 :: ty)

instance Arbitrary (SkipCount ty n) where
  arbitrary = pure SkipCount


-- | Skip any number of bytes with value @n@.
--
-- Serializing this type produces no bytes.
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


-- | @MatchBytes str bytes@ ensures that the subsequent bytes in the input stream are the same as @bytes@.
--
-- @str@ can be used to denote the context in which @MatchBytes@ is used for better parse failure messages.
-- For example, @MatchBytes "my format header" '[ 0xd3, 0x4d, 0xf0, 0x0d ]@ consumes four bytes from the input stream
-- if they are equal to @[ 0xd3, 0x4d, 0xf0, 0x0d ]@ respectively, or fails otherwise.
--
-- Serializing this type produces the @bytes@.
data MatchBytes (ctx :: Symbol) (bytes :: [Nat]) :: Type where
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

-- | Produce the (singleton) value of type 'MatchBytes' @ctx ns@.
matchBytes :: MatchBytesSing ctx ns => MatchBytes ctx ns
matchBytes = matchBytesSing

instance MatchBytesSing ctx ns => Arbitrary (MatchBytes ctx ns) where
  arbitrary = pure matchBytes

-- | An alias for 'MatchBytes' when you only need to match a single byte.
type MatchByte ctx byte = MatchBytes ctx '[ byte ]
