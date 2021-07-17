{-# LANGUAGE FlexibleInstances #-}

module Data.Binary.LE where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int

newtype LE a = LE { getLE :: a } deriving (Eq, Ord)

instance Binary (LE Word16) where
  get = LE <$> getWord16le
  put = putWord16le . getLE

instance Binary (LE Word32) where
  get = LE <$> getWord32le
  put = putWord32le . getLE

instance Binary (LE Word64) where
  get = LE <$> getWord64le
  put = putWord64le . getLE

instance Binary (LE Int16) where
  get = LE <$> getInt16le
  put = putInt16le . getLE

instance Binary (LE Int32) where
  get = LE <$> getInt32le
  put = putInt32le . getLE

instance Binary (LE Int64) where
  get = LE <$> getInt64le
  put = putInt64le . getLE

instance Binary (LE Float) where
  get = LE <$> getFloatle
  put = putFloatle . getLE

instance Binary (LE Double) where
  get = LE <$> getDoublele
  put = putDoublele . getLE
