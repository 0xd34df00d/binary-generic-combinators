{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia, DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

import Data.Binary
import GHC.Generics
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

import Data.Binary.Combinators
import Data.Binary.DerivingVia

decenc :: Binary a => a -> a
decenc = decode . encode

data SimpleSumType1
  = SST11 (MatchBytes "test context" '[ 0xd3, 0x4d, 0xf0, 0x0d ])
  | SST12 (MatchBytes "test context" '[ 0xde, 0xad, 0xbe, 0xef ])
  deriving (Show, Eq, Generic)
  deriving Binary via Alternatively SimpleSumType1

data SimpleSumType2
  = SST21 (MatchBytes "test context" '[ 0xde, 0xad, 0xbe, 0xef ])
  | SST22 (MatchBytes "test context" '[ 0xd3, 0x4d, 0xf0, 0x0d ])
  deriving (Show, Eq, Generic)
  deriving Binary via Alternatively SimpleSumType2

data ComplexType
  = CT1
    { header1 :: MatchBytes "test context" '[ 0xaa, 0xbb, 0xff ]
    , skip1 :: SkipByte 0xff
    , skipCount1 :: SkipCount Word8 123
    , counted1 :: CountedBy Word16 Word8
    , rest1 :: Some Word8
    }
  | CT2
    { header2 :: MatchBytes "test context" '[ 0xdd, 0xea, 0xae ]
    , skipCount2 :: SkipCount Word16 9
    , counted2 :: CountedBy Word32 Word16
    , rest2 :: Some Word16
    }
  | CT3
    { header3 :: MatchBytes "test context" '[ 0xda, 0xdd, 0xee ]
    , skipCount3 :: SkipCount Int 17
    , rest3 :: Some Word16
    }
  deriving (Show, Eq, Generic)
  deriving Binary via Alternatively ComplexType

instance Arbitrary ComplexType where
  arbitrary = genericArbitrary
  shrink = genericShrink

idHolds :: (Binary a, Eq a, Show a) => a -> Expectation
idHolds val = decenc val `shouldBe` val

main :: IO ()
main = hspec $ do
  describe "Alternatively" $ do
    it "works for simple types" $ do
      let val = SST11 matchBytes in idHolds val
      let val = SST12 matchBytes in idHolds val
    it "is order-invariant" $ do
      decode (encode $ SST11 matchBytes) `shouldBe` SST22 matchBytes
  describe "decode . encode = id" $ do
    it "for Many" $ property $ \(xs :: Many Int) -> idHolds xs
    it "for Some" $ property $ \(xs :: Some Int) -> idHolds xs
    it "for CountedBy" $ property $ \(xs :: CountedBy Word16 Int) -> idHolds xs
    it "for complex types" $ property $ \(val :: ComplexType) -> idHolds val
