{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Data.Neighborhood.Hex where

import Prelude hiding (Eq, Ord (..), reverse)
import qualified Prelude

import Data.Bits
import Data.Fin.List as Fin
import Data.Peano
import Data.Universe.Class
import Data.Word
import Relation.Binary.Comparison as A
import Util
import Util.Private.Bits

data Nbhd = N0 | N1 | N2o | N2m | N2p | N3v | N3a | N3s | N4o | N4m | N4p | N5 | N6
  deriving (Prelude.Eq, Enum, Bounded)

instance Universe Nbhd
instance Finite Nbhd

instance PartialEq Nbhd where (≡) = (==)
instance Eq Nbhd

instance Preord Nbhd where
    N0  ≤ _   = True
    _   ≤ N0  = False
    _   ≤ N6  = True
    N6  ≤ _   = False

    N1  ≤ _   = True
    _   ≤ N1  = False
    _   ≤ N5  = True
    N5  ≤ _   = False

    N2o ≤ N2m = False
    N2o ≤ N2p = False
    N2o ≤ _   = True
    N2m ≤ N2o = False
    N2m ≤ N2p = False
    N2m ≤ _   = True
    N2p ≤ N2o = False
    N2p ≤ N2m = False
    N2p ≤ N3v = False
    N2p ≤ N3s = False
    N2p ≤ _   = True
    N3v ≤ N3v = True
    N3v ≤ N4o = True
    N3v ≤ N4m = True
    N3v ≤ _   = False
    N3a ≤ N3a = True
    N3a ≤ N4o = True
    N3a ≤ N4m = True
    N3a ≤ N4p = True
    N3a ≤ _   = True
    N3s ≤ N3s = True
    N3s ≤ N4m = True
    N3s ≤ _   = False
    _   ≤ _   = False

instance PartialOrd Nbhd

instance Show Nbhd where
    show N0  = "0"
    show N1  = "1"
    show N2o = "2o"
    show N2m = "2m"
    show N2p = "2p"
    show N3v = "3v"
    show N3a = "3a"
    show N3s = "3s"
    show N4o = "4o"
    show N4m = "4m"
    show N4p = "4p"
    show N5  = "5"
    show N6  = "6"

complement :: Nbhd -> Nbhd
complement = \ case
    N0  -> N6
    N1  -> N5
    N2o -> N4o
    N2m -> N4m
    N2p -> N4p
    N4o -> N2o
    N4m -> N2m
    N4p -> N2p
    N5  -> N1
    N6  -> N0
    x   -> x

fromList :: List (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))) Bool -> Nbhd
fromList = fromListLE & isotropicize & \ case
    0x00 -> N0
    0x01 -> N1
    0x03 -> N2o
    0x05 -> N2m
    0x09 -> N2p
    0x07 -> N3v
    0x0B -> N3a
    0x15 -> N3s
    0x0F -> N4o
    0x17 -> N4m
    0x1B -> N4p
    0x1F -> N5
    _    -> N6
  where
    isotropicize x = foldr min maxBound [f . flip rotateWord6 k $! x | !k <- [0..3], f <- [id, reverseBits6]]

    rotateWord6 :: Word8 -> Int -> Word8
    rotateWord6 x k = (shiftL x k .|. shiftR x (6 - k)) .&. 0x3F

    reverseBits6 :: Word8 -> Word8
    reverseBits6 = reverseBits8 . flip shiftL 2
