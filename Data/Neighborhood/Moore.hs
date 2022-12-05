{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Data.Neighborhood.Moore where

import Prelude hiding (Eq, Ord (..), reverse)
import qualified Prelude

import qualified Data.Bits as Bits
import Data.Fin.List as Fin hiding (fromList)
import Data.Peano
import Data.Universe.Class
import Data.Word
import Relation.Binary.Comparison as A
import Util hiding (minimumBy)
import Util.Private.Bits

data Nbhd
  = N0 | N1c | N1e | N2c | N2e | N2k | N2a | N2i | N2n
  | N3c | N3e | N3k | N3a | N3i | N3n | N3y | N3q | N3j | N3r
  | N4c | N4e | N4k | N4a | N4i | N4n | N4y | N4q | N4j | N4r | N4t | N4w | N4z
  | N5c | N5e | N5k | N5a | N5i | N5n | N5y | N5q | N5j | N5r
  | N6c | N6e | N6k | N6a | N6i | N6n | N7c | N7e | N8
  deriving (Prelude.Eq, Enum, Bounded)

instance Universe Nbhd
instance Finite Nbhd

instance PartialEq Nbhd where (≡) = (==)

instance Show Nbhd where
    show N0 = "0"
    show N1c = "1c"
    show N1e = "1e"
    show N2c = "2c"
    show N2e = "2e"
    show N2k = "2k"
    show N2a = "2a"
    show N2i = "2i"
    show N2n = "2n"
    show N3c = "3c"
    show N3e = "3e"
    show N3k = "3k"
    show N3a = "3a"
    show N3i = "3i"
    show N3n = "3n"
    show N3y = "3y"
    show N3q = "3q"
    show N3j = "3j"
    show N3r = "3r"
    show N4c = "4c"
    show N4e = "4e"
    show N4k = "4k"
    show N4a = "4a"
    show N4i = "4i"
    show N4n = "4n"
    show N4y = "4y"
    show N4q = "4q"
    show N4j = "4j"
    show N4r = "4r"
    show N4t = "4t"
    show N4w = "4w"
    show N4z = "4z"
    show N5c = "5c"
    show N5e = "5e"
    show N5k = "5k"
    show N5a = "5a"
    show N5i = "5i"
    show N5n = "5n"
    show N5y = "5y"
    show N5q = "5q"
    show N5j = "5j"
    show N5r = "5r"
    show N6c = "6c"
    show N6e = "6e"
    show N6k = "6k"
    show N6a = "6a"
    show N6i = "6i"
    show N6n = "6n"
    show N7c = "7c"
    show N7e = "7e"
    show N8 = "8"

complement :: Nbhd -> Nbhd
complement N0 = N8
complement N1c = N7c
complement N1e = N7e
complement N2c = N6c
complement N2e = N6e
complement N2k = N6k
complement N2a = N6a
complement N2i = N6i
complement N2n = N6n
complement N3c = N5c
complement N3e = N5e
complement N3k = N5k
complement N3a = N5a
complement N3i = N5i
complement N3n = N5n
complement N3y = N5y
complement N3q = N5q
complement N3j = N5j
complement N3r = N5r
complement N5c = N3c
complement N5e = N3e
complement N5k = N3k
complement N5a = N3a
complement N5i = N3i
complement N5n = N3n
complement N5y = N3y
complement N5q = N3q
complement N5j = N3j
complement N5r = N3r
complement N6c = N2c
complement N6e = N2e
complement N6k = N2k
complement N6a = N2a
complement N6i = N2i
complement N6n = N2n
complement N7c = N1c
complement N7e = N1e
complement N8 = N0
complement x = x

fromList :: List (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))) Bool -> Nbhd
fromList = fromListLE & isotropicize & \ case
    0x00 -> N0
    0x01 -> N1c
    0x02 -> N1e
    0x05 -> N2c
    0x0A -> N2e
    0x09 -> N2k
    0x03 -> N2a
    0x22 -> N2i
    0x11 -> N2n
    0x15 -> N3c
    0x2A -> N3e
    0x29 -> N3k
    0x0E -> N3a
    0x07 -> N3i
    0x0D -> N3n
    0x25 -> N3y
    0x13 -> N3q
    0x0B -> N3j
    0x23 -> N3r
    0x55 -> N4c
    0xAA -> N4e
    0x2D -> N4k
    0x0F -> N4a
    0x36 -> N4i
    0x17 -> N4n
    0x35 -> N4y
    0x39 -> N4q
    0x2B -> N4j
    0x2E -> N4r
    0x27 -> N4t
    0x1B -> N4w
    0x33 -> N4z
    0xEA -> N5c
    0xD5 -> N5e
    0xD6 -> N5k
    0xF1 -> N5a
    0xF8 -> N5i
    0xF2 -> N5n
    0xDA -> N5y
    0xEC -> N5q
    0xF4 -> N5j
    0xDC -> N5r
    0xFA -> N6c
    0xF5 -> N6e
    0xF6 -> N6k
    0xFC -> N6a
    0xDD -> N6i
    0xEE -> N6n
    0xFE -> N7c
    0xFD -> N7e
    _    -> N8
  where
    isotropicize :: Word8 -> Word8
    isotropicize x = foldr min maxBound [f . flip Bits.rotate (2*k) $! x | !k <- [0..3], f <- [id, flip Bits.rotate 1 . reverseBits8]]
