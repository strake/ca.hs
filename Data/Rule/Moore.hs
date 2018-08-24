{-# OPTIONS_GHC -Wno-error=incomplete-patterns -Wno-error=incomplete-uni-patterns #-}

module Data.Rule.Moore (Rule, bits, tabulate, fromFn,
                        birth, death, survival, antisurvival) where

import Prelude hiding (Eq, Ord)
import qualified Prelude

import Control.Applicative
import Data.BitSet hiding (bits)
import Data.Bits
import Data.Bits.Bitwise (fromListLE, toListBE, toListLE)
import Data.Bool
import Data.Char
import Data.Foldable
import Data.LargeWord (Word128, Word256, LargeKey)
import qualified Data.List as List
import Data.Universe.Class
import Data.Universe.Instances.Base ()
import Data.Word
import Relation.Binary.Comparison
import Text.Read (Read (..), readP_to_Prec)
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import Util ((&), (∈), altMap, bind2)
import Util.Bits

import Data.Neighborhood.Moore as Neighborhood hiding (complement)
import qualified Data.Neighborhood.Moore as Neighborhood

newtype Rule = Rule (BitSet Word128)
  deriving (Prelude.Eq, Preord, PartialEq, Eq, PartialOrd, Bits, FiniteBits)

bits :: Rule -> Word128
bits (Rule (BitSet x)) = x

instance Read Rule where
    readPrec = readP_to_Prec $ \ _prec -> fmap (Rule . BitSet) $
        (\ b s -> s `shiftL` 64 .|. b) <$ ReadP.satisfy ((==) 'B' . toUpper) <*> nbhds <*
        optional (ReadP.char '/')      <* ReadP.satisfy ((==) 'S' . toUpper) <*> nbhds
      where nbhds :: ReadP Word128
            nbhds = fmap (foldr (flip setBit . fromEnum) zeroBits . asum) . many $
                    bind2 cfg ((− fromEnum' '0') . fromEnum' <$> ReadP.satisfy isDigit)
                    (many $ toLower <$> ReadP.satisfy ((∈ "cekainyqjrtwz") . toLower))

            (−) = (-)

            fromEnum' :: (Enum a, Integral b) => a -> b
            fromEnum' = fromIntegral . fromEnum

instance Show Rule where
    show = \ r -> asum ["B", showNbhds $ birth r, "/S", showNbhds $ survival r]
      where showNbhds [] = ""
            showNbhds xs | Just (n, xs) <- altMap (\ n -> (,) n <$> (flip List.stripPrefix xs =<< cfg n [])) [0..8] = show n ++ showNbhds xs
            showNbhds (x:xs) = show x ++ showNbhds xs

fromFn :: (Nbhd -> Bool -> Bool) -> Rule
fromFn = foldr (uncurry go) zeroBits . flip filter universe . uncurry
  where go nbhd cell = flip setBit $ fromEnum nbhd + bool 0 64 cell

birth, death, survival, antisurvival :: Rule -> [Nbhd]
birth = fmap toEnum . setBits . (.&. 0xFFFFFFFFFFFFFFFF) . bits
survival = fmap toEnum . setBits . (`shiftR` 64) . bits
death = flip filter [N0 ..] . (.) not . flip (∈) . survival
antisurvival = flip filter [N0 ..] . (.) not . flip (∈) . birth

tabulate :: Rule -> Word512
tabulate = fromListLE . \ r ->
    let checkRule (xs, x) = toNbhd xs ∈ bool birth survival x r
    in checkRule . arrange <$> [0..511]
  where toNbhd :: Word8 -> Nbhd
        toNbhd = isotropicize & \ case
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
            x -> (Neighborhood.complement . toNbhd . complement) x

        arrange :: Word16 -> (Word8, Bool)
        arrange = toListLE & \ [a, b, c, d, e, f, g, h, i,
                                _, _, _, _, _, _, _] -> (fromListLE [a, b, c, f, i, h, g, d], e)
        isotropicize x = minimum $ (\ k -> [x `rotateL` k, reverseBits x `rotateL` (k+1)]) `altMap` [0..7]

reverseBits :: FiniteBits a => a -> a
reverseBits = fromListLE . toListBE

type Word512 = LargeKey Word256 Word256

cfg :: Alternative f => Word -> [Char] -> f [Nbhd]
cfg 0 [] = pure [N0]
cfg 1 [] = cfg 1 "ce"
cfg 1 xs = (\ case 'c' -> pure N1c
                   'e' -> pure N1e
                   _   -> empty) `traverse` xs
cfg 2 [] = cfg 2 "cekain"
cfg 2 xs = (\ case 'c' -> pure N2c
                   'e' -> pure N2e
                   'k' -> pure N2k
                   'a' -> pure N2a
                   'i' -> pure N2i
                   'n' -> pure N2n
                   _   -> empty) `traverse` xs
cfg 3 [] = cfg 3 "cekainyqjr"
cfg 3 xs = (\ case 'c' -> pure N3c
                   'e' -> pure N3e
                   'k' -> pure N3k
                   'a' -> pure N3a
                   'i' -> pure N3i
                   'n' -> pure N3n
                   'y' -> pure N3y
                   'q' -> pure N3q
                   'j' -> pure N3j
                   'r' -> pure N3r
                   _   -> empty) `traverse` xs
cfg 4 [] = cfg 4 "cekainyqjrtwz"
cfg 4 xs = (\ case 'c' -> pure N4c
                   'e' -> pure N4e
                   'k' -> pure N4k
                   'a' -> pure N4a
                   'i' -> pure N4i
                   'n' -> pure N4n
                   'y' -> pure N4y
                   'q' -> pure N4q
                   'j' -> pure N4j
                   'r' -> pure N4r
                   't' -> pure N4t
                   'w' -> pure N4w
                   'z' -> pure N4z
                   _   -> empty) `traverse` xs
cfg 5 [] = cfg 5 "cekainyqjr"
cfg 5 xs = (\ case 'c' -> pure N5c
                   'e' -> pure N5e
                   'k' -> pure N5k
                   'a' -> pure N5a
                   'i' -> pure N5i
                   'n' -> pure N5n
                   'y' -> pure N5y
                   'q' -> pure N5q
                   'j' -> pure N5j
                   'r' -> pure N5r
                   _   -> empty) `traverse` xs
cfg 6 [] = cfg 6 "cekain"
cfg 6 xs = (\ case 'c' -> pure N6c
                   'e' -> pure N6e
                   'k' -> pure N6k
                   'a' -> pure N6a
                   'i' -> pure N6i
                   'n' -> pure N6n
                   _   -> empty) `traverse` xs
cfg 7 [] = cfg 7 "ce"
cfg 7 xs = (\ case 'c' -> pure N7c
                   'e' -> pure N7e
                   _   -> empty) `traverse` xs
cfg 8 [] = pure [N8]
cfg _ _  = empty
