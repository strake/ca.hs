module Data.Rule.Hex (Rule, bits, tabulate,
                      birth, death, survival, antisurvival,
                      isSelfComplementary) where

import Prelude hiding (Eq, Ord)
import qualified Prelude

import Control.Applicative
import Data.BitSet hiding (bits)
import Data.Bits
import Data.Bits.Bitwise (fromListLE, toListBE, toListLE)
import Data.Bool
import Data.Char
import Data.LargeWord (Word128)
import qualified Data.List as List
import Data.Word
import Relation.Binary.Comparison
import Text.Read (Read (..), readP_to_Prec)
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import Util ((&), (∈), bind2)
import Util.Bits

import Data.Neighborhood.Hex as Neighborhood

newtype Rule = Rule (BitSet Word32)
  deriving (Prelude.Eq, Preord, PartialEq, Eq, PartialOrd, Bits, FiniteBits)

bits :: Rule -> Word32
bits (Rule (BitSet x)) = x

instance Read Rule where
    readPrec = readP_to_Prec $ \ _prec -> fmap (Rule . BitSet) $
        (\ b s -> s `shiftL` 16 .|. b) <$ ReadP.satisfy ((==) 'B' . toUpper) <*> nbhds <*
        optional (ReadP.char '/')      <* ReadP.satisfy ((==) 'S' . toUpper) <*> nbhds
      where nbhds :: ReadP Word32
            nbhds = fmap (foldr (flip setBit . fromEnum) zeroBits . concat) . many $
                    bind2 cfg ((− fromEnum' '0') . fromEnum' <$> ReadP.satisfy isDigit)
                    (many $ toLower <$> ReadP.satisfy ((∈ "ompvas") . toLower))

            cfg :: Alternative f => Word -> [Char] -> f [Nbhd]
            cfg 0 [] = pure [N0]
            cfg 1 [] = pure [N1]
            cfg 2 [] = cfg 2 "omp"
            cfg 2 xs = (\ case 'o' -> pure N2o
                               'm' -> pure N2m
                               'p' -> pure N2p
                               _   -> empty) `traverse` xs
            cfg 3 [] = cfg 3 "vsa"
            cfg 3 xs = (\ case 'v' -> pure N3v
                               'a' -> pure N3a
                               's' -> pure N3s
                               'o' -> pure N3v
                               'm' -> pure N3a
                               'p' -> pure N3s
                               _   -> empty) `traverse` xs
            cfg 4 [] = cfg 4 "omp"
            cfg 4 xs = (\ case 'o' -> pure N4o
                               'm' -> pure N4m
                               'p' -> pure N4p
                               _   -> empty) `traverse` xs
            cfg 5 [] = pure [N5]
            cfg 6 [] = pure [N6]
            cfg _ _  = empty

            (−) = (-)

            fromEnum' :: (Enum a, Integral b) => a -> b
            fromEnum' = fromIntegral . fromEnum

instance Show Rule where
    show = \ r -> concat ["B", showNbhds $ birth r, "/S", showNbhds $ survival r]
      where showNbhds [] = ""
            showNbhds (N2o:N2m:N2p:xs) = "2" ++ showNbhds xs
            showNbhds (N3v:N3a:N3s:xs) = "3" ++ showNbhds xs
            showNbhds (N4o:N4m:N4p:xs) = "4" ++ showNbhds xs
            showNbhds (x:xs) = show x ++ showNbhds xs

birth, death, survival, antisurvival :: Rule -> [Nbhd]
birth = fmap toEnum . setBits . (.&. 0xFFFF) . bits
survival = fmap toEnum . setBits . (`shiftR` 16) . bits
death = flip filter [N0 ..] . (.) not . flip (∈) . survival
antisurvival = flip filter [N0 ..] . (.) not . flip (∈) . birth

isSelfComplementary :: Rule -> Bool
isSelfComplementary r =
    birth r    == (List.sortOn fromEnum $ Neighborhood.complement <$> death r) &&
    survival r == (List.sortOn fromEnum $ Neighborhood.complement <$> antisurvival r)

tabulate :: Rule -> Word128
tabulate = fromListLE . \ r ->
    let checkRule (xs, x) = toNbhd xs ∈ bool birth survival x r
    in checkRule . arrange <$> [0..127]
  where toNbhd :: Word8 -> Nbhd
        toNbhd = isotropicize & \ case
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
            0x3F -> N6

        arrange :: Word8 -> (Word8, Bool)
        arrange = toListLE & \ [a, b, c, d, e, f, g, _] -> (fromListLE [a, b, e, g, f, c], d)
        isotropicize x = minimum $ (\ k -> [x `rol` k, reverseBits x `shiftR` 2 `rol` k]) `concatMap` [0..5]

        infixl 8 `rol`
        x `rol` k = case k `mod` 6 of k -> (x `shiftL` k .|. x `shiftR` (6 - k)) .&. 0x3F

reverseBits :: FiniteBits a => a -> a
reverseBits = fromListLE . toListBE
