module Data.Rule.Hex (Rule, bits,
                      birth, death, survival, antisurvival,
                      isSelfComplementary) where

import Prelude hiding (Eq, Ord)
import qualified Prelude

import Control.Applicative
import Data.BitSet hiding (bits)
import Data.Bits
import Data.Char
import qualified Data.List as List
import Data.Word
import Relation.Binary.Comparison
import Text.Read (Read (..), readP_to_Prec)
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import Util ((∈), bind2)
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
