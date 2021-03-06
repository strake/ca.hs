module Data.Neighborhood.Hex where

import Prelude hiding (Eq, Ord (..))
import qualified Prelude

import Relation.Binary.Comparison as A

data Nbhd = N0 | N1 | N2o | N2m | N2p | N3v | N3a | N3s | N4o | N4m | N4p | N5 | N6
  deriving (Prelude.Eq, Enum)

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
