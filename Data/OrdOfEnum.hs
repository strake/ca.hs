module Data.OrdOfEnum where

import Data.Function (on)
import Data.Universe.Class

newtype OrdOfEnum a = OrdOfEnum { unOrdOfEnum :: a }
  deriving (Universe, Finite)
instance Enum a => Eq (OrdOfEnum a) where
    (==) = on (==) (fromEnum . unOrdOfEnum)
instance Enum a => Ord (OrdOfEnum a) where
    compare = on compare (fromEnum . unOrdOfEnum)
