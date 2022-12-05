module Util.Private.Bits where

import Data.Bits
import Data.Word

-- | Convert from a little-endian list of bits to 'Bits'.
{-# INLINE fromListLE #-}
fromListLE :: (Bits b, Foldable f) => f Bool {- ^ \[least significant bit, ..., most significant bit\] -} -> b
fromListLE = foldr f zeroBits
  where
    f b i = fromBool b .|. (i `shiftL` 1)

-- | The least significant bit.
{-# INLINE fromBool #-}
fromBool :: (Bits b) => Bool -> b
fromBool False = zeroBits
fromBool True  = bit 0

reverseBits8 :: Word8 -> Word8
reverseBits8 = \ x -> fromIntegral (shiftR (((fromIntegral x * 0x80200802 :: Word64) .&. 0x0884422110) * 0x0101010101) 32)
{-# INLINE reverseBits8 #-}
