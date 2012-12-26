{- |

Module      :  Data.Vector.Bit
Description :  Simple bit vectors for Haskell
Copyright   :  (c) Adam C. Foltzer 2011
License     :  BSD3

Maintainer  :  acfoltzer@gmail.com
Stability   :  experimental
Portability :  non-portable

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Vector.Bit (
  -- * Bit vectors
  BitVector,

  -- * Conversions
  
  -- ** To and from other 'Bits' instances
  unpack, pack,

  -- ** Specialized conversions
  unpackInteger, packInteger, unpackInt, packInt,

  -- * Utilities
  pad, padMax, zipPad, zipPadWith, trimLeading
  )

where

import Data.Bits
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as V

-- | A 'BitVector' is a little-endian 'V.Vector' of
-- 'Bool's.
type BitVector = V.Vector Bool

-- | Pads a 'BitVector' to the specified length by adding a vector of
-- 'False' values to the most-significant end.
pad :: Int -> BitVector -> BitVector
pad i v = v V.++ V.replicate (i - V.length v) False

-- | Pads two 'BitVector's to the length of the longest vector. If the
-- vectors are the same length, 'padMax' does nothing.
padMax :: BitVector -> BitVector -> (BitVector, BitVector)
padMax xs ys = (padlen xs, padlen ys)
  where
    padlen = pad $ max (V.length xs) (V.length ys)

-- | Like 'V.zip', except pads the vectors to equal length
-- rather than discarding elements of the longer vector.
zipPad :: BitVector -> BitVector -> V.Vector (Bool, Bool)
zipPad xs ys = uncurry V.zip (padMax xs ys)

-- | Like 'V.zipWith', except pads the vectors to equal length
-- rather than discarding elements of the longer vector.
zipPadWith :: V.Unbox a => (Bool -> Bool -> a) -> BitVector -> BitVector -> V.Vector a
zipPadWith f xs ys = uncurry (V.zipWith f) (padMax xs ys)

-- | Discards any 'False' values at the most-significant end of the
-- given 'BitVector'.
trimLeading :: BitVector -> BitVector
trimLeading = V.reverse . V.dropWhile not . V.reverse

instance Num BitVector where
  fromInteger = unpackInteger
  as + bs = if cout then V.tail sums `V.snoc` True else V.tail sums
    where
      cout            = V.last carries
      (sums, carries) = V.unzip sumsAndCarries
      sumsAndCarries  = V.scanl' fullAdd (False, False) (zipPad as bs)
      fullAdd (_, cin) (a, b) = ((a /= b) /= cin, (a && b) || (cin && (a /= b)))
  as * bs = trimLeading (sum partials)
    where
      partials = zipWith shiftMult (V.toList as) [0 ..]
      shiftMult True i  = bs `shiftL` i
      shiftMult False _ = V.empty
  as - bs = trimLeading $ V.take (V.length as') (rawSum + 1)
    where
      rawSum     = as' + complement bs'
      (as', bs') = padMax as bs
  abs = id
  signum v | V.null v  = 0
           | otherwise = 1

instance Bits BitVector where
  (.&.)       = V.zipWith (&&)
  (.|.)       = zipPadWith (||)
  xor         = zipPadWith (/=)
  complement  = V.map not
  shiftL v i  = V.replicate i False V.++ v
  shiftR      = flip V.drop
  rotateR v i = high V.++ low
    where (low, high) = V.splitAt i v
  rotateL v i = high V.++ low
    where (low, high) = V.splitAt (V.length v - i) v
  bitSize     = V.length
  isSigned    = const False
  bit i       = V.replicate i False `V.snoc` True
  testBit v i = fromMaybe False (v V.!? i)
  popCount v  = V.length $ V.filter id v

-- | Converts an instance of 'Bits' to a 'BitVector'. 
--
-- /Note:/ this uses 'bitSize', and will not work for instances which
-- do not implement this method, notably 'Integer'. To unpack
-- 'Integer' values, use 'unpackInteger'.
unpack :: (Bits a) => a -> BitVector
unpack w = trimLeading $ V.generate (bitSize w) (testBit w)

-- | Converts a 'BitVector' to an instance of 'Bits'.
pack :: (Bits a) => BitVector -> a
pack = V.ifoldl' set zero
  where
    set w i True = w `setBit` i
    set w _ _    = w
    -- Currently the best way to represent an empty Bits instance (Ugh)
    zero = clearBit (bit 0) 0

unpackInteger :: Integer -> BitVector
unpackInteger = V.unfoldr f
  where
    f (flip divMod 2 -> (0, 0)) = Nothing
    f (flip divMod 2 -> (q, 0)) = Just (False, q)
    f (flip divMod 2 -> (q, 1)) = Just (True, q)
    f _                         = error "unexpected remainder when unpacking"

packInteger :: BitVector -> Integer
packInteger = pack

unpackInt :: Int -> BitVector
unpackInt = unpack 

packInt :: BitVector -> Int
packInt = pack 
