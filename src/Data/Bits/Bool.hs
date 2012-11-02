module Data.Bits.Bool where

import Data.Bits

instance Bits Bool where
    bitSize _ = 1
    isSigned _ = False
    testBit x 0 = x
    testBit x _ = False
    bit 0 = True
    bit _ = False
    complement = not
    (.&.) = (&&)
    (.|.) = (||)
    xor   = (/=)
    shiftL _ _ = False
    shiftR _ _ = False
    rotate x _ = x
    popCount True  = 1
    popCount False = 0
