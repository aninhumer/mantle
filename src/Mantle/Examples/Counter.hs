{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Bits
import Mantle.Logic
import Mantle.Circuit
import Mantle.Synchronous

counter :: forall a sc. (Integral a, Bits a, Synchronous sc) => sc (Logic a)
counter = do
    val <- reg (0 :: a)
    val =: (rd val + 1)
    return $ rd val

