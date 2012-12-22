{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}

import Mantle.Prelude

import Mantle.Logic
import Mantle.Interface
import Mantle.Synchronous

counter :: SyncComp (Output Int) ()
counter out = do
    val <- reg 0
    onSync $ do
        val <=: (val + 1)
    out =: val

