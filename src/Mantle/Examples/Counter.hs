{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}

module Mantle.Examples.Counter where

import Mantle.Prelude

import Mantle.Logic
import Mantle.Interface
import Mantle.Synchronous
import Mantle.Verilog

counter :: SyncComp (Output Int)
counter out = do
    val <- reg 0
    onSync $ do
        val <=: rd val + 1
    out =: rd val

