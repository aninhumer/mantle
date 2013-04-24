{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}

module Mantle.Examples.Counter where

import Mantle.Prelude

import Mantle.Logic
import Mantle.Interface
import Mantle.Synchronous
import Mantle.Verilog

counter :: SyncComp (Output Int)
counter out = component $ do
    count <- reg 0
    onClock $ do
        count <=: rd count + 1
    out =: rd count

extCounter :: Synchronous ()
extCounter = do
    out <- make counter
    ext <- extOutput "count"
    ext =: out

counterCode = genModule "IntCounter" $ buildSync extCounter
