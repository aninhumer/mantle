{-# LANGUAGE RebindableSyntax #-}

module Mantle.Examples.FIFO where

import Mantle.Prelude

import Control.Lens
import Data.Bits

import Mantle.Logic
import Mantle.Synchronous
import Mantle.Examples.Channels

fifo :: (Integral a, Bits a) => SyncComp (Pipe a a)
fifo (Pipe inchan outchan) = do

    -- Need to implement regU to avoid Integral constraint
    val  <- reg 0
    full <- reg $ Constant False

    ready (ohand outchan) =: full
    ovalue outchan =: val
    ready (ihand inchan) =: not full

    onSync $ do
        iff (not full && enable (ihand inchan)) $ do
            val  <=: ivalue inchan
            full <=: true
        iff (full && enable (ohand outchan)) $ do
            full <=: false
