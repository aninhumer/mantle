{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mantle.Examples.FIFO where

import Mantle.Prelude

import Data.Bits

import Mantle.Logic
import Mantle.Synchronous
import Mantle.Interface

import Mantle.Examples.Channels

type FIFO a = Pipe a a

fifo :: (Integral a, Bits a) => SyncComp (FIFO a)
fifo (Pipe inchan outchan) = do

    -- Need to implement regU to avoid Integral constraint
    val  <- reg 0
    full <- reg $ Constant False

    valid outchan =: full
    value outchan =: val
    ready inchan  =: not full

    onSync $ do
        iff (not full && valid inchan) $ do
            val  <=: value inchan
            full <=: true
        iff (full && ready outchan) $ do
            full <=: false

fifoChain :: forall a. (Integral a, Bits a) => SyncComp (FIFO a)
fifoChain (Pipe inchan outchan) = do
    fifoA :: FIFO a <- make fifo
    fifoB :: FIFO a <- make fifo
    inchan >>> fifoA
    fifoA >>> fifoB
    fifoB >>> outchan
