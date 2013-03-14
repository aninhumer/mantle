{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mantle.Examples.FIFO where

import Mantle.Prelude


import Mantle.Bits
import Mantle.Logic
import Mantle.Synchronous
import Mantle.Interface

import Mantle.Examples.Channels

type FIFO a = Pipe a a

fifo :: Bits a => SyncComp (FIFO a)
fifo (Pipe inchan outchan) = do

    val  <- regU
    full <- reg False

    valid outchan =: rd full
    value outchan =: rd val
    enable inchan  =: not (rd full)

    onClock $ do
        iff (not (rd full) && valid inchan) $ do
            val  <=: value inchan
            full <=: true
        iff (rd full && enable outchan) $ do
            full <=: false


fifoChain :: forall a. (Integral a, Bits a) => SyncComp (FIFO a)
fifoChain (Pipe inchan outchan) = do
    fifoA :: FIFO a <- make fifo
    fifoB :: FIFO a <- make fifo
    inchan >>> fifoA
    fifoA >>> fifoB
    fifoB >>> outchan
