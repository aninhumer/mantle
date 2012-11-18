{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Mantle.Synchronous where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Sequence.Lens
import Data.Bits
import Data.Vector.Bit

import Mantle.Logic
import Mantle.RTL
import Mantle.Circuit

type Synchronous = ReaderT SyncRef (State RTL)

type MonadSync s = (MonadCircuit s, MonadReader SyncRef s)

syncBlock :: (MonadSync sc, Functor f) => sc (LensLike f RTL RTL Sync Sync)
syncBlock = do
    ref <- ask
    return $ syncs . ordinal ref

(=:) :: MonadSync sc => Reg a -> Logic a -> sc ()
(Reg r) =: (Logic e) = do
    blk <- syncBlock 
    blk.clockUpdates.at r ?= e
    return ()

reg :: (Bits a, MonadSync c) => a -> c (Reg a)
reg x = do
    val @ (Reg r) <- register
    blk <- syncBlock
    blk.resetUpdates.at r ?= (Lit $ unpack x)
    return val

