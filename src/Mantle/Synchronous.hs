{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Mantle.Synchronous where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Sequence.Lens
import Data.Bits
import Data.Vector.Bit

import Mantle.Logic
import Mantle.RTL
import Mantle.Circuit

type Synchronous sc = (MonadReader SyncRef sc, Circuit sc)

syncBlock :: (Synchronous sc, Functor f) => sc (LensLike f RTL RTL Sync Sync)
syncBlock = do
    ref :: Int <- ask
    return $ syncs . ordinal ref

(=:) :: Synchronous sc => Reg a -> Logic a -> sc ()
(Reg r) =: (Logic e) = do
    blk <- syncBlock 
    blk.clockUpdates %= M.insert r e
    return ()

reg :: (Bits a, Synchronous c) => a -> c (Reg a)
reg x = do
    val @ (Reg r) <- register
    blk <- syncBlock
    blk.resetUpdates %= M.insert r (Lit $ unpack x)
    return val

