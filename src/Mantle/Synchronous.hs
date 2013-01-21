{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mantle.Synchronous where

import Mantle.Prelude

import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Bits
import Data.Monoid

import Mantle.RTL
import Mantle.Circuit
import Mantle.Logic
import Mantle.Interface

type ClockReset = (Ref, Ref)

type Synchronous = ReaderT ClockReset Circuit

type SyncComp ifc = FlipIfc ifc -> Synchronous ()

instance MonadCircuit Synchronous where
    liftCircuit = lift

makeSync :: (Interface ifc, MonadCircuit c) =>
    ClockReset ->
    (FlipIfc ifc -> Synchronous ()) ->
    (FlipIfc ifc -> Circuit ())
makeSync cr syncF ifc = runReaderT (syncF ifc) cr

syncTrigger :: ClockReset -> Trigger
syncTrigger (c, r) =
    posedge (Wire c) <> negedge (Wire r)

onSync :: Statement -> Synchronous ()
onSync stmt = do
    cr@(clk,rst) <- ask
    onTrigger (syncTrigger cr) $ iff (Output (Var clk)) stmt

onReset :: Statement -> Synchronous ()
onReset stmt = do
    cr@(clk,rst) <- ask
    onTrigger (syncTrigger cr) $ iff (not (Output (Var rst))) stmt

(=~) :: Reg a -> Output a -> Synchronous ()
w =~ e = onSync (w <=: e)

reg :: Bits a => a -> Synchronous (Reg a)
reg x = do
    r <- newReg
    onReset (r <=: literal x)
    return r

