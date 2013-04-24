{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mantle.Synchronous where

import Mantle.Prelude

import Control.Monad.Reader
import Data.Monoid

import Mantle.RTL
import Mantle.Bits
import Mantle.Circuit
import Mantle.Logic
import Mantle.Interface

newtype Clock = Clock { unClock :: ExtInput Bool }

newClock :: MonadCircuit c => c Clock
newClock = do
    c <- newExtInput "clk"
    return $ Clock c

newtype Reset = Reset { unReset :: ExtInput Bool }

newReset :: MonadCircuit c => c Reset
newReset = do
    r <- newExtInput "rst_n"
    return $ Reset r

type ClockReset = (Clock, Reset)

type Synchronous = ReaderT ClockReset Circuit

type SyncComp ifc = Component Synchronous ifc

instance MonadCircuit Synchronous where
    liftCircuit = lift

makeSync :: (Interface ifc, MonadCircuit c) =>
    ClockReset ->
    (FlipIfc ifc -> Synchronous ()) ->
    (FlipIfc ifc -> Circuit ())
makeSync cr syncF ifc = runReaderT (syncF ifc) cr

buildSync :: Synchronous a -> RTL
buildSync sync = buildCircuit $ do
    c <- newClock
    r <- newReset
    runReaderT sync (c,r)

syncTrigger :: ClockReset -> Trigger
syncTrigger (Clock c, Reset r) =
    posedge c <> negedge r

onClock :: Statement -> Synchronous ()
onClock stmt = do
    cr@(Clock (ExtInput c), (Reset (ExtInput r))) <- ask
    onTrigger (syncTrigger cr) $
        if2 (not (Output (Var r))) (return ()) $
        iff (Output (Var c)) stmt

onReset :: Statement -> Synchronous ()
onReset stmt = do
    cr@(_ ,(Reset (ExtInput r))) <- ask
    onTrigger (syncTrigger cr) $ iff (not (Output (Var r))) stmt

(=~) :: Reg a -> Output a -> Synchronous ()
w =~ e = onClock (w <=: e)

reg :: Bits a => a -> Synchronous (Reg a)
reg x = do
    r <- newReg
    onReset (r <=: literal x)
    return r

regU :: Bits a => Synchronous (Reg a)
regU = do
    r <- newReg
    onReset (r <=: undef)
    return r
