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

newtype Clock = Clock (Wire Bool)

instance Readable Clock Bool where
    read (Clock w) = read w

newtype Reset = Reset (Wire Bool)

instance Readable Reset Bool where
    read (Reset w) = read w

type ClockReset = (Clock,Reset)

type Synchronous = ReaderT ClockReset Circuit

type SyncComp ifc = FlipIfc ifc -> Synchronous ()

instance MonadCircuit Synchronous where
    liftCircuit = lift

makeSync :: (Interface ifc, MonadCircuit c) =>
    ClockReset ->
    (FlipIfc ifc -> Synchronous ()) ->
    (FlipIfc ifc -> Circuit ())
makeSync cr syncF ifc = runReaderT (syncF ifc) cr

syncTrigger :: Clock -> Reset -> Trigger
syncTrigger (Clock c) (Reset r) =
    posedge c <> negedge r

onSync :: Statement -> Synchronous ()
onSync stmt = do
    (clk,rst) <- ask
    onTrigger (syncTrigger clk rst) $ iff clk stmt

onReset :: Statement -> Synchronous ()
onReset stmt = do
    (clk,rst) <- ask
    onTrigger (syncTrigger clk rst) $ iff (not rst) stmt

(=~) :: Readable r a => Reg a -> r -> Synchronous ()
w =~ e = onSync (w <=: e)

reg :: Bits a => Constant a -> Synchronous (Reg a)
reg x = do
    r <- newReg
    onReset (r <=: x)
    return r

