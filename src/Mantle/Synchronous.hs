{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mantle.Synchronous where

import Mantle.Prelude

import Control.Monad.Writer
import Control.Arrow (second)
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Bits
import Data.Vector.Bit
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

type SyncComp ifc a = Inner ifc -> Synchronous a

instance MonadCircuit Synchronous where
    liftCircuit = lift

makeSync :: Interface ifc =>
    ClockReset -> SyncComp ifc a -> Circuit (Outer ifc)
makeSync cr syncF = (`runReaderT` cr) $ do
    ifc <- newIfc
    syncF ifc
    return $ expose ifc

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

(<=:) :: Reg a -> Logic a -> Synchronous ()
(Reg r) <=: (Logic e) = do
    tell $ Sync [] [(r,e)]

reg :: Bits a => a -> Synchronous (Reg a)
reg x = do
    r @ (Reg n) <- newReg
    tell $ Sync [(n,unpack x)] []
    return r

