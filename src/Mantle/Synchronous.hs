{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mantle.Synchronous where

import Mantle.Prelude

import Control.Monad.Writer
import Control.Arrow (second)
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Bits
import Data.Vector.Bit

import Mantle.RTL
import Mantle.Circuit
import Mantle.Logic

newtype Clock = Clock (Wire Bool)

instance Readable Clock Bool where
    read (Clock w) = read w

newtype Reset = Reset (Wire Bool)

instance Readable Reset Bool where
    read (Reset w) = read w

type ClockReset = (Clock,Reset)


data Sync = Sync {
    syncInitials :: [(Ref,BitVector)],
    syncUpdates  :: [(Ref,Expr)]
}

instance Monoid Sync where
    mempty  = Sync [] []
    mappend (Sync xi xu) (Sync yi yu) = Sync (xi ++ yi) (xu ++ yu)

type Synchronous = WriterT Sync Circuit

onSync :: Clock -> Reset -> Trigger
onSync (Clock c) (Reset r) = Set.fromList [PosEdge c, NegEdge r]

syncBlock :: Clock -> Reset -> Sync -> Statement
syncBlock (Clock c) (Reset r) (Sync is us) = Cond noReset updates initial
  where
    noReset = Var r
    updates = S.fromList $ map (uncurry AsyncAssign) us
    initial = S.fromList $ map (uncurry AsyncAssign . second Lit) is

makeSync :: Clock -> Reset -> Synchronous a -> Circuit a
makeSync clk rst s = do
    (out,syn) <- runWriterT s
    addStmt (onSync clk rst) $ syncBlock clk rst syn
    return out

(<=:) :: Reg a -> Logic a -> Synchronous ()
(Reg r) <=: (Logic e) = do
    tell $ Sync [] [(r,e)]

reg :: Bits a => a -> Synchronous (Reg a)
reg x = do
    r @ (Reg n) <- newReg
    tell $ Sync [(n,unpack x)] []
    return r

