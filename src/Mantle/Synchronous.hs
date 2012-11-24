{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}

module Mantle.Synchronous where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Data.Bits
import Data.Vector.Bit

import Mantle.Logic
import Mantle.RTL
import Mantle.Circuit

newtype Clock = Clock { clockVar :: Name }

newtype Reset = Reset { resetVar :: Name }

data Sync = Sync {
    syncInitials :: [(Name,BitVector)],
    syncUpdates  :: [(Name,Expr)]
}

instance Monoid Sync where
    mempty  = Sync [] []
    mappend (Sync xi xu) (Sync yi yu) = Sync (xi ++ yi) (xu ++ yu)

type Synchronous = WriterT Sync (State RTL)

(<=:) :: Reg a -> Logic a -> Synchronous ()
(Reg r) <=: (Logic e) = do
    tell $ Sync [] [(r,e)]

reg :: Bits a => a -> Synchronous (Reg a)
reg x = do
    r @ (Reg n) <- freshReg
    tell $ Sync [(n,unpack x)] []
    return r

