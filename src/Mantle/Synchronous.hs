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
    _initial :: M.Map Name BitVector,
    _updates :: M.Map Name Expr
}

instance Monoid Sync where
    mempty  = Sync M.empty M.empty
    mappend (Sync xi xu) (Sync yi yu) =
        Sync (xi `M.union` yi) (xu `M.union` yu)

type Synchronous = WriterT Sync (State RTL)

(<=:) :: Reg a -> Logic a -> Synchronous ()
(Reg r) <=: (Logic e) = do
    tell $ Sync M.empty (M.singleton r e)

reg :: Bits a => a -> Synchronous (Reg a)
reg x = do
    r @ (Reg n) <- freshReg
    tell $ Sync (M.singleton n $ unpack x) M.empty
    return r

