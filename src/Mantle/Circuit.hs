{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Mantle.Circuit where

import Prelude hiding (length, mapM_)

import Control.Monad.State hiding (mapM_)
import Control.Lens
import Data.Foldable
import Data.Bits
import qualified Data.Map as M
import qualified Data.Sequence as S

import Mantle.RTL

type Circuit = State RTL

type MonadCircuit = MonadState RTL

emptyRTL :: RTL
emptyRTL = RTL M.empty M.empty

makeCircuit :: Circuit a -> RTL
makeCircuit c = execState c emptyRTL

newVariable :: MonadCircuit c => Variable -> c (Ref)
newVariable v = do
    count <- uses vars M.size
    let newRef = Ref count
    vars.at newRef ?= v
    return newRef

newtype Wire a = Wire { wireVar :: Ref }

newWire :: forall a c. (MonadCircuit c, Bits a) => c (Wire a)
newWire = do
    n <- newVariable $ Variable WireVar size
    return $ Wire n
    where size = bitSize (undefined :: a)

newtype Reg a = Reg { regVar :: Ref }

newReg :: forall a c. (MonadCircuit c, Bits a) => c (Reg a)
newReg = do
    n <- newVariable $ Variable RegVar size
    return $ Reg n
    where size = bitSize (undefined :: a)

addStmt :: MonadCircuit c => Trigger -> Statement -> c ()
addStmt t s = blocks %= M.insertWith (S.><) t (S.singleton s)

addStmts :: MonadCircuit c => Trigger -> S.Seq Statement -> c ()
addStmts t ss = mapM_ (addStmt t) ss

