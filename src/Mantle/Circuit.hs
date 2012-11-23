{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Mantle.Circuit where

import Prelude hiding (length)

import Control.Monad.State
import Control.Lens
import Data.Bits
import qualified Data.Map as M
import qualified Data.Sequence as S

import Mantle.RTL

type Circuit = State RTL

type MonadCircuit = MonadState RTL

emptyRTL :: RTL
emptyRTL = RTL M.empty M.empty

namedVariable :: MonadCircuit c => Name -> Variable -> c ()
namedVariable n v = do
    vars.at n %= tryInsert
  where
    tryInsert Nothing  = Just v
    tryInsert (Just _) = error $
        "Variable name " ++ show n ++ " already in use."

freshVariable :: MonadCircuit c => Variable -> c (Name)
freshVariable v = do
    count <- uses vars M.size
    let freshName = Name $ "fresh" ++ show count
    namedVariable freshName v
    return freshName

newtype Wire a = Wire { wireVar :: Name }

namedWire :: forall a c. (MonadCircuit c, Bits a) => Name -> c (Wire a)
namedWire n = do
    namedVariable n $ Variable WireVar size
    return $ Wire n
    where size = bitSize (undefined :: a)

freshWire :: forall a c. (MonadCircuit c, Bits a) => c (Wire a)
freshWire = do
    n <- freshVariable $ Variable WireVar size
    return $ Wire n
    where size = bitSize (undefined :: a)

newtype Reg a = Reg { regVar :: Name }

namedReg :: forall a c. (MonadCircuit c, Bits a) => Name -> c (Reg a)
namedReg n = do
    namedVariable n $ Variable RegVar size
    return $ Reg n
    where size = bitSize (undefined :: a)

freshReg :: forall a c. (MonadCircuit c, Bits a) => c (Reg a)
freshReg = do
    n <- freshVariable $ Variable RegVar size
    return $ Reg n
    where size = bitSize (undefined :: a)

