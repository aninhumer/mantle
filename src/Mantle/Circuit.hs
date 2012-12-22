{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mantle.Circuit where

import Prelude hiding (length, mapM_)

import Control.Monad.State hiding (mapM_)
import Control.Monad.Writer
import Control.Lens
import Data.Foldable
import Data.Bits
import qualified Data.Map as M
import qualified Data.Sequence as S

import Mantle.RTL

newtype Circuit a = Circuit
    { unCircuit :: StateT Int (Writer RTL) a }
    deriving (Monad)

class Monad m => MonadCircuit m where
    liftCircuit :: Circuit a -> m a

instance MonadCircuit Circuit where
    liftCircuit = id

circuit :: MonadCircuit c => StateT Int (Writer RTL) a -> c a
circuit = liftCircuit . Circuit

makeCircuit :: Circuit a -> RTL
makeCircuit (Circuit c) = execWriter $ evalStateT c 0

newRef :: MonadCircuit c => c (Ref)
newRef = circuit $ do
    ref <- get
    put $ ref + 1
    return $ Ref ref

newtype Wire a = Wire { wireVar :: Ref }

newWire :: MonadCircuit c => c (Wire a)
newWire = do
    ref <- newRef
    return $ Wire ref

newtype Reg a = Reg { regVar :: Ref }

newReg :: forall a c. (MonadCircuit c, Bits a) => c (Reg a)
newReg = do
    ref <- newRef
    circuit $ do
        tell $ (regs.at ref ?~ size) mempty
        return $ Reg ref
    where size = bitSize (undefined :: a)

addStmt :: MonadCircuit c => Trigger -> Statement -> c ()
addStmt t s = blocks %= M.insertWith (S.><) t (S.singleton s)

addStmts :: MonadCircuit c => Trigger -> S.Seq Statement -> c ()
addStmts t ss = mapM_ (addStmt t) ss

