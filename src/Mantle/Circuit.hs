{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mantle.Circuit where

import Control.Monad.State
import Control.Monad.Writer
import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as Set

import Mantle.RTL
import Mantle.Bits


newtype Circuit a = Circuit
    { unCircuit :: StateT Int (Writer RTL) a }
    deriving (Monad)

class Monad m => MonadCircuit m where
    liftCircuit :: Circuit a -> m a

instance MonadCircuit Circuit where
    liftCircuit = id

circuit :: MonadCircuit c => StateT Int (Writer RTL) a -> c a
circuit = liftCircuit . Circuit

buildCircuit :: Circuit a -> RTL
buildCircuit (Circuit c) = execWriter $ evalStateT c 0

newRef :: MonadCircuit c => c Ref
newRef = circuit $ do
    ref <- get
    put $ ref + 1
    return $ Ref ref

newVar :: forall v a c. (Bits a, MonadCircuit c) =>
    Simple Lens RTL (M.Map Ref VType) -> (Ref -> v a) -> c (v a)
newVar lens wrap = do
    ref <- newRef
    circuit $ do
        tell $ (lens.at ref ?~ size) mempty
        return $ wrap ref
    where size = repType (undefined :: a)

newtype ExtInput a = ExtInput Ref

newExtInput :: forall a c.
    (Bits a, MonadCircuit c) => c (ExtInput a)
newExtInput = newVar inputs ExtInput

newtype ExtOutput a = ExtOutput Ref

newExtOutput :: forall a c.
    (Bits a, MonadCircuit c) => c (ExtOutput a)
newExtOutput = newVar outputs ExtOutput

newtype Wire a = Wire { wireVar :: Ref }

newWire :: forall a c. (Bits a, MonadCircuit c) => c (Wire a)
newWire = newVar wires Wire

newtype Reg a = Reg { regVar :: RegRef }

newReg :: forall a c. (MonadCircuit c, Bits a) => c (Reg a)
newReg = newVar regs (Reg . NRef)

type Statement = StmtM ()

type StmtM = Writer Block

onTrigger :: MonadCircuit c => Trigger -> Statement -> c ()
onTrigger trig stmt = do
    let (_,newBlock) = runWriter stmt
    circuit $ tell $ (blocks.at trig ?~ newBlock) mempty

posedge :: ExtInput a -> Trigger
posedge (ExtInput x) = Set.singleton $ PosEdge x

negedge :: ExtInput a -> Trigger
negedge (ExtInput x) = Set.singleton $ NegEdge x
