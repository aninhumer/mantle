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

namedRef :: MonadCircuit mc => String -> mc Ref
namedRef = return . Named

newVar :: forall v a c. (Bits a, MonadCircuit c) =>
    DType -> Maybe String -> (Ref -> v a) -> c (v a)
newVar tag name wrap = do
    ref <- case name of
        Just x  -> namedRef x
        Nothing -> newRef
    let newDecl = Declaration tag size ref
    circuit $ tell $ (decls .~ [newDecl]) mempty
    return $ wrap ref
    where size = repType (undefined :: a)

newtype ExtInput a = ExtInput Ref

newExtInput :: forall a c.
    (Bits a, MonadCircuit c) => String -> c (ExtInput a)
newExtInput n = newVar DInput (Just n) ExtInput

newtype ExtOutput a = ExtOutput Ref

newExtOutput :: forall a c.
    (Bits a, MonadCircuit c) => String -> c (ExtOutput a)
newExtOutput n = newVar DOutput (Just n) ExtOutput

newtype Wire a = Wire { wireVar :: Ref }

newWire :: forall a c. (Bits a, MonadCircuit c) => c (Wire a)
newWire = newVar DWire Nothing Wire

bindComb :: MonadCircuit c => Ref -> Expr -> c ()
bindComb x e = circuit $ do
    tell $ (combs.at x ?~ e) mempty

newtype Reg a = Reg { regVar :: LValue }

newReg :: forall a c. (MonadCircuit c, Bits a) => c (Reg a)
newReg = newVar DReg Nothing (Reg . NormalRef)

readReg :: Reg a -> Expr
readReg (Reg ir) = case ir of
    NormalRef  r   -> Var r
    IndexedRef r i -> Index r i

writeReg :: Reg a -> Expr -> Statement
writeReg (Reg r) e = do
    tell $ (writes.at r ?~ e) mempty

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
