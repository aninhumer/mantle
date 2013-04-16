{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Mantle.Vector where

import qualified Data.Vector.Fixed as V
import qualified Data.Vector.Fixed.Boxed as VB

import Mantle.Bits
import Mantle.RTL
import Mantle.Circuit
import Mantle.Logic
import Mantle.Interface

type Vec = VB.Vec

instance (V.Arity n, Interface a) =>
    Interface (Vec n a) where
    type FlipIfc (Vec n a) = Vec n (FlipIfc a)
    newIfc = do
        pairs <- V.replicateM newIfc
        return (V.map fst pairs, V.map snd pairs)
    xs =: ys = do
        V.zipWithM (=:) xs ys
        return ()

extInputVec :: forall mc n a . (MonadCircuit mc, V.Arity n, Bits a)
    => String -> mc (Vec n (Output a))
extInputVec name = do
    (ExtInput x :: ExtInput a) <- newExtInput name
    return $ V.generate $ pos x
  where
    pos x i = index (literal i) (Output (Var x))

extOutputVec :: forall mc n a. (MonadCircuit mc, V.Arity n, Bits a)
    => String -> mc (Vec n (Input a))
extOutputVec name = do
    (ExtOutput x :: ExtOutput a) <- newExtOutput name
    return $ V.generate $ pos x
  where
    pos x i = indexedInput x (Lit $ Dec $ toInteger i)

-- TODO: Better constraints on i vs n
index :: Bits i => Output i -> Output (Vec n a) -> Output a
index (Output e) (Output (Var x)) =
    Output $ Index x e

(!) :: Bits i => Output (Vec n a) -> Output i -> Output a
(!) = flip index


type Memory n a = Reg (Vec n a)

indexMem :: Bits i => Output i -> Memory n a -> Reg a
indexMem (Output e) (Reg (NormalRef m)) = Reg $ IndexedRef m e
