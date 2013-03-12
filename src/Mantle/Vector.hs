{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module Mantle.Vector where

import qualified Data.Vector.Fixed as V
import Data.Vector.Fixed.Boxed (Vec)

import Mantle.Bits
import Mantle.RTL
import Mantle.Circuit
import Mantle.Interface

type VectorIfc n a d = Vec n (Signal a d)

instance (V.Arity n, Direction d, Bits a) =>
    Interface (VectorIfc n a d) where
    type FlipIfc (VectorIfc n a d) = VectorIfc n a (Flip d)
    newIfc = do
        pairs <- V.replicateM newIfc
        return (V.map fst pairs, V.map snd pairs)
    extIfc = do
        ext <- V.replicateM extIfc
        return ext
    xs =: ys = do
        V.zipWithM (=:) xs ys
        return ()

type Vector n a = VectorIfc n a Outer


type Memory n a = Reg (Vec n a)

index :: Bits i => Output i -> Memory n a -> Reg a
index (Output e) (Reg (NRef m)) = Reg $ IRef m e
