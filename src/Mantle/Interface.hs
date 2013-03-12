{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Mantle.Interface where

import Control.Lens
import Control.Monad.Writer

import Mantle.RTL
import Mantle.Bits
import Mantle.Circuit


data FaceK = Inner | Outer

type family   Flip (d :: FaceK) :: FaceK
type instance Flip Inner = Outer
type instance Flip Outer = Inner


data VoidIfc ifc = VoidIfc

infix 1 =:
class Interface ifc where
    type FlipIfc ifc
    newIfc :: MonadCircuit c => c (ifc, FlipIfc ifc)
    extIfc :: MonadCircuit c => c (ifc)
    (=:)   :: MonadCircuit c => FlipIfc ifc -> ifc -> c ()

data family   Signal a (d :: FaceK)
data instance Signal a Inner = Input { unInput :: Ref }
data instance Signal a Outer = Output { unOutput :: Expr }

type Input  a = Signal a Inner
type Output a = Signal a Outer


class IsDir d where
    toSignal :: Wire a -> Signal a d
    extSignal :: (Bits a, MonadCircuit c) => c (Signal a d)
    bindSignal :: MonadCircuit c => Signal a d -> Signal a (Flip d) -> c ()

extOutput :: forall a c. (Bits a, MonadCircuit c) => c (Input a)
extOutput = do
    (ExtOutput o :: ExtOutput a) <- newExtOutput
    return $ Input o

instance IsDir Inner where
    toSignal (Wire w) = Input w
    extSignal = extOutput
    bindSignal (Input x) (Output y) = circuit $ do
        tell $ (combs.at x ?~ y) mempty

extInput :: forall a c. (Bits a, MonadCircuit c) => c (Output a)
extInput = do
    (ExtInput i :: ExtInput a) <- newExtInput
    return $ Output (Var i)

instance IsDir Outer where
    toSignal (Wire w) = Output (Var w)
    extSignal = extInput
    bindSignal (Output y) (Input x) = circuit $ do
        tell $ (combs.at x ?~ y) mempty

type Direction d =
    (IsDir d, IsDir (Flip d), d ~ Flip (Flip d))


instance (Direction d, Bits a) => Interface (Signal a d) where
    type FlipIfc (Signal a d) = Signal a (Flip d)
    newIfc = do
        w <- newWire
        return (toSignal w, toSignal w)
    extIfc = extSignal
    (=:) = bindSignal


type Component c ifc = FlipIfc ifc -> c ()

make :: (Interface ifc, MonadCircuit c) =>
    Component c ifc -> c ifc
make compF = do
    (outer,inner) <- newIfc
    compF inner
    return outer

makeExtern :: forall ifc c. (Interface (FlipIfc ifc), MonadCircuit c) =>
    Component c ifc -> c (VoidIfc ifc)
makeExtern compF = do
    ext <- extIfc
    compF ext
    return VoidIfc


instance Interface () where
    type FlipIfc () = ()
    newIfc = return ((),())
    extIfc = return ()
    _ =: _ = return ()


instance (Interface a, Interface b) => Interface (a,b) where
    type FlipIfc (a,b) = (FlipIfc a, FlipIfc b)
    newIfc = do
        (ax,ay) <- newIfc
        (bx,by) <- newIfc
        return ((ax,bx),(ay,by))
    extIfc = do
        a <- extIfc
        b <- extIfc
        return (a,b)
    (x1,y1) =: (x2,y2) = do
        x1 =: x2
        y1 =: y2
