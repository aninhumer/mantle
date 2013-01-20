{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Mantle.Interface where

import Data.Bits

import Mantle.RTL
import Mantle.Circuit


data FaceK = Inner | Outer

type family   Flip (d :: FaceK) :: FaceK
type instance Flip Inner = Outer
type instance Flip Outer = Inner


type family FlipIfc x

class Interface ifc where
    newIfc :: (MonadCircuit c) => c (ifc, FlipIfc ifc)


type instance FlipIfc (ifc (d :: FaceK)) = ifc (Flip d)


data family   Signal a (d :: FaceK)
data instance Signal a Inner = Input { unInput :: Ref }
data instance Signal a Outer = Output { unOutput :: Expr }

type Input  a = Signal a Inner
type Output a = Signal a Outer


class IsDir d where
    toSignal :: Wire a -> Signal a d

instance IsDir Inner where
    toSignal (Wire w) = Input w

instance IsDir Outer where
    toSignal (Wire w) = Output (Var w)

type Direction d =
    (IsDir d, IsDir (Flip d), d ~ Flip (Flip d))


instance (Direction d, Bits a) => Interface (Signal a d) where
    newIfc = do
        w <- newWire
        return (toSignal w, toSignal w)


type Component c ifc = FlipIfc ifc -> c ()

make :: (Interface ifc, MonadCircuit c) =>
    Component c ifc -> c ifc
make compF = do
    (outer,inner) <- newIfc
    compF inner
    return outer


type instance FlipIfc () = ()

instance Interface () where
    newIfc = return ((),())


type instance FlipIfc (a,b) = (FlipIfc a, FlipIfc b)

instance (Interface a, Interface b) => Interface (a,b) where
    newIfc = do
        (ax,ay) <- newIfc
        (bx,by) <- newIfc
        return ((ax,bx),(ay,by))
