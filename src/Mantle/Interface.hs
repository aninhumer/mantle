{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Mantle.Interface where

import Control.Monad.Writer
import qualified Data.Map as M

import Mantle.RTL
import Mantle.Bits
import Mantle.Circuit


data Inner
data Outer

type family   Flip d
type instance Flip Inner = Outer
type instance Flip Outer = Inner


data VoidIfc ifc = VoidIfc

infix 1 =:
class Interface ifc where
    type FlipIfc ifc
    newIfc :: MonadCircuit c => c (ifc, FlipIfc ifc)
    (=:)   :: MonadCircuit c => FlipIfc ifc -> ifc -> c ()

data family   Signal a d
data instance Signal a Inner =
    Input  { unInput :: M.Map LValue (Output a -> Expr) }
data instance Signal a Outer =
    Output { unOutput :: Expr }

type Input  a = Signal a Inner
type Output a = Signal a Outer

outputWire :: forall a. Bits a => Output a -> Circuit (Wire a)
outputWire x = do
    (Wire w :: Wire a) <- newWire
    (refInput w) =: x
    return $ Wire w

combOutput :: Bits a => Output a -> Circuit (Output a)
combOutput x = do
    (Wire w) <- outputWire x
    return $ Output (Var w)

refInput :: Ref -> Input a
refInput x = Input $ M.singleton (NormalRef x) unOutput

indexedInput :: Ref -> Expr -> Input a
indexedInput x e =
    Input $ M.singleton (IndexedRef x e) unOutput

assocsInput :: [(LValue, Output a -> Expr)] -> Input a
assocsInput xs =
    Input $ M.fromListWith (error "Conflicting Inputs") xs

bind :: Bits a => Input a -> Output a -> Circuit ()
bind (Input i) o = do
    signal <- if M.size i <= 1
        then return o
        else combOutput o
    let bound = M.mapWithKey bindComb $ M.map ($ signal) i
    sequence_ $ M.elems $ bound

instance Monoid (Input a) where
    mempty  = Input $ mempty
    mappend (Input x) (Input y) =
        Input $ M.unionWith (error "Conflicting Inputs") x y


class IsDir d where
    toSignal :: Wire a -> Signal a d
    bindSignal :: (Bits a, MonadCircuit c) => Signal a d -> Signal a (Flip d) -> c ()

extOutput :: forall a c. (Bits a, MonadCircuit c)
    => String -> c (Input a)
extOutput n = do
    (ExtOutput o :: ExtOutput a) <- newExtOutput n
    return $ refInput o

instance IsDir Inner where
    toSignal (Wire w) = refInput w
    bindSignal x y = liftCircuit $ bind x y

extInput :: forall a c. (Bits a, MonadCircuit c)
    => String -> c (Output a)
extInput n = do
    (ExtInput i :: ExtInput a) <- newExtInput n
    return $ Output (Var i)

instance IsDir Outer where
    toSignal (Wire w) = Output (Var w)
    bindSignal y x = liftCircuit $ bind x y

type Direction d =
    (IsDir d, IsDir (Flip d), d ~ Flip (Flip d))


instance (Direction d, Bits a) => Interface (Signal a d) where
    type FlipIfc (Signal a d) = Signal a (Flip d)
    newIfc = do
        w <- newWire
        return (toSignal w, toSignal w)
    (=:) = bindSignal


type Component c ifc = FlipIfc ifc -> c (VoidIfc ifc)

component :: MonadCircuit mc
    => mc () -> mc (VoidIfc ifc)
component c = c >> return VoidIfc

make :: (Interface ifc, MonadCircuit c) =>
    Component c ifc -> c ifc
make compF = do
    (outer,inner) <- newIfc
    compF inner
    return outer


instance Interface () where
    type FlipIfc () = ()
    newIfc = return ((),())
    _ =: _ = return ()


instance (Interface a, Interface b) => Interface (a,b) where
    type FlipIfc (a,b) = (FlipIfc a, FlipIfc b)
    newIfc = do
        (ax,ay) <- newIfc
        (bx,by) <- newIfc
        return ((ax,bx),(ay,by))
    (x1,y1) =: (x2,y2) = do
        x1 =: x2
        y1 =: y2
