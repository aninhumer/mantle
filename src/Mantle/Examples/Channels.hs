{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Mantle.Examples.Channels where

import Mantle.Prelude

import Data.Monoid
import Control.Applicative

import Mantle.Bits
import Mantle.Interface
import Mantle.Circuit
import Mantle.Logic



data Channel d ifc = Channel {
    channel :: ifc,
    valid   :: Signal Bool d,
    ready   :: Signal Bool (Flip d)
}

instance (Direction d, Interface ifc)
    => Interface (Channel d ifc) where

    type FlipIfc (Channel d ifc) =
        Channel (Flip d) (FlipIfc ifc)
    newIfc = do
        (cx,cy) <- newIfc
        (vx,vy) <- newIfc
        (rx,ry) <- newIfc
        return (Channel cx vx rx, Channel cy vy ry)
    extIfc = do
        c <- extIfc
        v <- extIfc
        r <- extIfc
        return $ Channel c v r
    (Channel cx vx rx) =: (Channel cy vy ry) = do
        cx =: cy
        vx =: vy
        ry =: rx

type OutChan a = Channel Outer (Output a)
type InChan  a = Channel Inner (Input  a)

value :: Channel d (Signal a d) -> Signal a d
value = channel

type a :=>: b = OutChan a -> OutChan b
type a :=>  b = OutChan a -> b
infixr 8 :=>

instance Functor (Channel Outer) where
    fmap f (Channel x v r) = Channel (f x) v r

instance Applicative (Channel Outer) where
    pure x = (Channel x true terminal)
    (Channel f fv fr) <*> (Channel x xv xr) =
        Channel (f x) (fv && xv) $
            inputMap (&& xv) fr <> inputMap (&& fv) xr


chanGuard :: Output Bool -> (a :=>: a)
chanGuard cond (Channel x r e) =
    Channel x (r && cond) $ inputMap (&& cond) e


data ServerIfc d a b = Server {
    inchan  :: Channel (Flip d) a,
    outchan :: Channel d b
}

instance (Direction d, Interface a, Interface b)
    => Interface (ServerIfc d a b) where
    type FlipIfc (ServerIfc d a b) =
        ServerIfc (Flip d) (FlipIfc a) (FlipIfc b)
    newIfc = do
        (ix,iy) <- newIfc
        (ox,oy) <- newIfc
        return (Server ix ox, Server iy oy)
    extIfc = do
        i <- extIfc
        o <- extIfc
        return $ Server i o
    (Server ix ox) =: (Server iy oy) = do
        ix =: iy
        ox =: oy

type Server = ServerIfc Outer
type Pipe a b = Server (Input a) (Output b)


class Source src a | src -> a where
    srcChan :: src -> OutChan a

instance Source (OutChan a) a where
    srcChan = id

instance Source (Pipe a b) b where
    srcChan = outchan


class Sink ifc a | ifc -> a where
    snkChan :: ifc -> InChan a

instance Sink (InChan a) a where
    snkChan = id

instance Sink (Pipe a b) a where
    snkChan = inchan


(>->) :: forall src snk a c.
    (Bits a, Source src a, Sink snk a, MonadCircuit c)
    => src -> snk -> c ()
src >-> snk = do
    let (Channel xv xr xe) = srcChan src :: OutChan a
    let (Channel yv yr ye) = snkChan snk :: InChan a
    yv =: xv
    yr =: xr
    xe =: ye

(<-<) :: forall snk src a c.
    (Bits a, Sink snk a, Source src a, MonadCircuit c)
    => snk -> src -> c ()
(<-<) = flip (>->)


