{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mantle.Examples.Channels where

import Mantle.Prelude

import Mantle.Bits
import Mantle.Interface
import Mantle.Circuit
import Mantle.Logic


data Channel a d = Channel {
    value :: Signal a d,
    valid :: Signal Bool d,
    ready :: Signal Bool (Flip d)
}

type InChan  a = Channel a Inner
type OutChan a = Channel a Outer

instance (Direction d, Bits a) => Interface (Channel a d) where
    newIfc = do
        (vx,vy) <- newIfc
        (rx,ry) <- newIfc
        (ex,ey) <- newIfc
        return (Channel vx rx ex, Channel vy ry ey)
    extIfc = do
        v <- extIfc
        r <- extIfc
        e <- extIfc
        return $ Channel v r e
    (Channel vx rx ex) =: (Channel vy ry ey) = do
        vx =: vy
        rx =: ry
        ex =: ey

data PipeIfc a b d = Pipe {
    inchan  :: Channel a (Flip d),
    outchan :: Channel b d
}

type Pipe a b = PipeIfc a b Outer

instance (Direction d, Bits a, Bits b)
    => Interface (PipeIfc a b d) where
    newIfc = do
        (ix,iy) <- newIfc
        (ox,oy) <- newIfc
        return (Pipe ix ox, Pipe iy oy)
    extIfc = do
        i <- extIfc
        o <- extIfc
        return $ Pipe i o
    (Pipe ix ox) =: (Pipe iy oy) = do
        ix =: iy
        ox =: oy


class Source ifc a | ifc -> a where
    srcChan :: ifc -> OutChan a

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


chanMap :: (Output a -> Output b) -> OutChan a -> OutChan b
chanMap f (Channel v r e) = Channel (f v) r e

chanZip :: MonadCircuit mc =>
    (Output a -> Output b -> Output c) ->
    OutChan a -> OutChan b -> mc (OutChan c)
chanZip f (Channel xv xr xe) (Channel yv yr ye) = do
    (eo,ei) <- newIfc
    xe =: ei
    ye =: ei
    return $ Channel (f xv yv) (xr && yr) eo


(>>>) :: forall src snk a c.
    (Bits a, Source src a, Sink snk a, MonadCircuit c)
    => src -> snk -> c ()
src >>> snk = do
    let (Channel xv xr xe) = srcChan src :: OutChan a
    let (Channel yv yr ye) = snkChan snk :: InChan a
    yv =: xv
    yr =: xr
    xe =: ye

(<<<) :: forall snk src a c.
    (Bits a, Sink snk a, Source src a, MonadCircuit c)
    => snk -> src -> c ()
(<<<) = flip (>>>)


(>+<), (>-<), (>*<) ::
    (Num (Output a), MonadCircuit c) =>
    OutChan a -> OutChan a -> c (OutChan a)
(>+<) = chanZip (+)
(>-<) = chanZip (-)
(>*<) = chanZip (*)
