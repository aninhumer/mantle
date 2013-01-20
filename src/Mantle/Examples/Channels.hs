{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mantle.Examples.Channels where

import Data.Bits

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


(>>>) :: forall src snk a c.
    (Bits a, Source src a, Sink snk a, MonadCircuit c)
    => src -> snk -> c ()
src >>> snk = do
    let (Channel xv xr xe) = srcChan src :: (OutChan a)
    let (Channel yv yr ye) = snkChan snk :: (InChan a)
    yv =: xv
    yr =: xr
    xe =: ye
