{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Mantle.Examples.Channels where

import Mantle.Prelude

import Data.Bits

import Mantle.Interface
import Mantle.Circuit
import Mantle.Logic


data InChan a

instance Bits a => Interface (InChan a) where
    data Ifc d (InChan a) = InChan {
        ivalue  :: Ifc d (Input a),
        iready  :: Ifc d (Output Bool),
        ienable :: Ifc d (Input Bool)
    }
    newIfcCircuit = do
        v <- newIfc
        r <- newIfc
        e <- newIfc
        return $ InChan v r e
    expose (InChan v r e) =
        InChan (expose v) (expose r) (expose e)


data OutChan a

instance Bits a => Interface (OutChan a) where
    data Ifc d (OutChan a) = OutChan {
        ovalue  :: Ifc d (Output a),
        oready  :: Ifc d (Output Bool),
        oenable :: Ifc d (Input Bool)
    }
    newIfcCircuit = do
        v <- newIfc
        r <- newIfc
        e <- newIfc
        return $ OutChan v r e
    expose (OutChan v r e) =
        OutChan (expose v) (expose r) (expose e)


data Pipe a b

instance (Bits a, Bits b) => Interface (Pipe a b) where
    data Ifc d (Pipe a b) = Pipe {
        pipeIn  :: Ifc d (InChan a),
        pipeOut :: Ifc d (OutChan b)
    }
    newIfcCircuit = do
        pi <- newIfc
        po <- newIfc
        return $ Pipe pi po
    expose (Pipe pi po) =
        Pipe (expose pi) (expose po)

type Stream a = Outer (OutChan a)

class Source s a | s -> a where
    srcChan :: s -> Stream a

instance Source (Stream a) a where
    srcChan = id

instance Source (Outer (Pipe a b)) b where
    srcChan (Pipe _ o) = o

instance Source (Inner (InChan a)) a where
    srcChan (InChan (InputWire v) (OutputWire r) (InputWire e)) =
        OutChan (OutputWire v) (OutputWire e) (InputWire r)


class Sink s a | s -> a where
    snkChan :: s -> Outer (InChan a)

instance Sink (Outer (InChan a)) a where
    snkChan = id

instance Sink (Outer (Pipe a b)) a where
    snkChan (Pipe i _) = i

instance Sink (Inner (OutChan a)) a where
    snkChan (OutChan (OutputWire v) (OutputWire r) (InputWire e)) =
        InChan (InputWire v) (OutputWire e) (InputWire r)


(>>>) :: (Bits a, MonadCircuit c, Source x a, Sink y a) =>
    x -> y -> c ()
x >>> y = do
    let (OutChan xv xr xe) = srcChan x
    let (InChan  yv yr ye) = snkChan y
    yv =: xv
    ye =: xr
    xe =: yr

(<<<) :: (Bits a, MonadCircuit c, Source x a, Sink y a) =>
    y -> x -> c ()
(<<<) = flip (>>>)


-- Left biased stream merge
(>><) :: (Bits a, MonadCircuit c, Source x a, Source y a) =>
    x -> y -> c (Stream a)
x >>< y = do
    let (OutChan xv xr xe) = srcChan x
    let (OutChan yv yr ye) = srcChan y
    out@(OutChan zv zr ze) <- newIfc
    zr =: xr || yr
    xe =: ze && xr
    ye =: ze && not xr
    zv =: if xr then xv else yv
    return $ expose out

-- Right biased stream merge
(><<) :: (Bits a, MonadCircuit c, Source x a, Source y a) =>
    x -> y -> c (Stream a)
(><<) = flip (>><)
