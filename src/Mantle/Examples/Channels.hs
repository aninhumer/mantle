{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Mantle.Examples.Channels where

import Mantle.Prelude

import Data.Bits

import Mantle.Interface


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
