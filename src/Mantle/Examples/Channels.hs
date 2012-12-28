{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Mantle.Examples.Channels where

import Data.Bits
import Mantle.Interface
import Mantle.Interface.TH

data Handshake

instance Interface Handshake where
    data Ifc d Handshake = Handshake {
        enable :: Ifc d (Input Bool),
        ready  :: Ifc d (Output Bool)
    }
    newIfcCircuit = do
        e <- newIfc
        r <- newIfc
        return $ Handshake e r
    expose (Handshake e r) =
        Handshake (expose e) (expose r)

data InChan a

instance Bits a => Interface (InChan a) where
    data Ifc d (InChan a) = InChan {
        ivalue :: Ifc d (Input a),
        ihand  :: Ifc d (Handshake)
    }
    newIfcCircuit = do
        v <- newIfc
        h <- newIfc
        return $ InChan v h
    expose (InChan v h) =
        InChan (expose v) (expose h)

data OutChan a

instance Bits a => Interface (OutChan a) where
    data Ifc d (OutChan a) = OutChan {
        ovalue :: Ifc d (Output a),
        ohand  :: Ifc d (Handshake)
    }
    newIfcCircuit = do
        v <- newIfc
        h <- newIfc
        return $ OutChan v h
    expose (OutChan v h) =
        OutChan (expose v) (expose h)

data Pipe a b

instance (Bits a, Bits b) => Interface (Pipe a b) where
    data Ifc d (Pipe a b) = Pipe {
        pipeIn :: Ifc d (InChan a),
        pipeOut:: Ifc d (OutChan a)
    }
    newIfcCircuit = do
        pi <- newIfc
        po <- newIfc
        return $ Pipe pi po
    expose (Pipe pi po) =
        Pipe (expose pi) (expose po)
