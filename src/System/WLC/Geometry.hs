{-# LANGUAGE MultiParamTypeClasses #-}
module System.WLC.Geometry where

import Foreign.Ptr(Ptr)
import Data.Word(Word32)
import Data.Convertible.Base
import Data.Convertible.Instances.C

import System.WLC.Utilities
import Bindings.WLC.Geometry


data Point = Point { x :: Int, y :: Int }
    deriving (Eq, Show, Ord)

instance Primitive C'wlc_point Point where
    fromPrimitive C'wlc_point { c'wlc_point'x = x, c'wlc_point'y = y } = Point { x = convert x, y = convert y }
    toPrimitive Point { x = x, y = y } = C'wlc_point { c'wlc_point'x = convert x, c'wlc_point'y = convert y }

data Size = Size { w :: Word32, h :: Word32 }
    deriving (Eq, Show, Ord)

instance Primitive C'wlc_size Size where
    fromPrimitive C'wlc_size { c'wlc_size'w = w, c'wlc_size'h = h } = Size { w = convert w, h = convert h }
    toPrimitive Size { w = w, h = h } = C'wlc_size { c'wlc_size'w = convert w, c'wlc_size'h = convert h }

data Geometry = Geometry { origin :: Point, size :: Size }
    deriving (Eq, Show, Ord)

instance Primitive C'wlc_geometry Geometry where
    fromPrimitive C'wlc_geometry { c'wlc_geometry'size = size, c'wlc_geometry'origin = origin } =
        Geometry { origin = fromPrimitive origin, size = fromPrimitive size }
    toPrimitive Geometry { size = size, origin = origin } =
        C'wlc_geometry { c'wlc_geometry'size = toPrimitive size, c'wlc_geometry'origin = toPrimitive origin }
