{-# LANGUAGE MultiParamTypeClasses #-}
module System.WLC.Utilities(checkPtr, getPtrValue, apply3, Primitive(..)) where

import           Foreign.Ptr      (Ptr, nullPtr)
import           Foreign.Storable (Storable, peek)

class Primitive a b where
    fromPrimitive :: a -> b
    toPrimitive :: b -> a
    fromPrimitivePtr :: (Storable a) => Ptr a -> IO (Maybe b)
    fromPrimitivePtr ptr = fmap fromPrimitive <$> getPtrValue ptr

checkPtr :: Ptr a -> Maybe (Ptr a)
checkPtr ptr = if ptr /= nullPtr then Just ptr else Nothing

getPtrValue :: (Storable a) => Ptr a -> IO (Maybe a)
getPtrValue ptr = sequence $ fmap peek (checkPtr ptr)

apply3 :: (e -> a) -> (f -> b) -> (g -> c) -> (a -> b -> c -> d) -> (e -> f -> g -> d)
apply3 a1 a2 a3 fn e f g = fn (a1 e) (a2 f) (a3 g)
