{-# LANGUAGE MultiParamTypeClasses #-}

module System.WLC.Utilities(checkPtr, getPtrValue, Primitive(..)) where

import Foreign.Ptr(Ptr, nullPtr)
import Foreign.Storable(Storable, peek)

class Primitive a b where
  fromPrimitive :: a -> b
  fromPrimitivePtr :: (Storable a) => Ptr a -> IO (Maybe b)
  fromPrimitivePtr ptr = fmap fromPrimitive <$> getPtrValue ptr
  toPrimitive :: b -> a

checkPtr :: Ptr a -> Maybe (Ptr a)
checkPtr ptr = if ptr /= nullPtr then Just ptr else Nothing

getPtrValue :: (Storable a) => Ptr a -> IO (Maybe a)
getPtrValue ptr = sequence $ fmap peek (checkPtr ptr)
