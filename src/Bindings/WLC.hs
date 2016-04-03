{-# OPTIONS_HADDOCK hide #-}
{-|
Module      : Bindings.WLC
Description : WLC Bindings
Copyright   : (c) Ashley Towns 2016
License     : BSD3
Maintainer  : mail@ashleytowns.id.au
Stability   : experimental
Portability : POSIX

Provides bindings to the low level WLC API's
-}
module Bindings.WLC(module WLCBindings) where

import           Bindings.WLC.Core     as WLCBindings
import           Bindings.WLC.Defines  as WLCBindings
import           Bindings.WLC.Geometry as WLCBindings
import           Bindings.WLC.Render   as WLCBindings
import           Bindings.WLC.Wayland  as WLCBindings
