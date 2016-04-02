{-|
Module      : System.WLC
Description : WLC Abstractions
Copyright   : (c) Ashley Towns 2016
License     : BSD3
Maintainer  : mail@ashleytowns.id.au
Stability   : experimental
Portability : POSIX

Provides abstractions over the low level WLC API's
-}
module System.WLC(module WLC) where

import System.WLC.Core      as WLC
import System.WLC.Geometry  as WLC
import System.WLC.Types     as WLC
