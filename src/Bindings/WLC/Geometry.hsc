#include <bindings.dsl.h>
#include <wlc/geometry.h>

{-|
Module      : Bindings.WLC.Geometry
Description : WLC Geometry
Copyright   : (c) Ashley Towns 2016
License     : GPL-3
Maintainer  : mail@ashleytowns.id.au
Stability   : experimental
Portability : POSIX

Provides bindings to the geometry WLC data types.
-}
module Bindings.WLC.Geometry where
#strict_import

import Foreign.C.Types

-- |Fixed 2D point
#starttype struct wlc_point
#field x, CInt
#field y, CInt
#stoptype

-- |Fixed 2D size
#starttype struct wlc_size
#field w, CUInt
#field h, CUInt
#stoptype

-- |Fixed 2D point, size pair
#starttype struct wlc_geometry
#field origin, <wlc_point>
#field size, <wlc_size>
#stoptype
