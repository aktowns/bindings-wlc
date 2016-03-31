#include <bindings.dsl.h>
#include <wlc/geometry.h>

module Bindings.WLC.Geometry where
#strict_import

import Foreign.C.Types

#starttype struct wlc_point
#field x, CInt
#field y, CInt
#stoptype

#starttype struct wlc_size
#field w, CUInt
#field h, CUInt
#stoptype

#starttype struct wlc_geometry
#field origin, <wlc_point>
#field size, <wlc_size>
#stoptype
