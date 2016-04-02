#include <bindings.dsl.h>
#include <wlc/wlc-render.h>

{-|
Module      : Bindings.WLC.WLCWayland
Description : WLC Wayland
Copyright   : (c) Ashley Towns 2016
License     : BSD3
Maintainer  : mail@ashleytowns.id.au
Stability   : experimental
Portability : POSIX
-}
module Bindings.WLC.Wayland where
#strict_import

import Bindings.WLC.Defines
import Bindings.WLC.Geometry

#synonym_t wlc_resource , CUIntPtr

#opaque_t wl_resource
#opaque_t wl_display

#opaque_t wl_client
#opaque_t wl_interface

-- |Returns Wayland display.
#ccall wlc_get_wl_display , IO (Ptr <wl_display>)
-- |Returns view handle from wl_surface resource.
#ccall wlc_handle_from_wl_surface_resource , Ptr <wl_resource> -> IO <wlc_handle>
-- |Returns output handle from wl_output resource.
#ccall wlc_handle_from_wl_output_resource , Ptr <wl_resource> -> IO <wlc_handle>
-- |Returns internal wlc surface from wl_surface resource.
#ccall wlc_resource_from_wl_surface_resource , Ptr <wl_resource> -> IO <wlc_resource>
-- |Get surface size.
#ccall wlc_surface_get_size , <wlc_resource> -> IO (Ptr <wlc_size>)
-- |Return wl_surface resource from internal wlc surface.
#ccall wlc_surface_get_wl_resource , <wlc_resource> -> IO (Ptr <wl_resource>)
-- |Turns wl_surface into a wlc view. Returns 0 on failure. This will also trigger view.created callback as any view would.
-- For the extra arguments see details of wl_resource_create and wl_resource_set_implementation.
-- The extra arguments may be set NULL, if you are not implementing Wayland interface for the surface role.
#ccall wlc_view_from_surface , <wlc_resource> -> Ptr <wl_client> -> Ptr <wl_interface> -> Ptr () -> CUInt -> CUInt -> Ptr () -> IO <wlc_handle>
-- |Returns internal wlc surface from view handle
#ccall wlc_view_get_surface , <wlc_handle> -> IO <wlc_resource>
-- |Returns a list of the subsurfaces of the given surface
#ccall wlc_surface_get_subsurfaces , <wlc_resource> -> Ptr CSize -> IO (Ptr <wlc_resource>)
-- |Returns the size of a subsurface and its position relative to parent
#ccall wlc_get_subsurface_geometry , <wlc_resource> -> Ptr <wlc_geometry> -> IO ()
-- |Returns wl_client from view handle
#ccall wlc_view_get_wl_client , <wlc_handle> -> IO (Ptr <wl_client>)
-- |Returns surface role resource from view handle. Return value will be NULL if the view was not assigned role
-- or created with wlc_view_create_from_surface().
#ccall wlc_view_get_role , <wlc_handle> -> IO (Ptr <wl_resource>)
