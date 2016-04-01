#include <bindings.dsl.h>
#include <wlc/wlc-render.h>

{-|
Module      : Bindings.WLC.WLCRender
Description : WLC Render
Copyright   : (c) Ashley Towns 2016
License     : BSD3
Maintainer  : mail@ashleytowns.id.au
Stability   : experimental
Portability : POSIX

The functions in this file provide some basic rendering capabilities.
*_render(), *_read(), *_write() functions should only be called during post/pre render callbacks.
wlc_output_schedule_render() is exception and may be used to force wlc to render new frame (causing callbacks to trigger).

For more advanced drawing you should directly use GLES2.
This is not documented as it's currently relying on the implementation details of wlc.
-}
module Bindings.WLC.WLCRender where
#strict_import

import Bindings.WLC.Defines
import Bindings.WLC.Geometry
import Bindings.WLC.WLCWayland

-- |Allowed pixel formats.
#integral_t enum wlc_pixel_format
#num WLC_RGBA8888

-- |Write pixel data with the specific format to output's framebuffer.
-- If the geometry is out of bounds, it will be automaticall clamped.
#ccall wlc_pixels_write , <wlc_pixel_format> -> Ptr <wlc_geometry> -> Ptr () -> IO ()

-- |Read pixel data from output's framebuffer.
-- If the geometry is out of bounds, it will be automatically clamped.
-- Potentially clamped geometry will be stored in out_geometry, to indicate width / height of the returned data.
#ccall wlc_pixels_read , <wlc_pixel_format> -> Ptr <wlc_geometry> -> Ptr <wlc_geometry> -> Ptr () -> IO ()

-- |Renders surface.
#ccall wlc_surface_render , <wlc_resource> -> Ptr <wlc_geometry> -> IO ()

-- |Schedules output for rendering next frame. If output was already scheduled this is no-op,
-- if output is currently rendering, it will render immediately after.
#ccall wlc_output_schedule_render , <wlc_handle> -> IO ()

-- |Adds frame callbacks of the given surface for the next output frame.
-- It applies recursively to all subsurfaces.
-- Useful when the compositor creates custom animations which require disabling internal rendering,
-- but still need to update the surface textures (for ex. video players).
#ccall wlc_surface_flush_frame_callbacks , <wlc_resource> -> IO ()

-- |Enabled renderers
#integral_t enum wlc_renderer
#num WLC_RENDERER_GLES2
#num WLC_NO_RENDERER

-- |Returns currently active renderer on the given output
#ccall wlc_output_get_renderer , <wlc_handle> -> IO <wlc_renderer>

#integral_t enum wlc_surface_format
#num SURFACE_RGB
#num SURFACE_RGBA
#num SURFACE_EGL
#num SURFACE_Y_UV
#num SURFACE_Y_U_V
#num SURFACE_Y_XUXV
