#include <bindings.dsl.h>
#include <wlc/wlc.h>

module Bindings.WLC.WLC where
#strict_import

import Bindings.WLC.Geometry

#synonym_t wlc_handle , Ptr ()

#integral_t enum wlc_log_type
#num WLC_LOG_INFO
#num WLC_LOG_WARN
#num WLC_LOG_ERROR
#num WLC_LOG_WAYLAND

#integral_t enum wlc_view_state_bit
#num WLC_BIT_MAXIMIZED
#num WLC_BIT_FULLSCREEN
#num WLC_BIT_RESIZING
#num WLC_BIT_MOVING
#num WLC_BIT_ACTIVATED

#callback_t log_handler_cb , <wlc_log_type> -> CString -> IO ()
#ccall wlc_log_set_handler , <log_handler_cb> -> IO ()

#callback_t output_created_cb , <wlc_handle> -> IO Bool
#ccall wlc_set_output_created_cb , <output_created_cb> -> IO ()

#callback_t output_destroyed_cb , <wlc_handle> -> IO ()
#ccall wlc_set_output_destroyed_cb , <output_destroyed_cb> -> IO ()

#callback_t output_focus_cb , <wlc_handle> -> Bool -> IO ()
#ccall wlc_set_output_focus_cb , <output_focus_cb> -> IO ()

#callback_t output_resolution_cb , <wlc_handle> -> Ptr <wlc_size> -> Ptr <wlc_size> -> IO ()
#ccall wlc_set_output_resolution_cb , <output_resolution_cb> -> IO ()

#callback_t output_render_pre_cb , <wlc_handle> -> IO ()
#ccall wlc_set_output_render_pre_cb , <output_render_pre_cb> -> IO ()

#callback_t output_render_post_cb , <wlc_handle> -> IO ()
#ccall wlc_set_output_render_post_cb , <output_render_post_cb> -> IO ()

#callback_t view_created_cb , <wlc_handle> -> IO Bool
#ccall wlc_set_view_created_cb , <view_created_cb> -> IO ()

#callback_t view_destroyed_cb , <wlc_handle> -> IO ()
#ccall wlc_set_view_destroyed_cb , <view_destroyed_cb> -> IO ()

#callback_t view_focus_cb , <wlc_handle> -> Bool -> IO ()
#ccall wlc_set_view_focus_cb , <view_focus_cb> -> IO ()

#callback_t view_move_to_output_cb , <wlc_handle> -> <wlc_handle> -> <wlc_handle> -> IO ()
#ccall wlc_set_view_move_to_output_cb , <view_move_to_output_cb> -> IO ()

#callback_t view_request_geometry_cb , <wlc_handle> -> Ptr <wlc_geometry> -> IO ()
#ccall wlc_set_view_request_geometry_cb , <view_request_geometry_cb> -> IO ()

#callback_t view_request_state_cb , <wlc_handle> -> <wlc_view_state_bit> -> Bool -> IO ()
#ccall wlc_set_view_request_state_cb , <view_request_state_cb> -> IO ()

#callback_t view_request_move_cb , <wlc_handle> -> Ptr <wlc_point> -> IO ()
#ccall wlc_set_view_request_move_cb , <view_request_move_cb> -> IO ()

#callback_t view_request_resize_cb , <wlc_handle> -> CUInt -> Ptr <wlc_point> -> IO ()
#ccall wlc_set_view_request_resize_cb , <view_request_resize_cb> -> IO ()

#callback_t view_render_pre_cb , <wlc_handle> -> IO ()
#ccall wlc_set_view_render_pre_cb , <view_render_pre_cb> -> IO ()

#callback_t view_render_post_cb , <wlc_handle> -> IO ()
#ccall wlc_set_view_render_post_cb , <view_render_post_cb> -> IO ()

#callback_t compositor_ready_cb , IO ()
#ccall wlc_set_compositor_ready_cb , <compositor_ready_cb> -> IO ()

#callback_t compositor_terminate_cb , IO ()
#ccall wlc_set_compositor_terminate_cb , <compositor_terminate_cb> -> IO ()

#ccall wlc_init2 , IO Bool
#ccall wlc_terminate , IO ()
#ccall wlc_exec , CString -> Ptr CString -> IO ()
#ccall wlc_run , IO ()
