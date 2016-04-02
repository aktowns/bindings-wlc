#include <bindings.dsl.h>
#include <wlc/wlc.h>

{-|
Module      : Bindings.WLC.WLC
Description : Core WLC
Copyright   : (c) Ashley Towns 2016
License     : BSD3
Maintainer  : mail@ashleytowns.id.au
Stability   : experimental
Portability : POSIX

Provides bindings to the core WLC API.
-}
module Bindings.WLC.Core where
#strict_import

import Bindings.WLC.Defines
import Bindings.WLC.Geometry

#opaque_t wlc_event_source
#opaque_t xkb_state
#opaque_t xkb_keymap
#opaque_t libinput_device

-- |wlc_log(), wlc_vlog();
#integral_t enum wlc_log_type
#num WLC_LOG_INFO
#num WLC_LOG_WARN
#num WLC_LOG_ERROR
#num WLC_LOG_WAYLAND

-- |wlc_get_backend_type();
#integral_t enum wlc_backend_type
#num WLC_BACKEND_NONE
#num WLC_BACKEND_DRM
#num WLC_BACKEND_X11

-- |mask in wlc_event_loop_add_fd();
#integral_t enum wlc_event_bit
#num WLC_EVENT_READABLE
#num WLC_EVENT_WRITABLE
#num WLC_EVENT_HANGUP
#num WLC_EVENT_ERROR

-- |wlc_view_get_state();
#integral_t enum wlc_view_state_bit
#num WLC_BIT_MAXIMIZED
#num WLC_BIT_FULLSCREEN
#num WLC_BIT_RESIZING
#num WLC_BIT_MOVING
#num WLC_BIT_ACTIVATED

-- |wlc_view_get_type();
#integral_t enum wlc_view_type_bit
-- |Override redirect (x11)
#num WLC_BIT_OVERRIDE_REDIRECT
-- |Tooltips, DnD's, menus (x11)
#num WLC_BIT_UNMANAGED
-- |Splash screens (x11)
#num WLC_BIT_SPLASH
-- |Modal windows (x11)
#num WLC_BIT_MODAL
-- |xdg-shell, wl-shell popups
#num WLC_BIT_POPUP

-- |wlc_view_set_geometry(); Edges in interface interface.view.request.resize function.
#integral_t enum wlc_resize_edge
#num WLC_RESIZE_EDGE_NONE
#num WLC_RESIZE_EDGE_TOP
#num WLC_RESIZE_EDGE_BOTTOM
#num WLC_RESIZE_EDGE_LEFT
#num WLC_RESIZE_EDGE_TOP_LEFT
#num WLC_RESIZE_EDGE_BOTTOM_LEFT
#num WLC_RESIZE_EDGE_RIGHT
#num WLC_RESIZE_EDGE_TOP_RIGHT
#num WLC_RESIZE_EDGE_BOTTOM_RIGHT

-- |Mods in interface.keyboard.key function.
#integral_t enum wlc_modifier_bit
#num WLC_BIT_MOD_SHIFT
#num WLC_BIT_MOD_CAPS
#num WLC_BIT_MOD_CTRL
#num WLC_BIT_MOD_ALT
#num WLC_BIT_MOD_MOD2
#num WLC_BIT_MOD_MOD3
#num WLC_BIT_MOD_LOGO
#num WLC_BIT_MOD_MOD5

-- |Leds in interface.keyboard.key function.
#integral_t enum wlc_led_bit
#num WLC_BIT_LED_NUM
#num WLC_BIT_LED_CAPS
#num WLC_BIT_LED_SCROLL

-- |State in interface.keyboard.key function.
#integral_t enum wlc_key_state
#num WLC_KEY_STATE_RELEASED
#num WLC_KEY_STATE_PRESSED

-- |State in interface.pointer.button function.
#integral_t enum wlc_button_state
#num WLC_BUTTON_STATE_RELEASED
#num WLC_BUTTON_STATE_PRESSED

-- |Axis in interface.pointer.scroll function.
#integral_t enum wlc_scroll_axis_bit
#num WLC_SCROLL_AXIS_VERTICAL
#num WLC_SCROLL_AXIS_HORIZONTAL

-- |Type in interface.touch.touch function
#integral_t enum wlc_touch_type
#num WLC_TOUCH_DOWN
#num WLC_TOUCH_UP
#num WLC_TOUCH_MOTION
#num WLC_TOUCH_FRAME
#num WLC_TOUCH_CANCEL

-- |State of keyboard modifiers in various functions.
#starttype struct wlc_modifiers
#field leds, <wlc_led_bit>
#field mods, <wlc_modifier_bit>
#stoptype

-- * Callback API
-- ** Types
-- *** Output
#callback_t output_created_cb , <wlc_handle> -> IO Bool
#callback_t output_destroyed_cb , <wlc_handle> -> IO ()
#callback_t output_focus_cb , <wlc_handle> -> Bool -> IO ()
#callback_t output_resolution_cb , <wlc_handle> -> Ptr <wlc_size> -> Ptr <wlc_size> -> IO ()
#callback_t output_render_pre_cb , <wlc_handle> -> IO ()
#callback_t output_render_post_cb , <wlc_handle> -> IO ()
-- *** View
#callback_t view_created_cb , <wlc_handle> -> IO Bool
#callback_t view_destroyed_cb , <wlc_handle> -> IO ()
#callback_t view_focus_cb , <wlc_handle> -> Bool -> IO ()
#callback_t view_move_to_output_cb , <wlc_handle> -> <wlc_handle> -> <wlc_handle> -> IO ()
#callback_t view_request_geometry_cb , <wlc_handle> -> Ptr <wlc_geometry> -> IO ()
#callback_t view_request_state_cb , <wlc_handle> -> <wlc_view_state_bit> -> Bool -> IO ()
#callback_t view_request_move_cb , <wlc_handle> -> Ptr <wlc_point> -> IO ()
#callback_t view_request_resize_cb , <wlc_handle> -> CUInt -> Ptr <wlc_point> -> IO ()
#callback_t view_render_pre_cb , <wlc_handle> -> IO ()
#callback_t view_render_post_cb , <wlc_handle> -> IO ()
-- *** Input
#callback_t keyboard_key_cb , <wlc_handle> -> CUInt -> Ptr <wlc_modifiers> -> CUInt -> <wlc_key_state> -> IO Bool
#callback_t pointer_button_cb , <wlc_handle> -> CUInt -> Ptr <wlc_modifiers> -> CUInt -> <wlc_button_state> -> Ptr <wlc_point> -> IO Bool
#callback_t pointer_scroll_cb , <wlc_handle> -> CUInt -> Ptr <wlc_modifiers> -> <wlc_scroll_axis_bit> -> Double -> IO Bool
#callback_t pointer_motion_cb , <wlc_handle> -> CUInt -> Ptr <wlc_point> -> IO Bool
#callback_t touch_cb , <wlc_handle> -> CUInt -> Ptr <wlc_modifiers> -> <wlc_touch_type> -> CInt -> Ptr <wlc_point> -> IO Bool
-- *** Other
#callback_t compositor_ready_cb , IO ()
#callback_t compositor_terminate_cb , IO ()

-- ** Functions
-- *** Output
-- |Output was created. Return false if you want to destroy the output. (e.g. failed to allocate data related to view)
#ccall wlc_set_output_created_cb , <output_created_cb> -> IO ()
-- |Output was destroyed.
#ccall wlc_set_output_destroyed_cb , <output_destroyed_cb> -> IO ()
-- |Output got or lost focus.
#ccall wlc_set_output_focus_cb , <output_focus_cb> -> IO ()
-- |Output resolution changed.
#ccall wlc_set_output_resolution_cb , <output_resolution_cb> -> IO ()
-- |Output pre render hook.
#ccall wlc_set_output_render_pre_cb , <output_render_pre_cb> -> IO ()
-- |Output post render hook.
#ccall wlc_set_output_render_post_cb , <output_render_post_cb> -> IO ()
-- *** View
-- |View was created. Return false if you want to destroy the view. (e.g. failed to allocate data related to view)
#ccall wlc_set_view_created_cb , <view_created_cb> -> IO ()
-- |View was destroyed.
#ccall wlc_set_view_destroyed_cb , <view_destroyed_cb> -> IO ()
-- |View got or lost focus.
#ccall wlc_set_view_focus_cb , <view_focus_cb> -> IO ()
-- |View was moved to output.
#ccall wlc_set_view_move_to_output_cb , <view_move_to_output_cb> -> IO ()
-- |Request to set given geometry for view. Apply using wlc_view_set_geometry to agree.
#ccall wlc_set_view_request_geometry_cb , <view_request_geometry_cb> -> IO ()
-- |Request to disable or enable the given state for view. Apply using wlc_view_set_state to agree.
#ccall wlc_set_view_request_state_cb , <view_request_state_cb> -> IO ()
-- |Request to move itself. Start a interactive move to agree.
#ccall wlc_set_view_request_move_cb , <view_request_move_cb> -> IO ()
-- |Request to resize itself with the given edges. Start a interactive resize to agree.
#ccall wlc_set_view_request_resize_cb , <view_request_resize_cb> -> IO ()
-- |View pre render hook.
#ccall wlc_set_view_render_pre_cb , <view_render_pre_cb> -> IO ()
-- |View post render hook.
#ccall wlc_set_view_render_post_cb , <view_render_post_cb> -> IO ()
-- *** Input
-- |Key event was triggered, view handle will be zero if there was no focus. Return true to prevent sending the
-- event to clients.
#ccall wlc_set_keyboard_key_cb , <keyboard_key_cb> -> IO ()
-- |Button event was triggered, view handle will be zero if there was no focus. Return true to prevent sending the
-- event to clients.
#ccall wlc_set_pointer_button_cb , <pointer_button_cb> -> IO ()
-- |Scroll event was triggered, view handle will be zero if there was no focus. Return true to prevent sending the
-- event to clients.
#ccall wlc_set_pointer_scroll_cb , <pointer_scroll_cb> -> IO ()
-- |Motion event was triggered, view handle will be zero if there was no focus. Apply with wlc_pointer_set_position to
-- agree. Return true to prevent sending the event to clients.
#ccall wlc_set_pointer_motion_cb , <pointer_motion_cb> -> IO ()
-- |Touch event was triggered, view handle will be zero if there was no focus. Return true to prevent sending the
-- event to clients.
#ccall wlc_set_touch_cb , <touch_cb> -> IO ()
-- *** Other
-- |Compositor is ready to accept clients.
#ccall wlc_set_compositor_ready_cb , <compositor_ready_cb> -> IO ()
-- |Compositor is about to terminate
#ccall wlc_set_compositor_terminate_cb , <compositor_terminate_cb> -> IO ()

-- * Core API

-- |Creates a log handler callback
#callback_t log_handler_cb , <wlc_log_type> -> CString -> IO ()

-- |Set log handler. Can be set before wlc_init.
#ccall wlc_log_set_handler , <log_handler_cb> -> IO ()

-- |Initialize wlc. Returns false on failure.
--
-- Avoid running unverified code before wlc_init as wlc compositor may be run with higher
-- privileges on non logind systems where compositor binary needs to be suid.
--
-- wlc_init's purpose is to initialize and drop privileges as soon as possible.
--
-- Callbacks should be set using wlc_set_*_cb functions before calling wlc_init2,
-- failing to do so will cause any callback the init may trigger to not be called.
#ccall wlc_init2 , IO Bool
-- |Terminate wlc.
#ccall wlc_terminate , IO ()
-- |Query backend wlc is using.
#ccall wlc_get_backend_type , IO <wlc_backend_type>
-- |Exec program.
#ccall wlc_exec , CString -> Ptr CString -> IO ()
-- |Run event loop.
#ccall wlc_run , IO ()
-- |Link custom data to handle.
#ccall wlc_handle_set_user_data , <wlc_handle> -> Ptr () -> IO ()
-- |Get linked custom data from handle.
#ccall wlc_handle_get_user_data , <wlc_handle> -> IO (Ptr ())

-- * Output API

-- |Get outputs. Returned array is a direct reference, careful when moving and destroying handles.
#ccall wlc_get_outputs , Ptr CSize -> IO <wlc_handle>
-- |Get focused output.
#ccall wlc_get_focused_output , IO <wlc_handle>
-- |Get output name.
#ccall wlc_output_get_name , <wlc_handle> -> IO CString
-- |Get sleep state.
#ccall wlc_output_get_sleep , <wlc_handle> -> IO Bool
-- |Wake up / sleep.
#ccall wlc_output_set_sleep , <wlc_handle> -> Bool -> IO ()
-- |Get resolution.
#ccall wlc_output_get_resolution , <wlc_handle> -> IO (Ptr <wlc_size>)
-- |Set resolution.
#ccall wlc_output_set_resolution , <wlc_handle> -> Ptr <wlc_size> -> IO ()
-- |Get current visibility bitmask.
#ccall wlc_output_get_mask , <wlc_handle> -> IO CUInt
-- |Set visibility bitmask.
#ccall wlc_output_set_mask , <wlc_handle> -> CUInt -> IO ()
-- |Get views in stack order. Returned array is a direct reference, careful when moving and destroying handles.
#ccall wlc_output_get_views , <wlc_handle> -> Ptr CSize -> IO (Ptr wlc_handle)
-- |Focus output. Pass zero for no focus.
#ccall wlc_output_focus , <wlc_handle> -> IO ()

-- * View API
-- |Focus view. Pass zero for no focus.
#ccall wlc_view_focus , <wlc_handle> -> IO ()
-- |Close view.
#ccall wlc_view_close , <wlc_handle> -> IO ()
-- |Get current output.
#ccall wlc_view_get_output , <wlc_handle> -> IO <wlc_handle>
-- |Set output. Alternatively you can wlc_output_set_views.
#ccall wlc_view_set_output , <wlc_handle> -> <wlc_handle> -> IO ()
-- |Send behind everything.
#ccall wlc_view_send_to_back , <wlc_handle> -> IO ()
-- |Send below another view.
#ccall wlc_view_send_below , <wlc_handle> -> <wlc_handle> -> IO ()
-- |Send above another view.
#ccall wlc_view_bring_above , <wlc_handle> -> <wlc_handle> -> IO ()
-- |Bring to front of everything.
#ccall wlc_view_bring_to_front , <wlc_handle> -> IO ()
-- |Get current visibility bitmask.
#ccall wlc_view_get_mask , <wlc_handle> -> IO CUInt
-- |Set visibility bitmask.
#ccall wlc_view_set_mask , <wlc_handle> -> CUInt -> IO ()
-- |Get current geometry. (what client sees)
#ccall wlc_view_get_geometry , <wlc_handle> -> IO (Ptr <wlc_geometry>)
-- |Get visible geometry. (what wlc displays)
#ccall wlc_view_get_visible_geometry , <wlc_handle> -> Ptr <wlc_geometry> -> IO ()
-- |Set geometry. Set edges if the geometry change is caused by interactive resize.
#ccall wlc_view_set_geometry , <wlc_handle> -> CUInt -> Ptr <wlc_geometry> -> IO ()
-- |Get type bitfield.
#ccall wlc_view_get_type , <wlc_handle> -> IO CUInt
-- |Set type bit. Toggle indicates whether it is set or not.
#ccall wlc_view_set_type , <wlc_handle> -> <wlc_view_type_bit> -> Bool -> IO ()
-- |Get current state bitfield.
#ccall wlc_view_get_state , <wlc_handle> -> IO CUInt
-- |Set state bit. Toggle indicates whether it is set or not.
#ccall wlc_view_set_state , <wlc_handle> -> <wlc_view_state_bit> -> Bool -> IO ()
-- |Get parent view.
#ccall wlc_view_get_parent , <wlc_handle> -> IO <wlc_handle>
-- |Set parent view.
#ccall wlc_view_set_parent , <wlc_handle> -> <wlc_handle> -> IO ()
-- |Get title.
#ccall wlc_view_get_title , <wlc_handle> -> IO CString
-- |Get class. (shell-surface only)
#ccall wlc_view_get_class , <wlc_handle> -> IO CString
-- |Get app id. (xdg-surface only)
#ccall wlc_view_get_app_id , <wlc_handle> -> IO CString

-- * Input API
-- |Get currently held keys.
#ccall wlc_keyboard_get_current_keys , Ptr CSize -> IO (Ptr CUInt)
-- |Utility function to convert raw keycode to keysym. Passed modifiers may transform the key.
#ccall wlc_keyboard_get_keysym_for_key , CUInt -> Ptr <wlc_modifiers> -> IO CUInt
-- |Utility function to convert raw keycode to Unicode/UTF-32 codepoint. Passed modifiers may transform the key.
#ccall wlc_keyboard_get_utf32_for_key , CUInt -> Ptr <wlc_modifiers> -> IO CUInt
-- |Get current pointer position.
#ccall wlc_pointer_get_position , Ptr <wlc_point> -> IO ()
-- |Set current pointer position.
#ccall wlc_pointer_set_position , Ptr <wlc_point> -> IO ()
