{-|
Module      : System.WLC.Core
Description : Core WLC
Copyright   : (c) Ashley Towns 2016
License     : BSD3
Maintainer  : mail@ashleytowns.id.au
Stability   : experimental
Portability : POSIX

Provides abstractions over the low level bindings to the core WLC API.
-}
module System.WLC.Core where

import Bindings.WLC
import System.WLC.Types
import System.WLC.Geometry
import System.WLC.Utilities
import System.WLC.Internal.Types

import Data.Word(Word8, Word32)
import Control.Monad(liftM2)
import Foreign.Ptr(nullPtr)
import Foreign.C.String(peekCString, withCString, newCString)
import Foreign.Marshal.Array(withArray0)
import Foreign.Marshal.Alloc(free, alloca)
import Foreign.Marshal.Utils(with)
import Data.Convertible.Base
import Data.Convertible.Instances.C

-- * Callback API

-- | Available callbacks to be handed to 'dispatchEvent'
data Callback = OutputCreated (Output -> IO Bool)                     -- ^ Output was created. Return false if you want to destroy the output.
              | OutputDestroyed (Output -> IO ())                     -- ^ Output was destroyed.
              | OutputFocus (Output -> Bool -> IO ())                 -- ^ Output got or lost focus.
              | OutputResolution (Output -> Size -> Size -> IO ())    -- ^ Output resolution changed.
              | OutputRenderPre (Output -> IO ())                     -- ^ Output pre render hook.
              | OutputRenderPost (Output -> IO ())                    -- ^ Output post render hook.

              | ViewCreated (View -> IO Bool)                         -- ^ View was created. Return false if you want to destroy the view.
              | ViewDestroyed (View -> IO ())                         -- ^ View was destroyed.
              | ViewFocus (View -> Bool -> IO ())                     -- ^ View got or lost focus.
              | ViewMoveToOutput (View -> Output -> Output -> IO ())  -- ^ View was moved to output.
              | ViewRequestGeometry (View -> Geometry -> IO ())       -- ^ Request to set given geometry for view. Apply using 'viewSetGeometry' to agree.
              | ViewRequestState (View -> ViewState -> Bool -> IO ()) -- ^ Request to disable or enable the given state for view. Apply using 'viewSetViewState' to agree.
              | ViewRequestMove (View -> Point -> IO ())              -- ^ Request to move itself. Start a interactive move to agree.
              | ViewRequestResize (View -> ResizeEdge -> Point -> IO ()) -- ^ Request to resize itself with the given edges. Start a interactive resize to agree.
              | ViewRenderPre (View -> IO ())                         -- ^ View pre render hook.
              | ViewRenderPost (View -> IO ())                        -- ^ View post render hook.

              | KeyboardKey (Maybe View -> Int -> Modifiers -> Int -> KeyState -> IO Bool)                -- ^ Key event was triggered, view will be 'Nothing' if there was no focus. Return true to prevent sending the event to clients.
              | PointerButton (Maybe View -> Int -> Modifiers -> Int -> ButtonState -> Point -> IO Bool)  -- ^ Button event was triggered, view will be 'Nothing' if there was no focus. Return true to prevent sending the event to clients.
              | PointerScroll (Maybe View -> Int -> Modifiers -> ScrollAxis -> Double -> IO Bool)         -- ^ Scroll event was triggered, view will be 'Nothing' if there was no focus. Return true to prevent sending the event to clients.
              | PointerMotion (Maybe View -> Int -> Point -> IO Bool)                                     -- ^ Motion event was triggered, view will be 'Nothing' if there was no focus. Apply with 'pointerSetPosition' to agree. Return true to prevent sending the event to clients.
              | Touch (Maybe View -> Int -> Modifiers -> TouchType -> Int -> Point -> IO Bool)            -- ^ Touch event was triggered, view will be 'Nothing' if there was no focus. Return true to prevent sending the event to clients.

              | CompositorReady (IO ())                               -- ^ Compositor is ready to accept clients.
              | CompositorTerminate (IO ())                           -- ^ Compositor is about to terminate

-- |Marshals the 'Callback' and applies it to the underlying library
dispatchEvent :: Callback -> IO ()
dispatchEvent (OutputCreated cb) = mk'output_created_cb (cb . Output) >>= c'wlc_set_output_created_cb
dispatchEvent (OutputDestroyed cb) = mk'output_destroyed_cb (cb . Output) >>= c'wlc_set_output_destroyed_cb
dispatchEvent (OutputFocus cb) = mk'output_focus_cb (cb . Output) >>= c'wlc_set_output_focus_cb
dispatchEvent (OutputResolution cb) = mk'output_resolution_cb (\output fromPtr toPtr -> do
    Just from <- fromPrimitivePtr fromPtr
    Just to <- fromPrimitivePtr toPtr
    cb (Output output) from to) >>= c'wlc_set_output_resolution_cb
dispatchEvent (OutputRenderPre cb) = mk'output_render_pre_cb (cb . Output) >>= c'wlc_set_output_render_pre_cb
dispatchEvent (OutputRenderPost cb) = mk'output_render_post_cb (cb . Output) >>= c'wlc_set_output_render_post_cb

dispatchEvent (ViewCreated cb) = mk'view_created_cb (cb . View) >>= c'wlc_set_view_created_cb
dispatchEvent (ViewDestroyed cb) = mk'view_destroyed_cb (cb . View) >>= c'wlc_set_view_destroyed_cb
dispatchEvent (ViewFocus cb) = mk'view_focus_cb (cb . View) >>= c'wlc_set_view_focus_cb
dispatchEvent (ViewMoveToOutput cb) =
    mk'view_move_to_output_cb (apply3 View Output Output cb) >>= c'wlc_set_view_move_to_output_cb
dispatchEvent (ViewRequestGeometry cb) = mk'view_request_geometry_cb (\view geometryPtr -> do
    Just geometry <- fromPrimitivePtr geometryPtr
    cb (View view) geometry) >>= c'wlc_set_view_request_geometry_cb
dispatchEvent (ViewRequestState cb) = mk'view_request_state_cb (\view statptr toggle ->
    cb (View view) (fromPrimitive $ WlcViewStateBit statptr) toggle) >>= c'wlc_set_view_request_state_cb
dispatchEvent (ViewRequestMove cb) = mk'view_request_move_cb (\view pointPtr -> do
    Just point <- fromPrimitivePtr pointPtr
    cb (View view) point) >>= c'wlc_set_view_request_move_cb
dispatchEvent (ViewRequestResize cb) = mk'view_request_resize_cb (\view edge pointPtr -> do
    Just point <- fromPrimitivePtr pointPtr
    cb (View view) (fromPrimitive $ WlcResizeEdge edge) point) >>= c'wlc_set_view_request_resize_cb
dispatchEvent (ViewRenderPre cb) = mk'view_render_pre_cb (cb . View) >>= c'wlc_set_view_render_pre_cb
dispatchEvent (ViewRenderPost cb) = mk'view_render_post_cb (cb . View) >>= c'wlc_set_view_render_post_cb

dispatchEvent (KeyboardKey cb) = mk'keyboard_key_cb (\view time modifiersPtr key keyState -> do
    Just modifier <- fromPrimitivePtr modifiersPtr
    let keyst = fromPrimitive $ WlcKeyState keyState
    cb (tryGetView view) (convert time) modifier (fromIntegral key) keyst) >>= c'wlc_set_keyboard_key_cb
dispatchEvent (PointerButton cb) = mk'pointer_button_cb (\view time modifiersPtr button buttonState pointPtr -> do
    Just point <- fromPrimitivePtr pointPtr
    Just modifier <- fromPrimitivePtr modifiersPtr
    let buttonst = fromPrimitive $ WlcButtonState buttonState
    cb (tryGetView view) (convert time) modifier (convert button) buttonst point) >>= c'wlc_set_pointer_button_cb
dispatchEvent (PointerScroll cb) = mk'pointer_scroll_cb (\view time modifiersPtr axisL ammount -> do
    Just modifiers <- fromPrimitivePtr modifiersPtr
    let axis = fromPrimitive $ WlcScrollAxisBit axisL
    cb (tryGetView view) (convert time) modifiers axis ammount) >>= c'wlc_set_pointer_scroll_cb
dispatchEvent (PointerMotion cb) = mk'pointer_motion_cb (\view time pointPtr -> do
    putStrLn $ "dispatchEvent: view=" ++ show view ++ " time=" ++ show time ++ " pointPtr=" ++ show pointPtr
    Just point <- fromPrimitivePtr pointPtr
    cb (tryGetView view) (convert time) point) >>= c'wlc_set_pointer_motion_cb
dispatchEvent (Touch cb) = mk'touch_cb (\view time modifiersPtr touchType slot pointPtr -> do
    Just point <- fromPrimitivePtr pointPtr
    Just modifiers <- fromPrimitivePtr modifiersPtr
    let tt = fromPrimitive $ WlcTouchType touchType
    cb (tryGetView view) (convert time) modifiers tt (convert slot) point) >>= c'wlc_set_touch_cb

dispatchEvent (CompositorReady cb) = mk'compositor_ready_cb cb >>= c'wlc_set_compositor_ready_cb
dispatchEvent (CompositorTerminate cb) = mk'compositor_terminate_cb cb >>= c'wlc_set_compositor_terminate_cb

-- * Core API

-- |Set log handler. Can be set before initialize.
logHandler :: (LogType -> String -> IO ()) -> IO ()
logHandler cb = mk'log_handler_cb (\typ text -> do
  str <- peekCString text
  cb (fromPrimitive $ WlcLogType typ) str) >>= c'wlc_log_set_handler

-- |Initialize wlc. Returns false on failure.
--
-- Avoid running unverified code before 'initialize' as wlc compositor may be run with higher
-- privileges on non logind systems where compositor binary needs to be suid.
--
-- initialize's purpose is to initialize and drop privileges as soon as possible.
--
-- Callbacks should be set using 'dispatchEvent' before calling 'initialize',
-- failing to do so will cause any callback the init may trigger to not be called.
initialize :: IO Bool
initialize = c'wlc_init2

-- |Terminate wlc.
terminate :: IO ()
terminate = c'wlc_terminate

-- |Query backend wlc is using.
getBackendType :: IO BackendType
getBackendType = do
  backend <- c'wlc_get_backend_type
  return $ fromPrimitive (WlcBackendType backend)

-- |Exec program.
exec :: String -> [String] -> IO ()
exec app args = do
    let fullArgs = app : args
    putStrLn $ "Executing: " ++ app ++ " with " ++ show fullArgs
    convertedArgs <- mapM newCString fullArgs
    withCString app $ withArray0 nullPtr convertedArgs . c'wlc_exec
    mapM_ free convertedArgs

-- |Run event loop.
run :: IO ()
run = c'wlc_run

-- ** Output

-- |Get current visibility bitmask.
outputGetMask :: Output -> IO Word32
outputGetMask (Output view) = convert <$> c'wlc_output_get_mask view

-- |Set visibility bitmask.
outputSetMask :: Output -> Word32 -> IO ()
outputSetMask (Output view) mask = c'wlc_output_set_mask view (convert mask)

-- |Focus output. Pass zero for no focus.
outputFocus :: Output -> IO ()
outputFocus (Output output) = c'wlc_output_focus output

-- ** View

-- |Focus view. Pass zero for no focus.
viewFocus :: View -> IO ()
viewFocus (View view) = c'wlc_view_focus view

-- |Close 'View'.
viewClose :: View -> IO ()
viewClose (View view) = c'wlc_view_close view

-- |Get current output.
viewGetOutput :: View -> IO Output
viewGetOutput (View view) = Output <$> c'wlc_view_get_output view

-- |Set output. Alternatively you can 'outputSetViews'.
viewSetOutput :: View -> Output -> IO ()
viewSetOutput (View view) (Output output) = c'wlc_view_set_output view output

-- |Send behind everything.
viewSendToBack :: View -> IO ()
viewSendToBack (View view) = c'wlc_view_send_to_back view

-- |Send below another 'View'.
viewSendBelow :: View -> View -> IO ()
viewSendBelow (View view) (View other) = c'wlc_view_send_below view other

-- |Send above another 'View'.
viewBringAbove :: View -> View -> IO ()
viewBringAbove (View view) (View other) = c'wlc_view_bring_above view other

-- |Bring to front of everything.
viewBringToFront :: View -> IO ()
viewBringToFront (View view) = c'wlc_view_bring_to_front view

-- |Get current visibility bitmask.
viewGetMask :: View -> IO Word32
viewGetMask (View view) = convert <$> c'wlc_view_get_mask view

-- |Set visibility bitmask.
viewSetMask :: View -> Word32 -> IO ()
viewSetMask (View view) mask = c'wlc_view_set_mask view (convert mask)

-- |Get current geometry. (what client sees)
viewGetGeometry :: View -> IO Geometry
viewGetGeometry (View view) = do
    geoPtr <- c'wlc_view_get_geometry view
    Just geo <- fromPrimitivePtr geoPtr
    return geo

-- |Get 'ViewType' bitfield.
viewGetViewType :: View -> IO ViewType
viewGetViewType (View view) = fromPrimitive . WlcViewTypeBit <$> c'wlc_view_get_type view

-- |Set 'ViewType' bit. Toggle indicates whether it is set or not.
viewSetViewType :: View -> ViewType -> Bool -> IO ()
viewSetViewType (View view) vt = c'wlc_view_set_type view (getViewTypeBit $ toPrimitive vt)

-- |Get current 'ViewState' bitfield.
viewGetViewState :: View -> IO ViewState
viewGetViewState (View view) = fromPrimitive . WlcViewStateBit <$> c'wlc_view_get_state view

-- |Set 'ViewState' bit. Toggle indicates whether it is set or not.
viewSetViewState :: View -> ViewState -> Bool -> IO ()
viewSetViewState (View view) vs = c'wlc_view_set_state view (getViewStateBit $ toPrimitive vs)

-- |Get parent 'View'.
viewGetParent :: View -> IO (Maybe View)
viewGetParent (View view) = tryGetView <$> c'wlc_view_get_parent view

-- |Set parent 'View'.
viewSetParent :: View -> View -> IO ()
viewSetParent (View view) (View other) = c'wlc_view_set_parent view other

-- |Get 'View' title.
viewGetTitle :: View -> IO String
viewGetTitle (View view) = c'wlc_view_get_title view >>= peekCString

-- |Get class. (shell-surface only)
viewGetClass :: View -> IO String
viewGetClass (View view) = c'wlc_view_get_class view >>= peekCString

-- |Get app id. (xdg-surface only)
viewGetAppId :: View -> IO String
viewGetAppId (View view) = c'wlc_view_get_app_id view >>= peekCString

-- ** Input

-- |Get current pointer position.
pointerGetPosition :: IO Point
pointerGetPosition =
    alloca (\point -> do
        c'wlc_pointer_get_position point
        Just pt <- fromPrimitivePtr point
        return pt)

-- |Set current pointer position.
pointerSetPosition :: Point -> IO ()
pointerSetPosition pt = with point c'wlc_pointer_set_position
    where point = toPrimitive pt
