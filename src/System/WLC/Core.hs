{-# LANGUAGE MultiParamTypeClasses #-}
module System.WLC.Core where

import Bindings.WLC.Defines
import Bindings.WLC.Core
import System.WLC.Geometry
import System.WLC.Utilities
import Data.Word(Word8, Word32)
import Control.Monad(liftM2)
import Foreign.Ptr(nullPtr)
import Foreign.C.String(peekCString, withCString, newCString)
import Foreign.Marshal.Array(withArray0)
import Foreign.Marshal.Alloc(free, alloca)
import Foreign.Marshal.Utils(with)
import Data.Convertible.Base
import Data.Convertible.Instances.C

-- Wrappers around the type synonyms so we can add the Primitive instances.
newtype WlcLogType       = WlcLogType C'wlc_log_type deriving (Show)
newtype WlcBackendType   = WlcBackendType C'wlc_backend_type deriving (Show)
newtype WlcEventBit      = WlcEventBit C'wlc_event_bit deriving (Show)
newtype WlcViewStateBit  = WlcViewStateBit { getViewStateBit :: C'wlc_view_state_bit } deriving (Show)
newtype WlcViewTypeBit   = WlcViewTypeBit { getViewTypeBit :: C'wlc_view_type_bit } deriving (Show)
newtype WlcResizeEdge    = WlcResizeEdge C'wlc_resize_edge deriving (Show)
newtype WlcModifierBit   = WlcModifierBit { getModifierBit :: C'wlc_modifier_bit } deriving (Show)
newtype WlcLedBit        = WlcLedBit { getLedBit :: C'wlc_led_bit } deriving (Show)
newtype WlcKeyState      = WlcKeyState C'wlc_key_state deriving (Show)
newtype WlcButtonState   = WlcButtonState C'wlc_button_state deriving (Show)
newtype WlcScrollAxisBit = WlcScrollAxisBit C'wlc_scroll_axis_bit deriving (Show)
newtype WlcTouchType     = WlcTouchType C'wlc_touch_type deriving (Show)

data LogType = LogInfo
             | LogWarn
             | LogError
             | LogWayland deriving (Eq, Show)

instance Primitive WlcLogType LogType where
    fromPrimitive (WlcLogType x)
        | x == c'WLC_LOG_INFO = LogInfo
        | x == c'WLC_LOG_WARN = LogWarn
        | x == c'WLC_LOG_ERROR = LogError
        | x == c'WLC_LOG_WAYLAND = LogWayland
        | otherwise = error $ "unexpected logger given: " ++ show x

    toPrimitive LogInfo = WlcLogType c'WLC_LOG_INFO
    toPrimitive LogWarn = WlcLogType c'WLC_LOG_WARN
    toPrimitive LogError = WlcLogType c'WLC_LOG_ERROR
    toPrimitive LogWayland = WlcLogType c'WLC_LOG_WAYLAND

data BackendType = BackendNone
                 | BackendDRM
                 | BackendX11 deriving (Eq, Show)

instance Primitive WlcBackendType BackendType where
    fromPrimitive (WlcBackendType x)
        | x == c'WLC_BACKEND_NONE = BackendNone
        | x == c'WLC_BACKEND_DRM = BackendDRM
        | x == c'WLC_BACKEND_X11 = BackendX11
        | otherwise = error $ "unexpected backend given: " ++ show x

    toPrimitive BackendNone = WlcBackendType c'WLC_BACKEND_NONE
    toPrimitive BackendDRM = WlcBackendType c'WLC_BACKEND_DRM
    toPrimitive BackendX11 = WlcBackendType c'WLC_BACKEND_X11

data EventBit = EventReadable
              | EventWritable
              | EventHangup
              | EventError deriving (Eq, Show)

instance Primitive WlcEventBit EventBit where
    fromPrimitive (WlcEventBit x)
        | x == c'WLC_EVENT_READABLE = EventReadable
        | x == c'WLC_EVENT_WRITABLE = EventWritable
        | x == c'WLC_EVENT_HANGUP = EventHangup
        | x == c'WLC_EVENT_ERROR = EventError
        | otherwise = error $ "unexpected event bit given: " ++ show x

    toPrimitive EventReadable = WlcEventBit c'WLC_EVENT_READABLE
    toPrimitive EventWritable = WlcEventBit c'WLC_EVENT_WRITABLE
    toPrimitive EventHangup = WlcEventBit c'WLC_EVENT_HANGUP
    toPrimitive EventError = WlcEventBit c'WLC_EVENT_ERROR

data ViewState = ViewMaximized
               | ViewFullscreen
               | ViewResizing
               | ViewMoving
               | ViewActivated deriving (Eq, Show)

instance Primitive WlcViewStateBit ViewState where
    fromPrimitive (WlcViewStateBit x)
        | x == c'WLC_BIT_MAXIMIZED = ViewMaximized
        | x == c'WLC_BIT_FULLSCREEN = ViewFullscreen
        | x == c'WLC_BIT_RESIZING = ViewResizing
        | x == c'WLC_BIT_MOVING = ViewMoving
        | x == c'WLC_BIT_ACTIVATED = ViewActivated
        | otherwise = error $ "unexpected view state bit given: " ++ show x

    toPrimitive ViewMaximized = WlcViewStateBit c'WLC_BIT_MAXIMIZED
    toPrimitive ViewFullscreen = WlcViewStateBit c'WLC_BIT_FULLSCREEN
    toPrimitive ViewResizing = WlcViewStateBit c'WLC_BIT_RESIZING
    toPrimitive ViewMoving = WlcViewStateBit c'WLC_BIT_MOVING
    toPrimitive ViewActivated = WlcViewStateBit c'WLC_BIT_ACTIVATED

data ViewType = ViewOverrideRedirect
              | ViewUnmanaged
              | ViewSplash
              | ViewModal
              | ViewPopup deriving (Eq, Show)

instance Primitive WlcViewTypeBit ViewType where
    fromPrimitive (WlcViewTypeBit x)
        | x == c'WLC_BIT_OVERRIDE_REDIRECT = ViewOverrideRedirect
        | x == c'WLC_BIT_UNMANAGED = ViewUnmanaged
        | x == c'WLC_BIT_SPLASH = ViewSplash
        | x == c'WLC_BIT_MODAL = ViewModal
        | x == c'WLC_BIT_POPUP = ViewPopup
        | otherwise = error $ "unexpected view type bit given: " ++ show x

    toPrimitive ViewOverrideRedirect = WlcViewTypeBit c'WLC_BIT_OVERRIDE_REDIRECT
    toPrimitive ViewUnmanaged = WlcViewTypeBit c'WLC_BIT_UNMANAGED
    toPrimitive ViewSplash = WlcViewTypeBit c'WLC_BIT_SPLASH
    toPrimitive ViewModal = WlcViewTypeBit c'WLC_BIT_MODAL
    toPrimitive ViewPopup = WlcViewTypeBit c'WLC_BIT_POPUP

data ResizeEdge = EdgeNone
                | EdgeTop
                | EdgeBottom
                | EdgeLeft
                | EdgeTopLeft
                | EdgeBottomLeft
                | EdgeRight
                | EdgeTopRight
                | EdgeBottomRight deriving (Eq, Show)

instance Primitive WlcResizeEdge ResizeEdge where
    fromPrimitive (WlcResizeEdge x)
        | x == c'WLC_RESIZE_EDGE_NONE = EdgeNone
        | x == c'WLC_RESIZE_EDGE_TOP = EdgeTop
        | x == c'WLC_RESIZE_EDGE_BOTTOM = EdgeBottom
        | x == c'WLC_RESIZE_EDGE_LEFT = EdgeLeft
        | x == c'WLC_RESIZE_EDGE_TOP_LEFT = EdgeTopLeft
        | x == c'WLC_RESIZE_EDGE_BOTTOM_LEFT = EdgeBottomLeft
        | x == c'WLC_RESIZE_EDGE_RIGHT = EdgeRight
        | x == c'WLC_RESIZE_EDGE_TOP_RIGHT = EdgeTopRight
        | x == c'WLC_RESIZE_EDGE_BOTTOM_RIGHT = EdgeBottomRight
        | otherwise = error $ "unexpected resize edge bit given: " ++ show x

    toPrimitive EdgeNone = WlcResizeEdge c'WLC_RESIZE_EDGE_NONE
    toPrimitive EdgeTop = WlcResizeEdge c'WLC_RESIZE_EDGE_TOP
    toPrimitive EdgeBottom = WlcResizeEdge c'WLC_RESIZE_EDGE_BOTTOM
    toPrimitive EdgeLeft = WlcResizeEdge c'WLC_RESIZE_EDGE_LEFT
    toPrimitive EdgeTopLeft = WlcResizeEdge c'WLC_RESIZE_EDGE_TOP_LEFT
    toPrimitive EdgeBottomLeft = WlcResizeEdge c'WLC_RESIZE_EDGE_BOTTOM_LEFT
    toPrimitive EdgeRight = WlcResizeEdge c'WLC_RESIZE_EDGE_RIGHT
    toPrimitive EdgeTopRight = WlcResizeEdge c'WLC_RESIZE_EDGE_TOP_RIGHT
    toPrimitive EdgeBottomRight = WlcResizeEdge c'WLC_RESIZE_EDGE_BOTTOM_RIGHT

data Modifier = Shift
              | Caps
              | CTRL
              | Alt
              | Mod2
              | Mod3
              | ModLogo
              | Mod5 deriving (Eq, Show)

instance Primitive WlcModifierBit Modifier where
    fromPrimitive (WlcModifierBit x)
        | x == c'WLC_BIT_MOD_SHIFT = Shift
        | x == c'WLC_BIT_MOD_CAPS = Caps
        | x == c'WLC_BIT_MOD_CTRL = CTRL
        | x == c'WLC_BIT_MOD_ALT = Alt
        | x == c'WLC_BIT_MOD_MOD2 = Mod2
        | x == c'WLC_BIT_MOD_MOD3 = Mod3
        | x == c'WLC_BIT_MOD_LOGO = ModLogo
        | x == c'WLC_BIT_MOD_MOD5 = Mod5
        | otherwise = error $ "unexpected modifier bit given: " ++ show x

    toPrimitive Shift = WlcModifierBit c'WLC_BIT_MOD_SHIFT
    toPrimitive Caps = WlcModifierBit c'WLC_BIT_MOD_CAPS
    toPrimitive CTRL = WlcModifierBit c'WLC_BIT_MOD_CTRL
    toPrimitive Alt = WlcModifierBit c'WLC_BIT_MOD_ALT
    toPrimitive Mod2 = WlcModifierBit c'WLC_BIT_MOD_MOD2
    toPrimitive Mod3 = WlcModifierBit c'WLC_BIT_MOD_MOD3
    toPrimitive ModLogo = WlcModifierBit c'WLC_BIT_MOD_LOGO
    toPrimitive Mod5 = WlcModifierBit c'WLC_BIT_MOD_MOD5

data Led = LedNum
         | LedCaps
         | LedScroll deriving (Eq, Show)

instance Primitive WlcLedBit Led where
    fromPrimitive (WlcLedBit x)
        | x == c'WLC_BIT_LED_NUM = LedNum
        | x == c'WLC_BIT_LED_CAPS = LedCaps
        | x == c'WLC_BIT_LED_SCROLL = LedScroll
        | otherwise = error $ "unexpected led bit given: " ++ show x

    toPrimitive LedNum = WlcLedBit c'WLC_BIT_LED_NUM
    toPrimitive LedCaps = WlcLedBit c'WLC_BIT_LED_CAPS
    toPrimitive LedScroll = WlcLedBit c'WLC_BIT_LED_SCROLL

data KeyState = KeyReleased | KeyPressed deriving (Eq, Show)

instance Primitive WlcKeyState KeyState where
    fromPrimitive (WlcKeyState x)
        | x == c'WLC_KEY_STATE_RELEASED = KeyReleased
        | x == c'WLC_KEY_STATE_PRESSED = KeyPressed

    toPrimitive KeyReleased = WlcKeyState c'WLC_KEY_STATE_RELEASED
    toPrimitive KeyPressed = WlcKeyState c'WLC_KEY_STATE_PRESSED

data ButtonState = ButtonReleased | ButtonPressed deriving (Eq, Show)

instance Primitive WlcButtonState ButtonState where
    fromPrimitive (WlcButtonState x)
        | x == c'WLC_BUTTON_STATE_RELEASED = ButtonReleased
        | x == c'WLC_BUTTON_STATE_PRESSED = ButtonPressed

    toPrimitive ButtonReleased = WlcButtonState c'WLC_BUTTON_STATE_RELEASED
    toPrimitive ButtonPressed = WlcButtonState c'WLC_BUTTON_STATE_PRESSED

data ScrollAxis = AxisVertical | AxisHorizontal deriving (Eq, Show)

instance Primitive WlcScrollAxisBit ScrollAxis where
    fromPrimitive (WlcScrollAxisBit x)
        | x == c'WLC_SCROLL_AXIS_VERTICAL = AxisVertical
        | x == c'WLC_SCROLL_AXIS_HORIZONTAL = AxisHorizontal

    toPrimitive AxisVertical = WlcScrollAxisBit c'WLC_SCROLL_AXIS_VERTICAL
    toPrimitive AxisHorizontal = WlcScrollAxisBit c'WLC_SCROLL_AXIS_HORIZONTAL

data TouchType = TouchDown
               | TouchUp
               | TouchMotion
               | TouchFrame
               | TouchCancel deriving (Eq, Show)

instance Primitive WlcTouchType TouchType where
    fromPrimitive (WlcTouchType x)
        | x == c'WLC_TOUCH_DOWN = TouchDown
        | x == c'WLC_TOUCH_UP = TouchUp
        | x == c'WLC_TOUCH_MOTION = TouchMotion
        | x == c'WLC_TOUCH_FRAME = TouchFrame
        | x == c'WLC_TOUCH_CANCEL = TouchCancel

    toPrimitive TouchDown = WlcTouchType c'WLC_TOUCH_DOWN
    toPrimitive TouchUp = WlcTouchType c'WLC_TOUCH_UP
    toPrimitive TouchMotion = WlcTouchType c'WLC_TOUCH_MOTION
    toPrimitive TouchFrame = WlcTouchType c'WLC_TOUCH_FRAME
    toPrimitive TouchCancel = WlcTouchType c'WLC_TOUCH_CANCEL

data Modifiers = Modifiers { leds :: Led, mods :: Modifier }

instance Primitive C'wlc_modifiers Modifiers where
    fromPrimitive C'wlc_modifiers { c'wlc_modifiers'leds = leds, c'wlc_modifiers'mods = mods } =
        Modifiers { leds = fromPrimitive $ WlcLedBit leds, mods = fromPrimitive $ WlcModifierBit mods }
    toPrimitive Modifiers { leds = leds, mods = mods } =
        C'wlc_modifiers {
          c'wlc_modifiers'leds = getLedBit $ toPrimitive leds,
          c'wlc_modifiers'mods = getModifierBit $ toPrimitive mods }

newtype Output = Output { getOutputHandle :: C'wlc_handle }
newtype View = View { getViewHandle :: C'wlc_handle }

tryGetView :: C'wlc_handle -> Maybe View
tryGetView 0 = Nothing
tryGetView x = Just $ View x

tryGetOutput :: C'wlc_handle -> Maybe Output
tryGetOutput 0 = Nothing
tryGetOutput x = Just $ Output x

data Callback = OutputCreated (Output -> IO Bool)
              | OutputDestroyed (Output -> IO ())
              | OutputFocus (Output -> Bool -> IO ())
              | OutputResolution (Output -> Size -> Size -> IO ())
              | OutputRenderPre (Output -> IO ())
              | OutputRenderPost (Output -> IO ())

              | ViewCreated (View -> IO Bool)
              | ViewDestroyed (View -> IO ())
              | ViewFocus (View -> Bool -> IO ())
              | ViewMoveToOutput (View -> Output -> Output -> IO ())
              | ViewRequestGeometry (View -> Geometry -> IO ())
              | ViewRequestState (View -> ViewState -> Bool -> IO ())
              | ViewRequestMove (View -> Point -> IO ())
              | ViewRequestResize (View -> ResizeEdge -> Point -> IO ())
              | ViewRenderPre (View -> IO ())
              | ViewRenderPost (View -> IO ())

              | KeyboardKey (Maybe View -> Int -> Modifiers -> Int -> KeyState -> IO Bool)
              | PointerButton (Maybe View -> Int -> Modifiers -> Int -> ButtonState -> Point -> IO Bool)
              | PointerScroll (Maybe View -> Int -> Modifiers -> ScrollAxis -> Double -> IO Bool)
              | PointerMotion (Maybe View -> Int -> Point -> IO Bool)
              | Touch (Maybe View -> Int -> Modifiers -> TouchType -> Int -> Point -> IO Bool)

              | CompositorReady (IO ())
              | CompositorTerminate (IO ())

apply3 :: (e -> a) -> (f -> b) -> (g -> c) -> (a -> b -> c -> d) -> (e -> f -> g -> d)
apply3 a1 a2 a3 fn e f g = fn (a1 e) (a2 f) (a3 g)

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

stringTag :: LogType -> String
stringTag LogInfo = "INFO"
stringTag LogWarn = "WARN"
stringTag LogError = "ERROR"
stringTag LogWayland = "WAYLAND"

logHandler :: (LogType -> String -> IO ()) -> IO ()
logHandler cb = mk'log_handler_cb (\typ text -> do
  str <- peekCString text
  cb (fromPrimitive $ WlcLogType typ) str) >>= c'wlc_log_set_handler

initialize :: IO Bool
initialize = c'wlc_init2

terminate :: IO ()
terminate = c'wlc_terminate

outputGetMask :: Output -> IO Word32
outputGetMask (Output view) = convert <$> c'wlc_output_get_mask view

outputSetMask :: Output -> Word32 -> IO ()
outputSetMask (Output view) mask = c'wlc_output_set_mask view (convert mask)

outputFocus :: Output -> IO ()
outputFocus (Output output) = c'wlc_output_focus output

viewFocus :: View -> IO ()
viewFocus (View view) = c'wlc_view_focus view

viewClose :: View -> IO ()
viewClose (View view) = c'wlc_view_close view

viewGetOutput :: View -> IO Output
viewGetOutput (View view) = Output <$> c'wlc_view_get_output view

viewSetOutput :: View -> Output -> IO ()
viewSetOutput (View view) (Output output) = c'wlc_view_set_output view output

viewSendToBack :: View -> IO ()
viewSendToBack (View view) = c'wlc_view_send_to_back view

viewSendBelow :: View -> View -> IO ()
viewSendBelow (View view) (View other) = c'wlc_view_send_below view other

viewBringAbove :: View -> View -> IO ()
viewBringAbove (View view) (View other) = c'wlc_view_bring_above view other

viewBringToFront :: View -> IO ()
viewBringToFront (View view) = c'wlc_view_bring_to_front view

viewGetMask :: View -> IO Word32
viewGetMask (View view) = convert <$> c'wlc_view_get_mask view

viewSetMask :: View -> Word32 -> IO ()
viewSetMask (View view) mask = c'wlc_view_set_mask view (convert mask)

viewGetGeometry :: View -> IO Geometry
viewGetGeometry (View view) = do
    geoPtr <- c'wlc_view_get_geometry view
    Just geo <- fromPrimitivePtr geoPtr
    return geo

viewGetViewType :: View -> IO ViewType
viewGetViewType (View view) = fromPrimitive . WlcViewTypeBit <$> c'wlc_view_get_type view

viewSetViewType :: View -> ViewType -> Bool -> IO ()
viewSetViewType (View view) vt = c'wlc_view_set_type view (getViewTypeBit $ toPrimitive vt)

viewGetViewState :: View -> IO ViewState
viewGetViewState (View view) = fromPrimitive . WlcViewStateBit <$> c'wlc_view_get_state view

viewSetViewState :: View -> ViewState -> Bool -> IO ()
viewSetViewState (View view) vs = c'wlc_view_set_state view (getViewStateBit $ toPrimitive vs)

viewGetParent :: View -> IO (Maybe View)
viewGetParent (View view) = tryGetView <$> c'wlc_view_get_parent view

viewSetParent :: View -> View -> IO ()
viewSetParent (View view) (View other) = c'wlc_view_set_parent view other

viewGetTitle :: View -> IO String
viewGetTitle (View view) = c'wlc_view_get_title view >>= peekCString

viewGetClass :: View -> IO String
viewGetClass (View view) = c'wlc_view_get_class view >>= peekCString

viewGetAppId :: View -> IO String
viewGetAppId (View view) = c'wlc_view_get_app_id view >>= peekCString

pointerGetPosition :: IO Point
pointerGetPosition =
    alloca (\point -> do
        c'wlc_pointer_get_position point
        Just pt <- fromPrimitivePtr point
        return pt)

pointerSetPosition :: Point -> IO ()
pointerSetPosition pt = with point c'wlc_pointer_set_position
    where point = toPrimitive pt

getBackendType :: IO BackendType
getBackendType = do
  backend <- c'wlc_get_backend_type
  return $ fromPrimitive (WlcBackendType backend)

exec :: String -> [String] -> IO ()
exec app args = do
    let fullArgs = app : args
    putStrLn $ "Executing: " ++ app ++ " with " ++ show fullArgs
    convertedArgs <- mapM newCString fullArgs
    withCString app $ withArray0 nullPtr convertedArgs . c'wlc_exec
    mapM_ free convertedArgs

run :: IO ()
run = c'wlc_run
