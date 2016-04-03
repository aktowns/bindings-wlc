{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : System.WLC.Types
Description : Types used throughout System.WLC
Copyright   : (c) Ashley Towns 2016
License     : BSD3
Maintainer  : mail@ashleytowns.id.au
Stability   : experimental
Portability : POSIX

Provides a basic set of types used through out the library, also provides toPrimitive and fromPrimitive to
convert them back and forth from the underlying C representations.
-}
module System.WLC.Types where

import           Bindings.WLC
import           System.WLC.Internal.Types
import           System.WLC.Utilities      (Primitive (..))

data LogType = LogInfo
             | LogWarn
             | LogError
             | LogWayland deriving (Eq, Show)

data BackendType = BackendNone
                 | BackendDRM
                 | BackendX11 deriving (Eq, Show)

data EventBit = EventReadable
              | EventWritable
              | EventHangup
              | EventError deriving (Eq, Show)

data ViewState = ViewMaximized
               | ViewFullscreen
               | ViewResizing
               | ViewMoving
               | ViewActivated deriving (Eq, Show)


data ViewType = ViewOverrideRedirect -- ^ Override redirect (x11)
              | ViewUnmanaged        -- ^ Tooltips, DnD's, menus (x11)
              | ViewSplash           -- ^ Splash screens (x11)
              | ViewModal            -- ^ Modal windows (x11)
              | ViewPopup            -- ^ xdg-shell, wl-shell popups
              deriving (Eq, Show)

data ResizeEdge = EdgeNone
                | EdgeTop
                | EdgeBottom
                | EdgeLeft
                | EdgeTopLeft
                | EdgeBottomLeft
                | EdgeRight
                | EdgeTopRight
                | EdgeBottomRight deriving (Eq, Show)

data Modifier = Shift
              | Caps
              | CTRL
              | Alt
              | Mod2
              | Mod3
              | ModLogo
              | Mod5 deriving (Eq, Show)


data Led = LedNum
         | LedCaps
         | LedScroll deriving (Eq, Show)


data KeyState = KeyReleased | KeyPressed deriving (Eq, Show)

data ButtonState = ButtonReleased | ButtonPressed deriving (Eq, Show)

data ScrollAxis = AxisVertical | AxisHorizontal deriving (Eq, Show)

data TouchType = TouchDown
               | TouchUp
               | TouchMotion
               | TouchFrame
               | TouchCancel deriving (Eq, Show)

data Modifiers = Modifiers { leds :: Led, mods :: Modifier }

newtype Output = Output { getOutputHandle :: C'wlc_handle }

newtype View = View { getViewHandle :: C'wlc_handle }

tryGetView :: C'wlc_handle -> Maybe View
tryGetView 0 = Nothing
tryGetView x = Just $ View x

tryGetOutput :: C'wlc_handle -> Maybe Output
tryGetOutput 0 = Nothing
tryGetOutput x = Just $ Output x

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

instance Primitive WlcBackendType BackendType where
    fromPrimitive (WlcBackendType x)
        | x == c'WLC_BACKEND_NONE = BackendNone
        | x == c'WLC_BACKEND_DRM = BackendDRM
        | x == c'WLC_BACKEND_X11 = BackendX11
        | otherwise = error $ "unexpected backend given: " ++ show x

    toPrimitive BackendNone = WlcBackendType c'WLC_BACKEND_NONE
    toPrimitive BackendDRM = WlcBackendType c'WLC_BACKEND_DRM
    toPrimitive BackendX11 = WlcBackendType c'WLC_BACKEND_X11

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

instance Primitive WlcLedBit Led where
    fromPrimitive (WlcLedBit x)
        | x == c'WLC_BIT_LED_NUM = LedNum
        | x == c'WLC_BIT_LED_CAPS = LedCaps
        | x == c'WLC_BIT_LED_SCROLL = LedScroll
        | otherwise = error $ "unexpected led bit given: " ++ show x

    toPrimitive LedNum = WlcLedBit c'WLC_BIT_LED_NUM
    toPrimitive LedCaps = WlcLedBit c'WLC_BIT_LED_CAPS
    toPrimitive LedScroll = WlcLedBit c'WLC_BIT_LED_SCROLL

instance Primitive WlcKeyState KeyState where
    fromPrimitive (WlcKeyState x)
        | x == c'WLC_KEY_STATE_RELEASED = KeyReleased
        | x == c'WLC_KEY_STATE_PRESSED = KeyPressed

    toPrimitive KeyReleased = WlcKeyState c'WLC_KEY_STATE_RELEASED
    toPrimitive KeyPressed = WlcKeyState c'WLC_KEY_STATE_PRESSED

instance Primitive WlcButtonState ButtonState where
    fromPrimitive (WlcButtonState x)
        | x == c'WLC_BUTTON_STATE_RELEASED = ButtonReleased
        | x == c'WLC_BUTTON_STATE_PRESSED = ButtonPressed

    toPrimitive ButtonReleased = WlcButtonState c'WLC_BUTTON_STATE_RELEASED
    toPrimitive ButtonPressed = WlcButtonState c'WLC_BUTTON_STATE_PRESSED

instance Primitive WlcScrollAxisBit ScrollAxis where
    fromPrimitive (WlcScrollAxisBit x)
        | x == c'WLC_SCROLL_AXIS_VERTICAL = AxisVertical
        | x == c'WLC_SCROLL_AXIS_HORIZONTAL = AxisHorizontal

    toPrimitive AxisVertical = WlcScrollAxisBit c'WLC_SCROLL_AXIS_VERTICAL
    toPrimitive AxisHorizontal = WlcScrollAxisBit c'WLC_SCROLL_AXIS_HORIZONTAL

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

instance Primitive C'wlc_modifiers Modifiers where
    fromPrimitive C'wlc_modifiers { c'wlc_modifiers'leds = leds, c'wlc_modifiers'mods = mods } =
        Modifiers { leds = fromPrimitive $ WlcLedBit leds, mods = fromPrimitive $ WlcModifierBit mods }
    toPrimitive Modifiers { leds = leds, mods = mods } =
        C'wlc_modifiers {
          c'wlc_modifiers'leds = getLedBit $ toPrimitive leds,
          c'wlc_modifiers'mods = getModifierBit $ toPrimitive mods }
