{-# OPTIONS_HADDOCK hide #-}
module System.WLC.Internal.Types where

import           Bindings.WLC.Core

-- Wrappers around the type synonyms so we can add the Primitive instances.
newtype WlcLogType       = WlcLogType { getRawLogType :: C'wlc_log_type } deriving (Show)
newtype WlcBackendType   = WlcBackendType { getRawBackendType :: C'wlc_backend_type } deriving (Show)
newtype WlcEventBit      = WlcEventBit { getEventBit :: C'wlc_event_bit } deriving (Show)
newtype WlcViewStateBit  = WlcViewStateBit { getViewStateBit :: C'wlc_view_state_bit } deriving (Show)
newtype WlcViewTypeBit   = WlcViewTypeBit { getViewTypeBit :: C'wlc_view_type_bit } deriving (Show)
newtype WlcResizeEdge    = WlcResizeEdge { getResizeEdge :: C'wlc_resize_edge } deriving (Show)
newtype WlcModifierBit   = WlcModifierBit { getModifierBit :: C'wlc_modifier_bit } deriving (Show)
newtype WlcLedBit        = WlcLedBit { getLedBit :: C'wlc_led_bit } deriving (Show)
newtype WlcKeyState      = WlcKeyState { getRawKeyState :: C'wlc_key_state } deriving (Show)
newtype WlcButtonState   = WlcButtonState { getRawButtonState :: C'wlc_button_state } deriving (Show)
newtype WlcScrollAxisBit = WlcScrollAxisBit { getScrollAxisBit :: C'wlc_scroll_axis_bit } deriving (Show)
newtype WlcTouchType     = WlcTouchType { getRawTouchType :: C'wlc_touch_type } deriving (Show)
