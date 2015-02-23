{-# LANGUAGE DeriveDataTypeable #-}

module Graphics.XHB.Ewmh.Atoms where

import Data.Typeable (Typeable)
import Data.Hashable (Hashable(..))
import Graphics.XHB.Atom

data UTF8_STRING = UTF8_STRING
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable UTF8_STRING where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike UTF8_STRING where
    toAtomName a = '_' : show a

data EwmhAtom =
    -- Root Window Properties
      NET_SUPPORTED
    | NET_CLIENT_LIST
    | NET_CLIENT_LIST_STACKING
    | NET_NUMBER_OF_DESKTOPS
    | NET_DESKTOP_GEOMETRY
    | NET_DESKTOP_VIEWPORT
    | NET_CURRENT_DESKTOP
    | NET_DESKTOP_NAMES
    | NET_ACTIVE_WINDOW
    | NET_WORKAREA
    | NET_SUPPORTING_WM_CHECK
    | NET_VIRTUAL_ROOTS
    | NET_DESKTOP_LAYOUT
    | NET_SHOWING_DESKTOP

    -- Other Root Window Messages
    | NET_CLOSE_WINDOW
    | NET_MOVERESIZE_WINDOW
    | NET_WM_MOVERESIZE
    | NET_RESTACK_WINDOW
    | NET_REQUEST_FRAME_EXTENTS

    -- Application Window Property
    | NET_WM_NAME
    | NET_WM_VISIBLE_NAME
    | NET_WM_ICON_NAME
    | NET_WM_VISIBLE_ICON_NAME
    | NET_WM_DESKTOP
    | NET_WM_WINDOW_TYPE
    | NET_WM_STATE
    | NET_WM_ALLOWED_ACTIONS
    | NET_WM_STRUT
    | NET_WM_STRUT_PARTIAL
    | NET_WM_ICON_GEOMETRY
    | NET_WM_ICON
    | NET_WM_PID
    | NET_WM_HANDLED_ICONS
    | NET_WM_USER_TIME
    | NET_WM_USER_TIME_WINDOW
    | NET_FRAME_EXTENTS
    | NET_WM_OPAQUE_REGION
    | NET_WM_BYPASS_COMPOSITOR

    -- Window Manager Protocols
    | NET_WM_PING
    | NET_WM_SYNC_REQUEST
    | NET_WM_SYNC_REQUEST_COUNTER
    | NET_WM_FULLSCREEN_MONITORS

    -- Other Properties
    | NET_WM_FULL_PLACEMENT

    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable EwmhAtom where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike EwmhAtom where
    toAtomName a = '_' : show a

data NET_WM_WINDOW_TYPE =
      NET_WM_WINDOW_TYPE_DESKTOP
    | NET_WM_WINDOW_TYPE_DOCK
    | NET_WM_WINDOW_TYPE_TOOLBAR
    | NET_WM_WINDOW_TYPE_MENU
    | NET_WM_WINDOW_TYPE_UTILITY
    | NET_WM_WINDOW_TYPE_SPLASH
    | NET_WM_WINDOW_TYPE_DIALOG
    | NET_WM_WINDOW_TYPE_DROPDOWN_MENU
    | NET_WM_WINDOW_TYPE_POPUP_MENU
    | NET_WM_WINDOW_TYPE_TOOLTIP
    | NET_WM_WINDOW_TYPE_NOTIFICATION
    | NET_WM_WINDOW_TYPE_COMBO
    | NET_WM_WINDOW_TYPE_DND
    | NET_WM_WINDOW_TYPE_NORMAL
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_WINDOW_TYPE where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_WINDOW_TYPE where
    toAtomName a = '_' : show a

data NET_WM_STATE =
      NET_WM_STATE_MODAL
    | NET_WM_STATE_STICKY
    | NET_WM_STATE_MAXIMIZED_VERT
    | NET_WM_STATE_MAXIMIZED_HORZ
    | NET_WM_STATE_SHADED
    | NET_WM_STATE_SKIP_TASKBAR
    | NET_WM_STATE_SKIP_PAGER
    | NET_WM_STATE_HIDDEN
    | NET_WM_STATE_FULLSCREEN
    | NET_WM_STATE_ABOVE
    | NET_WM_STATE_BELOW
    | NET_WM_STATE_DEMANDS_ATTENTION
    | NET_WM_STATE_FOCUSED
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_STATE where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_STATE where
    toAtomName a = '_' : show a

data NET_WM_ALLOWED_ACTIONS =
      NET_WM_ACTION_MOVE
    | NET_WM_ACTION_RESIZE
    | NET_WM_ACTION_MINIMIZE
    | NET_WM_ACTION_SHADE
    | NET_WM_ACTION_STICK
    | NET_WM_ACTION_MAXIMIZE_HORZ
    | NET_WM_ACTION_MAXIMIZE_VERT
    | NET_WM_ACTION_FULLSCREEN
    | NET_WM_ACTION_CHANGE_DESKTOP
    | NET_WM_ACTION_CLOSE
    | NET_WM_ACTION_ABOVE
    | NET_WM_ACTION_BELOW
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_ALLOWED_ACTIONS where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_ALLOWED_ACTIONS where
    toAtomName a = '_' : show a
