{-# LANGUAGE DeriveDataTypeable #-}

module Graphics.XHB.Ewmh.Atoms where

import Data.Typeable (Typeable)
import Data.Hashable (Hashable(..))
import Graphics.XHB.Atom

-- | UTF8_STRING

data UTF8_STRING = UTF8_STRING
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable UTF8_STRING where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike UTF8_STRING where
    toAtomName a = '_' : show a

data EWMH_ATOM = EWMH_ATOM

-- Root Window Properties

-- | NET_SUPPORTED

data NET_SUPPORTED = NET_SUPPORTED
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_SUPPORTED where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_SUPPORTED where
    toAtomName a = '_' : show a

-- | NET_CLIENT_LIST

data NET_CLIENT_LIST = NET_CLIENT_LIST
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_CLIENT_LIST where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_CLIENT_LIST where
    toAtomName a = '_' : show a

-- | NET_CLIENT_LIST_STACKING

data NET_CLIENT_LIST_STACKING = NET_CLIENT_LIST_STACKING
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_CLIENT_LIST_STACKING where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_CLIENT_LIST_STACKING where
    toAtomName a = '_' : show a

-- | NET_NUMBER_OF_DESKTOPS

data NET_NUMBER_OF_DESKTOPS = NET_NUMBER_OF_DESKTOPS
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_NUMBER_OF_DESKTOPS where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_NUMBER_OF_DESKTOPS where
    toAtomName a = '_' : show a

-- | NET_DESKTOP_GEOMETRY

data NET_DESKTOP_GEOMETRY = NET_DESKTOP_GEOMETRY
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_DESKTOP_GEOMETRY where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_DESKTOP_GEOMETRY where
    toAtomName a = '_' : show a

-- | NET_DESKTOP_VIEWPORT

data NET_DESKTOP_VIEWPORT = NET_DESKTOP_VIEWPORT
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_DESKTOP_VIEWPORT where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_DESKTOP_VIEWPORT where
    toAtomName a = '_' : show a

-- | NET_CURRENT_DESKTOP

data NET_CURRENT_DESKTOP = NET_CURRENT_DESKTOP
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_CURRENT_DESKTOP where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_CURRENT_DESKTOP where
    toAtomName a = '_' : show a

-- | NET_DESKTOP_NAMES

data NET_DESKTOP_NAMES = NET_DESKTOP_NAMES
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_DESKTOP_NAMES where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_DESKTOP_NAMES where
    toAtomName a = '_' : show a

-- | NET_ACTIVE_WINDOW

data NET_ACTIVE_WINDOW = NET_ACTIVE_WINDOW
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_ACTIVE_WINDOW where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_ACTIVE_WINDOW where
    toAtomName a = '_' : show a

-- | NET_WORKAREA

data NET_WORKAREA = NET_WORKAREA
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WORKAREA where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WORKAREA where
    toAtomName a = '_' : show a

-- | NET_SUPPORTING_WM_CHECK

data NET_SUPPORTING_WM_CHECK = NET_SUPPORTING_WM_CHECK
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_SUPPORTING_WM_CHECK where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_SUPPORTING_WM_CHECK where
    toAtomName a = '_' : show a

-- | NET_VIRTUAL_ROOTS

data NET_VIRTUAL_ROOTS = NET_VIRTUAL_ROOTS
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_VIRTUAL_ROOTS where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_VIRTUAL_ROOTS where
    toAtomName a = '_' : show a

-- | NET_DESKTOP_LAYOUT

data NET_DESKTOP_LAYOUT = NET_DESKTOP_LAYOUT
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_DESKTOP_LAYOUT where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_DESKTOP_LAYOUT where
    toAtomName a = '_' : show a

-- | NET_SHOWING_DESKTOP

data NET_SHOWING_DESKTOP = NET_SHOWING_DESKTOP
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_SHOWING_DESKTOP where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_SHOWING_DESKTOP where
    toAtomName a = '_' : show a


-- Other Root Window Messages

-- | NET_CLOSE_WINDOW

data NET_CLOSE_WINDOW = NET_CLOSE_WINDOW
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_CLOSE_WINDOW where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_CLOSE_WINDOW where
    toAtomName a = '_' : show a

-- | NET_MOVERESIZE_WINDOW

data NET_MOVERESIZE_WINDOW = NET_MOVERESIZE_WINDOW
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_MOVERESIZE_WINDOW where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_MOVERESIZE_WINDOW where
    toAtomName a = '_' : show a

-- | NET_WM_MOVERESIZE

data NET_WM_MOVERESIZE = NET_WM_MOVERESIZE
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_MOVERESIZE where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_MOVERESIZE where
    toAtomName a = '_' : show a

-- | NET_RESTACK_WINDOW

data NET_RESTACK_WINDOW = NET_RESTACK_WINDOW
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_RESTACK_WINDOW where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_RESTACK_WINDOW where
    toAtomName a = '_' : show a

-- | NET_REQUEST_FRAME_EXTENTS

data NET_REQUEST_FRAME_EXTENTS = NET_REQUEST_FRAME_EXTENTS
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_REQUEST_FRAME_EXTENTS where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_REQUEST_FRAME_EXTENTS where
    toAtomName a = '_' : show a

-- Application Window Property

-- | NET_WM_NAME

data NET_WM_NAME = NET_WM_NAME
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_NAME where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_NAME where
    toAtomName a = '_' : show a

-- | NET_WM_VISIBLE_NAME

data NET_WM_VISIBLE_NAME = NET_WM_VISIBLE_NAME
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_VISIBLE_NAME where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_VISIBLE_NAME where
    toAtomName a = '_' : show a

-- | NET_WM_ICON_NAME

data NET_WM_ICON_NAME = NET_WM_ICON_NAME
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_ICON_NAME where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_ICON_NAME where
    toAtomName a = '_' : show a

-- | NET_WM_VISIBLE_ICON_NAME

data NET_WM_VISIBLE_ICON_NAME = NET_WM_VISIBLE_ICON_NAME
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_VISIBLE_ICON_NAME where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_VISIBLE_ICON_NAME where
    toAtomName a = '_' : show a

-- | NET_WM_DESKTOP

data NET_WM_DESKTOP = NET_WM_DESKTOP
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_DESKTOP where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_DESKTOP where
    toAtomName a = '_' : show a

-- | NET_WM_STRUT

data NET_WM_STRUT = NET_WM_STRUT
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_STRUT where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_STRUT where
    toAtomName a = '_' : show a

-- | NET_WM_STRUT_PARTIAL

data NET_WM_STRUT_PARTIAL = NET_WM_STRUT_PARTIAL
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_STRUT_PARTIAL where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_STRUT_PARTIAL where
    toAtomName a = '_' : show a

-- | NET_WM_ICON_GEOMETRY

data NET_WM_ICON_GEOMETRY = NET_WM_ICON_GEOMETRY
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_ICON_GEOMETRY where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_ICON_GEOMETRY where
    toAtomName a = '_' : show a

-- | NET_WM_ICON

data NET_WM_ICON = NET_WM_ICON
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_ICON where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_ICON where
    toAtomName a = '_' : show a

-- | NET_WM_PID

data NET_WM_PID = NET_WM_PID
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_PID where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_PID where
    toAtomName a = '_' : show a

-- | NET_WM_HANDLED_ICONS

data NET_WM_HANDLED_ICONS = NET_WM_HANDLED_ICONS
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_HANDLED_ICONS where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_HANDLED_ICONS where
    toAtomName a = '_' : show a

-- | NET_WM_USER_TIME

data NET_WM_USER_TIME = NET_WM_USER_TIME
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_USER_TIME where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_USER_TIME where
    toAtomName a = '_' : show a

-- | NET_WM_USER_TIME_WINDOW

data NET_WM_USER_TIME_WINDOW = NET_WM_USER_TIME_WINDOW
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_USER_TIME_WINDOW where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_USER_TIME_WINDOW where
    toAtomName a = '_' : show a

-- | NET_FRAME_EXTENTS

data NET_FRAME_EXTENTS = NET_FRAME_EXTENTS
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_FRAME_EXTENTS where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_FRAME_EXTENTS where
    toAtomName a = '_' : show a

-- | NET_WM_OPAQUE_REGION

data NET_WM_OPAQUE_REGION = NET_WM_OPAQUE_REGION
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_OPAQUE_REGION where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_OPAQUE_REGION where
    toAtomName a = '_' : show a

-- | NET_WM_BYPASS_COMPOSITOR

data NET_WM_BYPASS_COMPOSITOR = NET_WM_BYPASS_COMPOSITOR
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_BYPASS_COMPOSITOR where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_BYPASS_COMPOSITOR where
    toAtomName a = '_' : show a

-- Window Manager Protocols

-- | NET_WM_PING

data NET_WM_PING = NET_WM_PING
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_PING where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_PING where
    toAtomName a = '_' : show a

-- | NET_WM_SYNC_REQUEST

data NET_WM_SYNC_REQUEST = NET_WM_SYNC_REQUEST
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_SYNC_REQUEST where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_SYNC_REQUEST where
    toAtomName a = '_' : show a

-- | NET_WM_SYNC_REQUEST_COUNTER

data NET_WM_SYNC_REQUEST_COUNTER = NET_WM_SYNC_REQUEST_COUNTER
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_SYNC_REQUEST_COUNTER where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_SYNC_REQUEST_COUNTER where
    toAtomName a = '_' : show a

-- | NET_WM_FULLSCREEN_MONITORS

data NET_WM_FULLSCREEN_MONITORS = NET_WM_FULLSCREEN_MONITORS
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_FULLSCREEN_MONITORS where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_FULLSCREEN_MONITORS where
    toAtomName a = '_' : show a

-- Other Properties

-- | NET_WM_FULL_PLACEMENT

data NET_WM_FULL_PLACEMENT = NET_WM_FULL_PLACEMENT
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Hashable NET_WM_FULL_PLACEMENT where
    hashWithSalt s = hashWithSalt s . show

instance AtomLike NET_WM_FULL_PLACEMENT where
    toAtomName a = '_' : show a

-- | NET_WM_WINDOW_TYPE

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

-- | NET_WM_STATE

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

-- | NET_WM_ALLOWED_ACTIONS

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
