{-# LANGUAGE DeriveDataTypeable #-}

module Graphics.XHB.Ewmh.Atoms where

import Data.Typeable (Typeable)

class Show a => EwmhAtom a where
    toString :: a -> String
    toString a = "_" ++ show a

data NET_WM_STATE_HINT = NET_WM_STATE_MODAL
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
    deriving (Eq, Ord, Read, Show, Typeable)

instance EwmhAtom NET_WM_STATE_HINT

data ROOT_WINDOW_PROPERTY = NET_SUPPORTED
                          | NET_CLIENT_LIST
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
    deriving (Eq, Ord, Read, Show, Typeable)

instance EwmhAtom ROOT_WINDOW_PROPERTY

data ROOT_WINDOW_MESSAGES = NET_CLOSE_WINDOW
                          | NET_MOVERESIZE_WINDOW
                          | NET_WM_MOVERESIZE
                          | NET_RESTACK_WINDOW
                          | NET_REQUEST_FRAME_EXTENTS
    deriving (Eq, Ord, Read, Show, Typeable)

instance EwmhAtom ROOT_WINDOW_MESSAGES

data APPLICATION_WINDOW_PROPERTY = NET_WM_NAME
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
    deriving (Eq, Ord, Read, Show, Typeable)

instance EwmhAtom APPLICATION_WINDOW_PROPERTY

data WINDOW_MANAGER_PROTOCOL = NET_WM_PING
                             | NET_WM_SYNC_REQUEST
                             | NET_WM_FULLSCREEN_MONITORS
    deriving (Eq, Ord, Read, Show, Typeable)

instance EwmhAtom WINDOW_MANAGER_PROTOCOL
