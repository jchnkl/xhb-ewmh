{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Graphics.XHB.Ewmh.Types
    ( Ewmh
    , EwmhT(..)
    , SOURCE_INDICATION(..)
    , NET_WM_STATE_HINT(..)
    , NET_WM_STATE_ACTION(..)
    , NET_MOVERESIZE_WINDOW_FLAG(..)
    ) where

import Control.Applicative (Applicative)
import Control.Monad.State (MonadState(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Graphics.XHB (Connection, BitEnum(..))
import Graphics.XHB.Atom

data SOURCE_INDICATION = SOURCE_NONE
                       | SOURCE_APPLICATION
                       | SOURCE_PAGER
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

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
    deriving (Eq, Ord, Read, Typeable)

instance Show NET_WM_STATE_HINT where
    show NET_WM_STATE_MODAL             = "_NET_WM_STATE_MODAL"
    show NET_WM_STATE_STICKY            = "_NET_WM_STATE_STICKY"
    show NET_WM_STATE_MAXIMIZED_VERT    = "_NET_WM_STATE_MAXIMIZED_VERT"
    show NET_WM_STATE_MAXIMIZED_HORZ    = "_NET_WM_STATE_MAXIMIZED_HORZ"
    show NET_WM_STATE_SHADED            = "_NET_WM_STATE_SHADED"
    show NET_WM_STATE_SKIP_TASKBAR      = "_NET_WM_STATE_SKIP_TASKBAR"
    show NET_WM_STATE_SKIP_PAGER        = "_NET_WM_STATE_SKIP_PAGER"
    show NET_WM_STATE_HIDDEN            = "_NET_WM_STATE_HIDDEN"
    show NET_WM_STATE_FULLSCREEN        = "_NET_WM_STATE_FULLSCREEN"
    show NET_WM_STATE_ABOVE             = "_NET_WM_STATE_ABOVE"
    show NET_WM_STATE_BELOW             = "_NET_WM_STATE_BELOW"
    show NET_WM_STATE_DEMANDS_ATTENTION = "_NET_WM_STATE_DEMANDS_ATTENTION"
    show NET_WM_STATE_FOCUSED           = "_NET_WM_STATE_FOCUSED"

data NET_WM_STATE_ACTION = NET_WM_STATE_REMOVE
                         | NET_WM_STATE_ADD
                         | NET_WM_STATE_TOGGLE
    deriving (Enum, Eq, Ord, Read, Typeable)

instance Show NET_WM_STATE_ACTION where
    show NET_WM_STATE_REMOVE = "_NET_WM_STATE_REMOVE"
    show NET_WM_STATE_ADD    = "_NET_WM_STATE_ADD"
    show NET_WM_STATE_TOGGLE = "_NET_WM_STATE_TOGGLE"

data NET_MOVERESIZE_WINDOW_FLAG = NET_MOVERESIZE_WINDOW_X
                                | NET_MOVERESIZE_WINDOW_Y
                                | NET_MOVERESIZE_WINDOW_WIDTH
                                | NET_MOVERESIZE_WINDOW_HEIGHT
                                | NET_MOVERESIZE_WINDOW_SOURCE SOURCE_INDICATION
    deriving (Eq, Ord, Read, Show, Typeable)

-- 8 ^= x, 9 ^= x, 10 ^= width, 11 ^= height, 12 ^= application, 13 ^= pager
instance BitEnum NET_MOVERESIZE_WINDOW_FLAG where
    -- toBit :: a -> Int
    toBit d = case d of
        NET_MOVERESIZE_WINDOW_X                         -> 8
        NET_MOVERESIZE_WINDOW_Y                         -> 9
        NET_MOVERESIZE_WINDOW_WIDTH                     -> 10
        NET_MOVERESIZE_WINDOW_HEIGHT                    -> 11
        NET_MOVERESIZE_WINDOW_SOURCE SOURCE_APPLICATION -> 12
        NET_MOVERESIZE_WINDOW_SOURCE SOURCE_PAGER       -> 13
        f -> error $ "no bit for NET_MOVERESIZE_WINDOW_FLAG: " ++ show f

    -- fromBit :: Int -> a
    fromBit i = case i of
        8 ->  NET_MOVERESIZE_WINDOW_X
        9 ->  NET_MOVERESIZE_WINDOW_Y
        10 -> NET_MOVERESIZE_WINDOW_WIDTH
        11 -> NET_MOVERESIZE_WINDOW_HEIGHT
        12 -> NET_MOVERESIZE_WINDOW_SOURCE SOURCE_APPLICATION
        13 -> NET_MOVERESIZE_WINDOW_SOURCE SOURCE_PAGER
        b  -> error $ "not a NET_MOVERESIZE_WINDOW_FLAG: " ++ show b

data NET_WM_MOVERESIZE = NET_WM_MOVERESIZE_SIZE_TOPLEFT
                       | NET_WM_MOVERESIZE_SIZE_TOP
                       | NET_WM_MOVERESIZE_SIZE_TOPRIGHT
                       | NET_WM_MOVERESIZE_SIZE_RIGHT
                       | NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT
                       | NET_WM_MOVERESIZE_SIZE_BOTTOM
                       | NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT
                       | NET_WM_MOVERESIZE_SIZE_LEFT
                       | NET_WM_MOVERESIZE_MOVE
                       | NET_WM_MOVERESIZE_SIZE_KEYBOARD
                       | NET_WM_MOVERESIZE_MOVE_KEYBOARD
                       | NET_WM_MOVERESIZE_CANCEL
    deriving (Eq, Ord, Read, Typeable)

instance Show NET_WM_MOVERESIZE where
    show NET_WM_MOVERESIZE_SIZE_TOPLEFT     = "_NET_WM_MOVERESIZE_SIZE_TOPLEFT"
    show NET_WM_MOVERESIZE_SIZE_TOP         = "_NET_WM_MOVERESIZE_SIZE_TOP"
    show NET_WM_MOVERESIZE_SIZE_TOPRIGHT    = "_NET_WM_MOVERESIZE_SIZE_TOPRIGHT"
    show NET_WM_MOVERESIZE_SIZE_RIGHT       = "_NET_WM_MOVERESIZE_SIZE_RIGHT"
    show NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT = "_NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT"
    show NET_WM_MOVERESIZE_SIZE_BOTTOM      = "_NET_WM_MOVERESIZE_SIZE_BOTTOM"
    show NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT  = "_NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT"
    show NET_WM_MOVERESIZE_SIZE_LEFT        = "_NET_WM_MOVERESIZE_SIZE_LEFT"
    show NET_WM_MOVERESIZE_MOVE             = "_NET_WM_MOVERESIZE_MOVE"
    show NET_WM_MOVERESIZE_SIZE_KEYBOARD    = "_NET_WM_MOVERESIZE_SIZE_KEYBOARD"
    show NET_WM_MOVERESIZE_MOVE_KEYBOARD    = "_NET_WM_MOVERESIZE_MOVE_KEYBOARD"
    show NET_WM_MOVERESIZE_CANCEL           = "_NET_WM_MOVERESIZE_CANCEL"

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
    deriving (Eq, Ord, Read, Typeable)

instance Show ROOT_WINDOW_PROPERTY where
    show NET_SUPPORTED           = "_NET_SUPPORTED"
    show NET_CLIENT_LIST         = "_NET_CLIENT_LIST"
    show NET_NUMBER_OF_DESKTOPS  = "_NET_NUMBER_OF_DESKTOPS"
    show NET_DESKTOP_GEOMETRY    = "_NET_DESKTOP_GEOMETRY"
    show NET_DESKTOP_VIEWPORT    = "_NET_DESKTOP_VIEWPORT"
    show NET_CURRENT_DESKTOP     = "_NET_CURRENT_DESKTOP"
    show NET_DESKTOP_NAMES       = "_NET_DESKTOP_NAMES"
    show NET_ACTIVE_WINDOW       = "_NET_ACTIVE_WINDOW"
    show NET_WORKAREA            = "_NET_WORKAREA"
    show NET_SUPPORTING_WM_CHECK = "_NET_SUPPORTING_WM_CHECK"
    show NET_VIRTUAL_ROOTS       = "_NET_VIRTUAL_ROOTS"
    show NET_DESKTOP_LAYOUT      = "_NET_DESKTOP_LAYOUT"
    show NET_SHOWING_DESKTOP     = "_NET_SHOWING_DESKTOP"

data ROOT_WINDOW_MESSAGES = NET_CLOSE_WINDOW
                          | NET_MOVERESIZE_WINDOW
                          | NET_WM_MOVERESIZE
                          | NET_RESTACK_WINDOW
                          | NET_REQUEST_FRAME_EXTENTS
    deriving (Eq, Ord, Read, Typeable)

instance Show ROOT_WINDOW_MESSAGES where
    show NET_CLOSE_WINDOW          = "_NET_CLOSE_WINDOW"
    show NET_MOVERESIZE_WINDOW     = "_NET_MOVERESIZE_WINDOW"
    show NET_WM_MOVERESIZE         = "_NET_WM_MOVERESIZE"
    show NET_RESTACK_WINDOW        = "_NET_RESTACK_WINDOW"
    show NET_REQUEST_FRAME_EXTENTS = "_NET_REQUEST_FRAME_EXTENTS"

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
    deriving (Eq, Ord, Read, Typeable)

instance Show APPLICATION_WINDOW_PROPERTY where
    show NET_WM_NAME              = "_NET_WM_NAME"
    show NET_WM_VISIBLE_NAME      = "_NET_WM_VISIBLE_NAME"
    show NET_WM_ICON_NAME         = "_NET_WM_ICON_NAME"
    show NET_WM_VISIBLE_ICON_NAME = "_NET_WM_VISIBLE_ICON_NAME"
    show NET_WM_DESKTOP           = "_NET_WM_DESKTOP"
    show NET_WM_WINDOW_TYPE       = "_NET_WM_WINDOW_TYPE"
    show NET_WM_STATE             = "_NET_WM_STATE"
    show NET_WM_ALLOWED_ACTIONS   = "_NET_WM_ALLOWED_ACTIONS"
    show NET_WM_STRUT             = "_NET_WM_STRUT"
    show NET_WM_STRUT_PARTIAL     = "_NET_WM_STRUT_PARTIAL"
    show NET_WM_ICON_GEOMETRY     = "_NET_WM_ICON_GEOMETRY"
    show NET_WM_ICON              = "_NET_WM_ICON"
    show NET_WM_PID               = "_NET_WM_PID"
    show NET_WM_HANDLED_ICONS     = "_NET_WM_HANDLED_ICONS"
    show NET_WM_USER_TIME         = "_NET_WM_USER_TIME"
    show NET_WM_USER_TIME_WINDOW  = "_NET_WM_USER_TIME_WINDOW"
    show NET_FRAME_EXTENTS        = "_NET_FRAME_EXTENTS"
    show NET_WM_OPAQUE_REGION     = "_NET_WM_OPAQUE_REGION"
    show NET_WM_BYPASS_COMPOSITOR = "_NET_WM_BYPASS_COMPOSITOR"

data WINDOW_MANAGER_PROTOCOL = NET_WM_PING
                             | NET_WM_SYNC_REQUEST
                             | NET_WM_FULLSCREEN_MONITORS
    deriving (Eq, Ord, Read, Typeable)

instance Show WINDOW_MANAGER_PROTOCOL where
    show NET_WM_PING                = "_NET_WM_PING"
    show NET_WM_SYNC_REQUEST        = "_NET_WM_SYNC_REQUEST"
    show NET_WM_FULLSCREEN_MONITORS = "_NET_WM_FULLSCREEN_MONITORS"

newtype EwmhT m a = EwmhT { unEwmhT :: ReaderT Connection (AtomT m) a }
    deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             )

type Ewmh = EwmhT IO

instance MonadTrans EwmhT where
    lift m = EwmhT . lift . lift $ m

instance (MonadState s m) => MonadState s (EwmhT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (MonadReader r m) => MonadReader r (EwmhT m) where
    ask = lift ask
    local f (EwmhT m) = EwmhT . ReaderT $ local f . runReaderT m

instance (MonadWriter w m) => MonadWriter w (EwmhT m) where
    tell = lift . tell
    listen = EwmhT . listen . unEwmhT
    pass = EwmhT . pass . unEwmhT
