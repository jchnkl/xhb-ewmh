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
    deriving (Eq, Ord, Read, Show, Typeable)

data NET_WM_STATE_ACTION = NET_WM_STATE_REMOVE
                         | NET_WM_STATE_ADD
                         | NET_WM_STATE_TOGGLE
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

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
    deriving (Eq, Ord, Read, Show, Typeable)

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
