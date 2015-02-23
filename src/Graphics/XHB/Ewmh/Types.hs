{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Graphics.XHB.Ewmh.Types where

import Data.Word (Word32)
import Data.Typeable (Typeable)
import Graphics.XHB (ButtonIndex(..), StackMode(..), WINDOW)
import Graphics.XHB.Atom
import Graphics.XHB.Ewmh.Atoms
import Graphics.XHB.Ewmh.Values

type EwmhT = AtomT

type Ewmh = EwmhT IO

type MonadEwmh = MonadAtom

data NetSupported = NetSupported
    { ewmhAtoms           :: [EWMH_ATOM]
    , netWmStates         :: [NET_WM_STATE]
    , netWmAllowedActions :: [NET_WM_ALLOWED_ACTIONS]
    , netWmWindowTypes    :: [NET_WM_WINDOW_TYPE]
    }
    deriving (Eq, Ord, Read, Show, Typeable)

data NetDesktopLayout = NetDesktopLayout
    { orientation     :: NET_DESKTOP_LAYOUT_ORIENTATION
    , starting_corner :: NET_DESKTOP_LAYOUT_STARTING_CORNER
    , columns         :: Word32
    , rows            :: Word32
    }
    deriving (Eq, Ord, Read, Show, Typeable)

data NetMoveresizeWindow = NetMoveresizeWindow
    { netMoveresizeWindow_sourceIndication :: SourceIndication
    , netMoveresizeWindow_gravity          :: Gravity
    , netMoveresizeWindow_x                :: Maybe Int
    , netMoveresizeWindow_y                :: Maybe Int
    , netMoveresizeWindow_width            :: Maybe Word32
    , netMoveresizeWindow_height           :: Maybe Word32
    }
    deriving (Eq, Ord, Read, Show, Typeable)

-- TODO: push to xhb package
deriving instance Eq ButtonIndex
deriving instance Ord ButtonIndex
deriving instance Read ButtonIndex

data NetWmMoveresize = NetWmMoveresize
    { netWmMoveresize_x_root           :: Maybe Int
    , netWmMoveresize_y_root           :: Maybe Int
    , netWmMoveresize_direction        :: NET_WM_MOVERESIZE_DIRECTION
    , netWmMoveresize_button           :: ButtonIndex
    , netWmMoveresize_sourceIndication :: SourceIndication
    }
    deriving (Eq, Ord, Read, Show, Typeable)

-- TODO: push to xhb package
deriving instance Eq StackMode
deriving instance Ord StackMode
deriving instance Read StackMode

-- no Read because there's no Read instance for WINDOW
data NetRestackWindow = NetRestackWindow
    { netRestackWindow_sourceIndication :: SourceIndication
    , netRestackWindow_sibling_window   :: WINDOW
    , netRestackWindow_detail           :: StackMode
    }
    deriving (Eq, Ord, Show, Typeable)

data NetWmDesktop = NetWmDesktop
    { netWmDesktop_new_desktop       :: Word32
    , netWmDesktop_source_indication :: SourceIndication
    }
    deriving (Eq, Ord, Read, Show, Typeable)

data NetWmState = NetWmState
    { netWmState_window :: WINDOW
    , netWmState_action :: NET_WM_STATE_ACTION
    , netWmState_first_property :: NET_WM_STATE
    , netWmState_second_property :: NET_WM_STATE
    , netWmState_source_indication :: SourceIndication
    }
    deriving (Eq, Ord, Show, Typeable)
