{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Graphics.XHB.Ewmh.Types where

import Data.Word (Word8, Word32)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Graphics.XHB (ButtonIndex(..), StackMode(..), WINDOW)
import Graphics.XHB.Atom
import Graphics.XHB.Ewmh.Atoms
import Graphics.XHB.Ewmh.Values
import Graphics.XHB.Ewmh.Serialize

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

instance Serialize NetDesktopLayout where
    serialize (NetDesktopLayout o s c r) = do
        serialize o
        serialize c
        serialize r
        serialize s

    deserialize = do
        o <- deserialize
        c <- deserialize
        r <- deserialize
        s <- deserialize
        return $ NetDesktopLayout o s c r

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
    { netWmState_action :: NET_WM_STATE_ACTION
    , netWmState_first_property :: NET_WM_STATE
    , netWmState_second_property :: Maybe NET_WM_STATE
    , netWmState_source_indication :: SourceIndication
    }
    deriving (Eq, Ord, Read, Show, Typeable)

data NetWmStrut = NetWmStrut
    { netWmStrut_left   :: Word32
    , netWmStrut_right  :: Word32
    , netWmStrut_top    :: Word32
    , netWmStrut_bottom :: Word32
    }
    deriving (Eq, Ord, Read, Show, Typeable)

instance Serialize NetWmStrut where
    serialize v = mapM_ ($ v) [ serialize . netWmStrut_left
                              , serialize . netWmStrut_right
                              , serialize . netWmStrut_top
                              , serialize . netWmStrut_bottom
                              ]

    deserialize = NetWmStrut <$> deserialize
                             <*> deserialize
                             <*> deserialize
                             <*> deserialize

data NetWmStrutPartial = NetWmStrutPartial
    { netWmStrutPartial_left           :: Word32
    , netWmStrutPartial_right          :: Word32
    , netWmStrutPartial_top            :: Word32
    , netWmStrutPartial_bottom         :: Word32
    , netWmStrutPartial_left_start_y   :: Word32
    , netWmStrutPartial_left_end_y     :: Word32
    , netWmStrutPartial_right_start_y  :: Word32
    , netWmStrutPartial_right_end_y    :: Word32
    , netWmStrutPartial_top_start_x    :: Word32
    , netWmStrutPartial_top_end_x      :: Word32
    , netWmStrutPartial_bottom_start_x :: Word32
    , netWmStrutPartial_bottom_end_x   :: Word32
    }
    deriving (Eq, Ord, Read, Show, Typeable)

instance Serialize NetWmStrutPartial where
    serialize v = mapM_ ($ v) [ serialize . netWmStrutPartial_left
                              , serialize . netWmStrutPartial_right
                              , serialize . netWmStrutPartial_top
                              , serialize . netWmStrutPartial_bottom
                              , serialize . netWmStrutPartial_left_start_y
                              , serialize . netWmStrutPartial_left_end_y
                              , serialize . netWmStrutPartial_right_start_y
                              , serialize . netWmStrutPartial_right_end_y
                              , serialize . netWmStrutPartial_top_start_x
                              , serialize . netWmStrutPartial_top_end_x
                              , serialize . netWmStrutPartial_bottom_start_x
                              , serialize . netWmStrutPartial_bottom_end_x
                              ]

    deserialize = NetWmStrutPartial <$> deserialize
                                    <*> deserialize
                                    <*> deserialize
                                    <*> deserialize
                                    <*> deserialize
                                    <*> deserialize
                                    <*> deserialize
                                    <*> deserialize
                                    <*> deserialize
                                    <*> deserialize
                                    <*> deserialize
                                    <*> deserialize

data NetWmIconGeometry = NetWmIconGeometry
    { netWmIconGeometry_x      :: Word32
    , netWmIconGeometry_y      :: Word32
    , netWmIconGeometry_width  :: Word32
    , netWmIconGeometry_height :: Word32
    }
    deriving (Eq, Ord, Read, Show, Typeable)

instance Serialize NetWmIconGeometry where
    serialize v = mapM_ ($ v) [ serialize . netWmIconGeometry_x
                              , serialize . netWmIconGeometry_y
                              , serialize . netWmIconGeometry_width
                              , serialize . netWmIconGeometry_height
                              ]

    deserialize = NetWmIconGeometry <$> deserialize
                                    <*> deserialize
                                    <*> deserialize
                                    <*> deserialize

data NetWmIconData = NetWmIconData
    { netWmIconData_a :: Word8
    , netWmIconData_r :: Word8
    , netWmIconData_g :: Word8
    , netWmIconData_b :: Word8
    }
    deriving (Eq, Ord, Read, Show, Typeable)

instance Serialize NetWmIconData where
    serialize v = mapM_ ($ v) [ serialize . netWmIconData_a
                              , serialize . netWmIconData_r
                              , serialize . netWmIconData_g
                              , serialize . netWmIconData_b
                              ]

    deserialize = NetWmIconData <$> deserialize
                                <*> deserialize
                                <*> deserialize
                                <*> deserialize

data NetWmIcon = NetWmIcon
    { netWmIcon_width  :: Word32
    , netWmIcon_height :: Word32
    , netWmIcon_data   :: [[NetWmIconData]]
    }
    deriving (Eq, Ord, Read, Show, Typeable)

instance Serialize NetWmIcon where
    serialize v = mapM_ ($ v) [ serialize . netWmIcon_width
                              , serialize . netWmIcon_height
                              , serialize . netWmIcon_data
                              ]

    deserialize = do
        width  <- deserialize
        height <- deserialize
        NetWmIcon width height <$>
            replicateM (fromIntegral height)
                (replicateM (fromIntegral width) deserialize)

netWmIconToPPM :: NetWmIcon -> String
netWmIconToPPM (NetWmIcon w h d) =
    "P3\n"
    ++ show w ++ " " ++ show h ++ "\n"
    ++ "255\n"
    ++ unlines (map (unwords . map conv) d)
    where
    conv (NetWmIconData _ r g b) = show r ++ " " ++ show g ++ " " ++ show b

data NetFrameExtents = NetFrameExtents
    { netFrameExtents_left   :: Word32
    , netFrameExtents_right  :: Word32
    , netFrameExtents_top    :: Word32
    , netFrameExtents_bottom :: Word32
    }
    deriving (Eq, Ord, Read, Show, Typeable)

instance Serialize NetFrameExtents where
    serialize v = mapM_ ($ v) [ serialize . netFrameExtents_left
                              , serialize . netFrameExtents_right
                              , serialize . netFrameExtents_top
                              , serialize . netFrameExtents_bottom
                              ]

    deserialize = NetFrameExtents <$> deserialize
                                  <*> deserialize
                                  <*> deserialize
                                  <*> deserialize

data NetWmOpaqueRegion = NetWmOpaqueRegion
    { netWmOpaqueRegion_x      :: Word32
    , netWmOpaqueRegion_y      :: Word32
    , netWmOpaqueRegion_width  :: Word32
    , netWmOpaqueRegion_height :: Word32
    }
    deriving (Eq, Ord, Read, Show, Typeable)

instance Serialize NetWmOpaqueRegion where
    serialize v = mapM_ ($ v) [ serialize . netWmOpaqueRegion_x
                              , serialize . netWmOpaqueRegion_y
                              , serialize . netWmOpaqueRegion_width
                              , serialize . netWmOpaqueRegion_height
                              ]

    deserialize = NetWmOpaqueRegion <$> deserialize
                                    <*> deserialize
                                    <*> deserialize
                                    <*> deserialize

data NetWmSyncRequest = NetWmSyncRequest
    { netWmSyncRequest_low  :: Word32
    , netWmSyncRequest_high :: Word32
    }
    deriving (Eq, Ord, Read, Show, Typeable)

data NetWmFullscreenMonitors = NetWmFullscreenMonitors
    { netWmFullscreenMonitors_top               :: Word32
    , netWmFullscreenMonitors_bottom            :: Word32
    , netWmFullscreenMonitors_left              :: Word32
    , netWmFullscreenMonitors_right             :: Word32
    , netWmFullscreenMonitors_source_indication :: SourceIndication
    }
    deriving (Eq, Ord, Read, Show, Typeable)
