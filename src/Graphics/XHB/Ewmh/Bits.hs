{-# LANGUAGE DeriveDataTypeable #-}

module Graphics.XHB.Ewmh.Bits where

import Data.Typeable (Typeable)
import Graphics.XHB (BitEnum(..))

data NET_DESKTOP_LAYOUT_ORIENTATION = NET_WM_ORIENTATION_HORZ
                                    | NET_WM_ORIENTATION_VERT
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance BitEnum NET_DESKTOP_LAYOUT_ORIENTATION where
    toBit v = case v of
        NET_WM_ORIENTATION_HORZ -> 0
        NET_WM_ORIENTATION_VERT -> 1

    fromBit v = case v of
        0 -> NET_WM_ORIENTATION_HORZ
        1 -> NET_WM_ORIENTATION_VERT
        _ -> error "NET_DESKTOP_LAYOUT_ORIENTATION: no such bit"

data NET_DESKTOP_LAYOUT_STARTING_CORNER = NET_WM_TOPLEFT
                                        | NET_WM_TOPRIGHT
                                        | NET_WM_BOTTOMRIGHT
                                        | NET_WM_BOTTOMLEFT
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance BitEnum NET_DESKTOP_LAYOUT_STARTING_CORNER where
    toBit v = case v of
        NET_WM_TOPLEFT     -> 0
        NET_WM_TOPRIGHT    -> 1
        NET_WM_BOTTOMRIGHT -> 2
        NET_WM_BOTTOMLEFT  -> 3

    fromBit v = case v of
        0 -> NET_WM_TOPLEFT
        1 -> NET_WM_TOPRIGHT
        2 -> NET_WM_BOTTOMRIGHT
        3 -> NET_WM_BOTTOMLEFT
        _ -> error "NET_DESKTOP_LAYOUT_STARTING_CORNER: no such bit"

data NET_WM_MOVERESIZE_DIRECTION = NET_WM_MOVERESIZE_SIZE_TOPLEFT
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

instance BitEnum NET_WM_MOVERESIZE_DIRECTION where
    toBit v = case v of
        NET_WM_MOVERESIZE_SIZE_TOPLEFT     -> 0
        NET_WM_MOVERESIZE_SIZE_TOP         -> 1
        NET_WM_MOVERESIZE_SIZE_TOPRIGHT    -> 2
        NET_WM_MOVERESIZE_SIZE_RIGHT       -> 3
        NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT -> 4
        NET_WM_MOVERESIZE_SIZE_BOTTOM      -> 5
        NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT  -> 6
        NET_WM_MOVERESIZE_SIZE_LEFT        -> 7
        NET_WM_MOVERESIZE_MOVE             -> 8
        NET_WM_MOVERESIZE_SIZE_KEYBOARD    -> 9
        NET_WM_MOVERESIZE_MOVE_KEYBOARD    -> 10
        NET_WM_MOVERESIZE_CANCEL           -> 11

    fromBit v = case v of
        0  -> NET_WM_MOVERESIZE_SIZE_TOPLEFT
        1  -> NET_WM_MOVERESIZE_SIZE_TOP
        2  -> NET_WM_MOVERESIZE_SIZE_TOPRIGHT
        3  -> NET_WM_MOVERESIZE_SIZE_RIGHT
        4  -> NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT
        5  -> NET_WM_MOVERESIZE_SIZE_BOTTOM
        6  -> NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT
        7  -> NET_WM_MOVERESIZE_SIZE_LEFT
        8  -> NET_WM_MOVERESIZE_MOVE
        9  -> NET_WM_MOVERESIZE_SIZE_KEYBOARD
        10 -> NET_WM_MOVERESIZE_MOVE_KEYBOARD
        11 -> NET_WM_MOVERESIZE_CANCEL
        _ -> error "NET_DESKTOP_LAYOUT_STARTING_CORNER: no such bit"

data NET_WM_STATE_ACTION = NET_WM_STATE_REMOVE
                         | NET_WM_STATE_ADD
                         | NET_WM_STATE_TOGGLE
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance BitEnum NET_WM_STATE_ACTION where
    toBit v = case v of
        NET_WM_STATE_REMOVE -> 0
        NET_WM_STATE_ADD    -> 1
        NET_WM_STATE_TOGGLE -> 2

    fromBit v = case v of
        0 -> NET_WM_STATE_REMOVE
        1 -> NET_WM_STATE_ADD
        2 -> NET_WM_STATE_TOGGLE
        _ -> error "NET_WM_STATE_ACTION: no such bit"

data SOURCE_INDICATION = SOURCE_NONE
                       | SOURCE_APPLICATION
                       | SOURCE_PAGER
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance BitEnum SOURCE_INDICATION where
    toBit v = case v of
        SOURCE_NONE        -> 0
        SOURCE_APPLICATION -> 1
        SOURCE_PAGER       -> 2

    fromBit v = case v of
        0 -> SOURCE_NONE
        1 -> SOURCE_APPLICATION
        2 -> SOURCE_PAGER
        _ -> error "SOURCE_INDICATION: no such bit"

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
        f -> error $ "NET_MOVERESIZE_WINDOW_FLAG: no bit for " ++ show f

    -- fromBit :: Int -> a
    fromBit i = case i of
        8 ->  NET_MOVERESIZE_WINDOW_X
        9 ->  NET_MOVERESIZE_WINDOW_Y
        10 -> NET_MOVERESIZE_WINDOW_WIDTH
        11 -> NET_MOVERESIZE_WINDOW_HEIGHT
        12 -> NET_MOVERESIZE_WINDOW_SOURCE SOURCE_APPLICATION
        13 -> NET_MOVERESIZE_WINDOW_SOURCE SOURCE_PAGER
        b  -> error $ "NET_MOVERESIZE_WINDOW_FLAG: no such bit: " ++ show b
