{-# LANGUAGE DeriveDataTypeable #-}

module Graphics.XHB.Ewmh.Values where

import Data.Typeable (Typeable)
import Data.Binary.Get (getWord32host)
import Data.Binary.Put (putWord32host)
import Graphics.XHB (SimpleEnum(..))
import Graphics.XHB.Ewmh.Serialize

data Gravity = WinGravity
             | NorthWest
             | North
             | NorthEast
             | West
             | Center
             | East
             | SouthWest
             | South
             | SouthEast
             | Static
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance SimpleEnum Gravity where
    toValue v = case v of
        WinGravity -> 0
        NorthWest  -> 1
        North      -> 2
        NorthEast  -> 3
        West       -> 4
        Center     -> 5
        East       -> 6
        SouthWest  -> 7
        South      -> 8
        SouthEast  -> 9
        Static     -> 10

    fromValue v = case v of
        0  -> WinGravity
        1  -> NorthWest
        2  -> North
        3  -> NorthEast
        4  -> West
        5  -> Center
        6  -> East
        7  -> SouthWest
        8  -> South
        9  -> SouthEast
        10 -> Static
        _  -> error "Gravity: no such value"

data SourceIndication = SourceNone
                      | SourceApplication
                      | SourcePager
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance SimpleEnum SourceIndication where
    toValue v = case v of
        SourceNone        -> 0
        SourceApplication -> 1
        SourcePager       -> 2

    fromValue v = case v of
        0 -> SourceNone
        1 -> SourceApplication
        2 -> SourcePager
        _ -> error "SourceIndication: no such value"

data NET_DESKTOP_LAYOUT_ORIENTATION = NET_WM_ORIENTATION_HORZ
                                    | NET_WM_ORIENTATION_VERT
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Serialize NET_DESKTOP_LAYOUT_ORIENTATION where
    serialize   = putWord32host . toValue
    deserialize = fmap fromValue getWord32host

instance SimpleEnum NET_DESKTOP_LAYOUT_ORIENTATION where
    toValue v = case v of
        NET_WM_ORIENTATION_HORZ -> 0
        NET_WM_ORIENTATION_VERT -> 1

    fromValue v = case v of
        0 -> NET_WM_ORIENTATION_HORZ
        1 -> NET_WM_ORIENTATION_VERT
        _ -> error "NET_DESKTOP_LAYOUT_ORIENTATION: no such value"

data NET_DESKTOP_LAYOUT_STARTING_CORNER = NET_WM_TOPLEFT
                                        | NET_WM_TOPRIGHT
                                        | NET_WM_BOTTOMRIGHT
                                        | NET_WM_BOTTOMLEFT
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance Serialize NET_DESKTOP_LAYOUT_STARTING_CORNER where
    serialize   = putWord32host . toValue
    deserialize = fmap fromValue getWord32host

instance SimpleEnum NET_DESKTOP_LAYOUT_STARTING_CORNER where
    toValue v = case v of
        NET_WM_TOPLEFT     -> 0
        NET_WM_TOPRIGHT    -> 1
        NET_WM_BOTTOMRIGHT -> 2
        NET_WM_BOTTOMLEFT  -> 3

    fromValue v = case v of
        0 -> NET_WM_TOPLEFT
        1 -> NET_WM_TOPRIGHT
        2 -> NET_WM_BOTTOMRIGHT
        3 -> NET_WM_BOTTOMLEFT
        _ -> error "NET_DESKTOP_LAYOUT_STARTING_CORNER: no such value"

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
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance SimpleEnum NET_WM_MOVERESIZE_DIRECTION where
    toValue v = case v of
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

    fromValue v = case v of
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
        _ -> error "NET_WM_MOVERESIZE_DIRECTION: no such value"

data NET_WM_STATE_ACTION = NET_WM_STATE_REMOVE
                         | NET_WM_STATE_ADD
                         | NET_WM_STATE_TOGGLE
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

instance SimpleEnum NET_WM_STATE_ACTION where
    toValue v = case v of
        NET_WM_STATE_REMOVE -> 0
        NET_WM_STATE_ADD    -> 1
        NET_WM_STATE_TOGGLE -> 2

    fromValue v = case v of
        0 -> NET_WM_STATE_REMOVE
        1 -> NET_WM_STATE_ADD
        2 -> NET_WM_STATE_TOGGLE
        _ -> error "NET_WM_STATE_ACTION: no such value"
