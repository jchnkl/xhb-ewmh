{-# LANGUAGE DeriveDataTypeable #-}

module Graphics.XHB.Icccm.Types where

import Data.Typeable (Typeable)
import Graphics.XHB (SimpleEnum(..), BitEnum(..))

data WindowGravity = WindowGravityNorthWest
                   | WindowGravityNorth
                   | WindowGravityNorthEast
                   | WindowGravityWest
                   | WindowGravityCenter
                   | WindowGravityEast
                   | WindowGravitySouthWest
                   | WindowGravitySouth
                   | WindowGravitySouthEast
                   | WindowGravityStatic
    deriving (Eq, Ord, Read, Show, Typeable)

instance SimpleEnum WindowGravity where
    toValue g = case g of
        WindowGravityNorthWest -> 1
        WindowGravityNorth     -> 2
        WindowGravityNorthEast -> 3
        WindowGravityWest      -> 4
        WindowGravityCenter    -> 5
        WindowGravityEast      -> 6
        WindowGravitySouthWest -> 7
        WindowGravitySouth     -> 8
        WindowGravitySouthEast -> 9
        WindowGravityStatic    -> 10

    fromValue n = case n of
        1  -> WindowGravityNorthWest
        2  -> WindowGravityNorth
        3  -> WindowGravityNorthEast
        4  -> WindowGravityWest
        5  -> WindowGravityCenter
        6  -> WindowGravityEast
        7  -> WindowGravitySouthWest
        8  -> WindowGravitySouth
        9  -> WindowGravitySouthEast
        10 -> WindowGravityStatic
        _  -> error "no such Gravity"

data SIZE_HINTS_FLAGS = UsPosition
                      | UsSize
                      | PPosition
                      | PSize
                      | PMinSize
                      | PMaxSize
                      | PResizeInc
                      | PAspect
                      | BaseSize
                      | PWinGravity
    deriving (Eq, Ord, Read, Show, Typeable)

instance BitEnum SIZE_HINTS_FLAGS where
    toBit b = case b of
        UsPosition  -> 0
        UsSize      -> 1
        PPosition   -> 2
        PSize       -> 3
        PMinSize    -> 4
        PMaxSize    -> 5
        PResizeInc  -> 6
        PAspect     -> 7
        BaseSize    -> 8
        PWinGravity -> 9

    fromBit i = case i of
        0 -> UsPosition
        1 -> UsSize
        2 -> PPosition
        3 -> PSize
        4 -> PMinSize
        5 -> PMaxSize
        6 -> PResizeInc
        7 -> PAspect
        8 -> BaseSize
        9 -> PWinGravity
        _ -> error "no such SIZE_HINTS_FLAGS"

data WM_NORMAL_HINTS = WM_NORMAL_HINTS
    { flags          :: [SIZE_HINTS_FLAGS]
    , usPosition     :: Maybe (Int, Int)
    , usSize         :: Maybe (Int, Int)
    , minSize        :: Maybe (Int, Int)
    , maxSize        :: Maybe (Int, Int)
    , resizeInc      :: Maybe (Int, Int)
    , minAspect      :: Maybe (Int, Int)
    , maxAspect      :: Maybe (Int, Int)
    , baseSize       :: Maybe (Int, Int)
    , win_gravity    :: Maybe WindowGravity
    }
    deriving (Eq, Ord, Read, Show, Typeable)
