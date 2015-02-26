{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.XHB.Ewmh.Error
    ( EwmhErrorCtx
    , runEwmhT
    , getNetSupported
    , getNetClientList
    , getNetClientListStacking
    , getNetNumberOfDesktops
    , getNetDesktopGeometry
    , getNetDesktopViewport
    , getNetCurrentDesktop
    , getNetDesktopNames
    , getActiveWindow
    , getNetWorkarea
    , getNetSupportingWmCheck
    , getNetVirtualRoots
    , getNetDesktopLayout
    , getNetShowingDesktop
    , getNetWmName
    , getNetWmVisibleName
    , getNetWmIconName
    , getNetWmVisibleIconName
    , getNetWmDesktop
    , getNetWmWindowType
    , getNetWmState
    , getNetWmAllowedActions
    , getNetWmStrut
    , getNetWmStrutPartial
    , getNetWmIconGeometry
    , getNetWmIcon
    , getNetWmPID
    , getNetWmHandledIcons
    , getNetWmUserTime
    , getNetWmUserTimeWindow
    , getNetFrameExtents
    , getNetWmOpaqueRegion
    , getNetWmBypassCompositor
    ) where

import Control.Applicative (Applicative)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word32)
import Graphics.XHB (Connection, SomeError, WINDOW)
import Graphics.XHB.Ewmh.Atoms
import Graphics.XHB.Ewmh.Types
import qualified Graphics.XHB.Ewmh as E

type EwmhErrorCtx m = (MonadError SomeError m, EwmhCtx m)

eitherToError :: MonadError e m => Either e a -> m a
eitherToError (Left e)  = throwError e
eitherToError (Right a) = return a

runEwmhT :: (MonadError SomeError m, MonadIO m, Applicative m)
         => Connection -> EwmhT m a -> m a
runEwmhT c m = E.runEwmhT c m >>= eitherToError

----------------------------
-- Root Window Properties --
----------------------------

getNetSupported :: EwmhErrorCtx m => Connection -> m NetSupported
getNetSupported c = E.getNetSupported c >>= eitherToError

getNetClientList :: EwmhErrorCtx m => Connection -> m [WINDOW]
getNetClientList c = E.getNetClientList c >>= eitherToError

getNetClientListStacking :: EwmhErrorCtx m => Connection -> m [WINDOW]
getNetClientListStacking c = E.getNetClientListStacking c >>= eitherToError

getNetNumberOfDesktops :: EwmhErrorCtx m => Connection -> m Word32
getNetNumberOfDesktops c = E.getNetNumberOfDesktops c >>= eitherToError

getNetDesktopGeometry :: EwmhErrorCtx m => Connection -> m NetDesktopGeometry
getNetDesktopGeometry c = E.getNetDesktopGeometry c >>= eitherToError

getNetDesktopViewport :: EwmhErrorCtx m => Connection -> m NetDesktopViewport
getNetDesktopViewport c = E.getNetDesktopViewport c >>= eitherToError

getNetCurrentDesktop :: EwmhErrorCtx m => Connection -> m Word32
getNetCurrentDesktop c = E.getNetCurrentDesktop c >>= eitherToError

getNetDesktopNames :: EwmhErrorCtx m => Connection -> m [String]
getNetDesktopNames c = E.getNetDesktopNames c >>= eitherToError

getActiveWindow :: EwmhErrorCtx m => Connection -> m WINDOW
getActiveWindow c = E.getActiveWindow c >>= eitherToError

getNetWorkarea :: EwmhErrorCtx m => Connection -> m NetWorkarea
getNetWorkarea c = E.getNetWorkarea c >>= eitherToError

getNetSupportingWmCheck :: EwmhErrorCtx m => Connection -> m WINDOW
getNetSupportingWmCheck c = E.getNetSupportingWmCheck c >>= eitherToError

getNetVirtualRoots :: EwmhErrorCtx m => Connection -> m [WINDOW]
getNetVirtualRoots c = E.getNetVirtualRoots c >>= eitherToError

getNetDesktopLayout :: EwmhErrorCtx m => Connection -> m NetDesktopLayout
getNetDesktopLayout c = E.getNetDesktopLayout c >>= eitherToError

getNetShowingDesktop :: EwmhErrorCtx m => Connection -> m Word32
getNetShowingDesktop c = E.getNetShowingDesktop c >>= eitherToError

-----------------------------------
-- Application Window Properties --
-----------------------------------

getNetWmName :: EwmhErrorCtx m => Connection -> WINDOW -> m [String]
getNetWmName c w = E.getNetWmName c w >>= eitherToError

getNetWmVisibleName :: EwmhErrorCtx m => Connection -> WINDOW -> m [String]
getNetWmVisibleName c w = E.getNetWmVisibleName c w >>= eitherToError

getNetWmIconName :: EwmhErrorCtx m => Connection -> WINDOW -> m [String]
getNetWmIconName c w = E.getNetWmIconName c w >>= eitherToError

getNetWmVisibleIconName :: EwmhErrorCtx m => Connection -> WINDOW -> m [String]
getNetWmVisibleIconName c w = E.getNetWmVisibleIconName c w >>= eitherToError

getNetWmDesktop :: EwmhErrorCtx m => Connection -> WINDOW -> m Word32
getNetWmDesktop c w = E.getNetWmDesktop c w >>= eitherToError

getNetWmWindowType :: EwmhErrorCtx m => Connection -> WINDOW -> m [NET_WM_WINDOW_TYPE]
getNetWmWindowType c w = E.getNetWmWindowType c w >>= eitherToError

getNetWmState :: EwmhErrorCtx m => Connection -> WINDOW -> m [NET_WM_STATE]
getNetWmState c w = E.getNetWmState c w >>= eitherToError

getNetWmAllowedActions :: EwmhErrorCtx m => Connection -> WINDOW -> m [NET_WM_ALLOWED_ACTIONS]
getNetWmAllowedActions c w = E.getNetWmAllowedActions c w >>= eitherToError

getNetWmStrut :: EwmhErrorCtx m => Connection -> WINDOW -> m NetWmStrut
getNetWmStrut c w = E.getNetWmStrut c w >>= eitherToError

getNetWmStrutPartial :: EwmhErrorCtx m => Connection -> WINDOW -> m NetWmStrutPartial
getNetWmStrutPartial c w = E.getNetWmStrutPartial c w >>= eitherToError

getNetWmIconGeometry :: EwmhErrorCtx m => Connection -> WINDOW -> m NetWmIconGeometry
getNetWmIconGeometry c w = E.getNetWmIconGeometry c w >>= eitherToError

getNetWmIcon :: EwmhErrorCtx m => Connection -> WINDOW -> m NetWmIcon
getNetWmIcon c w = E.getNetWmIcon c w >>= eitherToError

getNetWmPID :: EwmhErrorCtx m => Connection -> WINDOW -> m Word32
getNetWmPID c w = E.getNetWmPID c w >>= eitherToError

getNetWmHandledIcons :: EwmhErrorCtx m => Connection -> WINDOW -> m Word32
getNetWmHandledIcons c w = E.getNetWmHandledIcons c w >>= eitherToError

getNetWmUserTime :: EwmhErrorCtx m => Connection -> WINDOW -> m Word32
getNetWmUserTime c w = E.getNetWmUserTime c w >>= eitherToError

getNetWmUserTimeWindow :: EwmhErrorCtx m => Connection -> WINDOW -> m WINDOW
getNetWmUserTimeWindow c w = E.getNetWmUserTimeWindow c w >>= eitherToError

getNetFrameExtents :: EwmhErrorCtx m => Connection -> WINDOW -> m NetFrameExtents
getNetFrameExtents c w = E.getNetFrameExtents c w >>= eitherToError

getNetWmOpaqueRegion :: EwmhErrorCtx m => Connection -> WINDOW -> m NetWmOpaqueRegion
getNetWmOpaqueRegion c w = E.getNetWmOpaqueRegion c w >>= eitherToError

getNetWmBypassCompositor :: EwmhErrorCtx m => Connection -> WINDOW -> m Word32
getNetWmBypassCompositor c w = E.getNetWmBypassCompositor c w >>= eitherToError
