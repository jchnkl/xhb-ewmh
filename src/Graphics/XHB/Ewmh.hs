{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.XHB.Ewmh
    ( runEwmhT
    , getNetSupported
    , setNetSupported
    , getNetClientList
    , setNetClientList
    , getNetClientListStacking
    , setNetClientListStacking
    , getNetNumberOfDesktops
    , setNetNumberOfDesktops
    , requestNetNumberOfDesktops
    , getNetDesktopGeometry
    , setNetDesktopGeometry
    , requestNetDesktopGeometry
    , getNetDesktopViewport
    , setNetDesktopViewport
    , requestNetDesktopViewport
    , getNetCurrentDesktop
    , setNetCurrentDesktop
    , requestNetCurrentDesktop
    , getNetDesktopNames
    , setNetDesktopNames
    , getActiveWindow
    , setActiveWindow
    , requestNetActiveWindow
    , getNetWorkarea
    , setNetWorkarea
    , getNetSupportingWmCheck
    , setNetSupportingWmCheck
    , getNetVirtualRoots
    , setNetVirtualRoots
    , getNetDesktopLayout
    , setNetDesktopLayout
    , getNetShowingDesktop
    , setNetShowingDesktop
    , requestNetShowingDesktop
    , requestNetCloseWindow
    , requestNetMoveresizeWindow
    , requestNetWmMoveresize
    , requestNetRestackWindow
    , requestNetRequestFrameExtents
    , getNetWmName
    , setNetWmName
    , getNetWmVisibleName
    , setNetWmVisibleName
    , getNetWmIconName
    , setNetWmIconName
    , getNetWmVisibleIconName
    , setNetWmVisibleIconName
    , getNetWmDesktop
    , setNetWmDesktop
    , requestNetWmDesktop
    , getNetWmWindowType
    , setNetWmWindowType
    , getNetWmState
    , setNetWmState
    , requestNetWmState
    , getNetWmAllowedActions
    , setNetWmAllowedActions
    , getNetWmStrut
    , setNetWmStrut
    , getNetWmStrutPartial
    , setNetWmStrutPartial
    , getNetWmIconGeometry
    , setNetWmIconGeometry
    , getNetWmIcon
    , setNetWmIcon
    , getNetWmPID
    , setNetWmPID
    , getNetWmHandledIcons
    , setNetWmHandledIcons
    , getNetWmUserTime
    , setNetWmUserTime
    , getNetWmUserTimeWindow
    , setNetWmUserTimeWindow
    , getNetFrameExtents
    , setNetFrameExtents
    , getNetWmOpaqueRegion
    , setNetWmOpaqueRegion
    , getNetWmBypassCompositor
    , setNetWmBypassCompositor
    , requestNetWmPing
    , requestNetWmSyncRequest
    , requestNetWmFullscreenMonitors
    ) where

import Data.Bits ((.|.), shiftL)
import Data.Word (Word32)
import Data.Maybe (isJust, catMaybes, fromMaybe)
import Control.Monad (join, void)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.ByteString.Lazy.Char8 (pack)

import Foreign.C (CChar(..))
import Graphics.XHB (Connection, SomeError, WINDOW, ATOM, XidLike, Atom(..))
import Graphics.XHB (GetProperty(..), ChangeProperty(..), Window(..))
import Graphics.XHB (SendEvent(..), ClientMessageEvent(..), ClientMessageData(..))
import Graphics.XHB (PropMode(..), EventMask(..), Time(..), UnknownError(..))
import qualified Graphics.XHB as X
import Graphics.XHB.AtomCache
import Graphics.XHB.Ewmh.Values
import Graphics.XHB.Ewmh.Atoms
import Graphics.XHB.Ewmh.Types
import Graphics.XHB.Ewmh.Serialize

instance XidLike Atom where
    toXid a = X.toXid (X.toValue a :: Word32)
    fromXid a = X.fromValue (X.fromXid a :: Word32)

class PropertyType t where
    toPropertyType :: AtomCacheCtx m => t -> m ATOM

instance PropertyType Atom where
    toPropertyType = return . X.fromXid . X.toXid

instance PropertyType UTF8_STRING where
    toPropertyType = unsafeLookupATOM

type Prop p t r m = (AtomLike p, PropertyType t, Serialize r, EwmhCtx m)

type Request p d m = (AtomLike p, Serialize d, EwmhCtx m)

eitherToExcept :: Monad m => Either e a -> ExceptT e m a
eitherToExcept = ExceptT . return

runEwmhT :: (MonadIO m, Applicative m)
         => Connection -> EwmhT m a -> m (Either SomeError a)
runEwmhT c = runAtomCacheT
    . fmap (join . join . join . join)
    . seedAtoms c utf8
    . seedAtoms c ewmh
    . seedAtoms c states
    . seedAtoms c actions
    . seedAtoms c types
    where
    utf8    = [UTF8_STRING]
    ewmh    = [NET_SUPPORTED .. NET_WM_FULL_PLACEMENT]
    states  = [NET_WM_STATE_MODAL .. NET_WM_STATE_FOCUSED]
    actions = [NET_WM_ACTION_MOVE .. NET_WM_ACTION_BELOW]
    types   = [NET_WM_WINDOW_TYPE_DESKTOP .. NET_WM_WINDOW_TYPE_NORMAL]

getProp :: Prop p t r m => Connection -> WINDOW -> p -> t -> m (Either SomeError r)
getProp c w p t = runExceptT $ do
    ap <- unsafeLookupATOM p
    at <- toPropertyType t
    eitherToExcept
        =<< fmap fromReply . eitherToExcept
        =<< getPropertyReply (request ap at)
    where
    fromReply r = case fromBytes (X.value_GetPropertyReply r) of
        Right a -> Right a
        Left  e -> Left . X.toError . UnknownError . pack $ e
    getPropertyReply req = liftIO $ X.getProperty c req >>= X.getReply
    request ap at = MkGetProperty
        { delete_GetProperty = False
        , window_GetProperty = w
        , property_GetProperty = ap
        , type_GetProperty = at
        , long_offset_GetProperty = 0
        , long_length_GetProperty = maxBound
        }

setProp :: Prop p t r m => Connection -> WINDOW -> p -> t -> r -> m ()
setProp c w p t r = do
    ap <- unsafeLookupATOM p
    at <- toPropertyType t
    liftIO . X.changeProperty c $ request ap at
    where
    values = toBytes r
    request ap at = MkChangeProperty
        { mode_ChangeProperty = PropModeReplace
        , window_ChangeProperty = w
        , property_ChangeProperty = ap
        , type_ChangeProperty = at
        , format_ChangeProperty = 8
        , data_len_ChangeProperty = fromIntegral $ length values
        , data_ChangeProperty = values
        }

getRootProp :: Prop p t r m => Connection -> p -> t -> m (Either SomeError r)
getRootProp c = getProp c (X.getRoot c)

setRootProp :: Prop p t r m => Connection -> p -> t -> r -> m ()
setRootProp c = setProp c (X.getRoot c)

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

-- | Send an Ewmh request for `WINDOW` to the root window
sendRequest :: Request p d m => Connection -> WINDOW -> p -> d -> m ()
sendRequest c w a d = void . runMaybeT $ lookupATOM a >>= hoistMaybe >>= send
    where
    send = liftIO . X.sendEvent c . request (X.getRoot c) . serializeEvent

    serializeEvent = map (CChar . fromIntegral) . toBytes . event

    event typ = MkClientMessageEvent
        { format_ClientMessageEvent = 32
        , window_ClientMessageEvent = w
        , type_ClientMessageEvent = typ
        , data_ClientMessageEvent = ClientData8 $ toBytes d
        }

    request win raw = MkSendEvent
        { propagate_SendEvent = False
        , destination_SendEvent = win
        , event_mask_SendEvent = [ EventMaskSubstructureNotify
                                 , EventMaskSubstructureRedirect
                                 ]
        , event_SendEvent = raw
        }

----------------------------
-- Root Window Properties --
----------------------------

getNetSupported :: EwmhCtx m => Connection -> m (Either SomeError NetSupported)
getNetSupported c = runExceptT $ do
    atomids <- mapM lookupAtomId
        =<< eitherToExcept
        =<< getRootProp c NET_SUPPORTED AtomATOM
    return $ NetSupported (atoms atomids) (states atomids) (actions atomids) (types atomids)
    where
    -- yeah..
    atoms   = catMaybes . map fromAtom . catMaybes
    states  = catMaybes . map fromAtom . catMaybes
    actions = catMaybes . map fromAtom . catMaybes
    types   = catMaybes . map fromAtom . catMaybes

setNetSupported :: EwmhCtx m => Connection -> NetSupported -> m ()
setNetSupported c ns = do
    state     <- unsafeLookupATOM NET_WM_STATE
    types     <- unsafeLookupATOM NET_WM_WINDOW_TYPE
    actions   <- unsafeLookupATOM NET_WM_ALLOWED_ACTIONS

    atoms     <- mapM unsafeLookupATOM (ewmhAtoms ns)
    atoms'    <- insertAt state   atoms   <$> mapM unsafeLookupATOM (netWmStates ns)
    atoms''   <- insertAt types   atoms'  <$> mapM unsafeLookupATOM (netWmWindowTypes ns)
    atoms'''  <- insertAt actions atoms'' <$> mapM unsafeLookupATOM (netWmAllowedActions ns)

    setRootProp c NET_SUPPORTED AtomATOM atoms'''

    where
    insertAt :: Eq t => t -> [t] -> [t] -> [t]
    insertAt _ [] _      = []
    insertAt a (x:xs) as | a == x    = x : as ++ xs
                         | otherwise = x : insertAt a xs as

getNetClientList :: EwmhCtx m => Connection -> m (Either SomeError [WINDOW])
getNetClientList c = getRootProp c NET_CLIENT_LIST AtomWINDOW

setNetClientList :: EwmhCtx m => Connection -> [WINDOW] -> m ()
setNetClientList c = setRootProp c NET_CLIENT_LIST AtomWINDOW

getNetClientListStacking :: EwmhCtx m => Connection -> m (Either SomeError [WINDOW])
getNetClientListStacking c = getRootProp c NET_CLIENT_LIST_STACKING AtomWINDOW

setNetClientListStacking :: EwmhCtx m => Connection -> [WINDOW] -> m ()
setNetClientListStacking c = setRootProp c NET_CLIENT_LIST_STACKING AtomWINDOW

getNetNumberOfDesktops :: EwmhCtx m => Connection -> m (Either SomeError Word32)
getNetNumberOfDesktops c = getRootProp c NET_NUMBER_OF_DESKTOPS AtomCARDINAL

setNetNumberOfDesktops :: EwmhCtx m => Connection -> Word32 -> m ()
setNetNumberOfDesktops c = setRootProp c NET_NUMBER_OF_DESKTOPS AtomCARDINAL

requestNetNumberOfDesktops :: EwmhCtx m => Connection -> Word32 -> m ()
requestNetNumberOfDesktops c n = do
    sendRequest c none NET_NUMBER_OF_DESKTOPS [n]
    where none= X.fromXid $ X.toXid (X.toValue WindowNone :: Word32)

getNetDesktopGeometry :: EwmhCtx m => Connection -> m (Either SomeError NetDesktopGeometry)
getNetDesktopGeometry c = getRootProp c NET_DESKTOP_GEOMETRY AtomCARDINAL

setNetDesktopGeometry :: EwmhCtx m => Connection -> NetDesktopGeometry -> m ()
setNetDesktopGeometry c = setRootProp c NET_DESKTOP_GEOMETRY AtomCARDINAL

requestNetDesktopGeometry :: EwmhCtx m => Connection -> NetDesktopGeometry -> m ()
requestNetDesktopGeometry c (NetDesktopGeometry w h) = do
    sendRequest c none NET_DESKTOP_GEOMETRY [w, h]
    where none = X.fromXid $ X.toXid (X.toValue WindowNone :: Word32)

getNetDesktopViewport :: EwmhCtx m => Connection -> m (Either SomeError NetDesktopViewport)
getNetDesktopViewport c = getRootProp c NET_DESKTOP_VIEWPORT AtomCARDINAL

setNetDesktopViewport :: EwmhCtx m => Connection -> NetDesktopViewport -> m ()
setNetDesktopViewport c = setRootProp c NET_DESKTOP_VIEWPORT AtomCARDINAL

requestNetDesktopViewport :: EwmhCtx m => Connection -> Viewport -> m ()
requestNetDesktopViewport c (Viewport x y) = do
    sendRequest c none NET_DESKTOP_VIEWPORT [x, y]
    where none = X.fromXid $ X.toXid (X.toValue WindowNone :: Word32)

getNetCurrentDesktop :: EwmhCtx m => Connection -> m (Either SomeError Word32)
getNetCurrentDesktop c = getRootProp c NET_CURRENT_DESKTOP AtomCARDINAL

setNetCurrentDesktop :: EwmhCtx m => Connection -> Word32 -> m ()
setNetCurrentDesktop c = setRootProp c NET_CURRENT_DESKTOP AtomCARDINAL

requestNetCurrentDesktop :: EwmhCtx m => Connection -> Word32 -> m ()
requestNetCurrentDesktop c v = do
    sendRequest c none NET_CURRENT_DESKTOP [v, X.toValue TimeCurrentTime]
    where none = X.fromXid $ X.toXid (X.toValue WindowNone :: Word32)

getNetDesktopNames :: EwmhCtx m => Connection -> m (Either SomeError [String])
getNetDesktopNames c = getProp c (X.getRoot c) NET_DESKTOP_NAMES UTF8_STRING

setNetDesktopNames :: EwmhCtx m => Connection -> [String] -> m ()
setNetDesktopNames c = setRootProp c NET_DESKTOP_NAMES UTF8_STRING

getActiveWindow :: EwmhCtx m => Connection -> m (Either SomeError WINDOW)
getActiveWindow c = getRootProp c NET_ACTIVE_WINDOW AtomWINDOW

setActiveWindow :: EwmhCtx m => Connection -> WINDOW -> m ()
setActiveWindow c = setRootProp c NET_ACTIVE_WINDOW AtomWINDOW

requestNetActiveWindow :: EwmhCtx m => Connection -> NetActiveWindow -> m ()
requestNetActiveWindow c (NetActiveWindow src mw) = do
    sendRequest c none NET_ACTIVE_WINDOW values
    where none = X.fromXid $ X.toXid (X.toValue WindowNone :: Word32)
          values = [ X.toValue src
                   , X.toValue TimeCurrentTime
                   , maybe 0 (X.fromXid . X.toXid) mw
                   ] :: [Word32]

getNetWorkarea :: EwmhCtx m => Connection -> m (Either SomeError NetWorkarea)
getNetWorkarea c = getRootProp c NET_WORKAREA AtomCARDINAL

setNetWorkarea :: EwmhCtx m => Connection -> NetWorkarea -> m ()
setNetWorkarea c = setRootProp c NET_WORKAREA AtomCARDINAL

getNetSupportingWmCheck :: EwmhCtx m => Connection -> m (Either SomeError WINDOW)
getNetSupportingWmCheck c = getRootProp c NET_SUPPORTING_WM_CHECK AtomWINDOW

setNetSupportingWmCheck :: EwmhCtx m => Connection -> WINDOW -> m ()
setNetSupportingWmCheck c = setRootProp c NET_SUPPORTING_WM_CHECK AtomWINDOW

getNetVirtualRoots :: EwmhCtx m => Connection -> m (Either SomeError [WINDOW])
getNetVirtualRoots c = getRootProp c NET_VIRTUAL_ROOTS AtomWINDOW

setNetVirtualRoots :: EwmhCtx m => Connection -> [WINDOW] -> m ()
setNetVirtualRoots c = setRootProp c NET_VIRTUAL_ROOTS AtomWINDOW

getNetDesktopLayout :: EwmhCtx m => Connection -> m (Either SomeError NetDesktopLayout)
getNetDesktopLayout conn = getRootProp conn NET_DESKTOP_LAYOUT AtomCARDINAL

setNetDesktopLayout :: EwmhCtx m => Connection -> NetDesktopLayout -> m ()
setNetDesktopLayout c = setRootProp c NET_DESKTOP_LAYOUT AtomCARDINAL

getNetShowingDesktop :: EwmhCtx m => Connection -> m (Either SomeError Word32)
getNetShowingDesktop c = getRootProp c NET_SHOWING_DESKTOP AtomCARDINAL

setNetShowingDesktop :: EwmhCtx m => Connection -> Word32 -> m ()
setNetShowingDesktop c = setRootProp c NET_SHOWING_DESKTOP AtomCARDINAL

requestNetShowingDesktop :: EwmhCtx m => Connection -> Bool -> m ()
requestNetShowingDesktop c b = sendRequest c none NET_SHOWING_DESKTOP [fromEnum b]
    where none = X.fromXid $ X.toXid (X.toValue WindowNone :: Word32)

--------------------------------
-- Other Root Window Messages --
--------------------------------

requestNetCloseWindow :: EwmhCtx m => Connection -> WINDOW -> SourceIndication -> m ()
requestNetCloseWindow c w si = do
    sendRequest c w NET_CLOSE_WINDOW ([X.toValue TimeCurrentTime, X.toValue si] :: [Word32])

requestNetMoveresizeWindow :: EwmhCtx m
                           => Connection -> WINDOW -> NetMoveresizeWindow -> m ()
requestNetMoveresizeWindow c w mr = sendRequest c w NET_MOVERESIZE_WINDOW values
    where
    x      = netMoveresizeWindow_x mr
    y      = netMoveresizeWindow_y mr
    width  = fromIntegral <$> netMoveresizeWindow_width mr
    height = fromIntegral <$> netMoveresizeWindow_height mr

    sourceIndicationBit = case netMoveresizeWindow_source_indication mr of
        SourceApplication -> 12
        SourcePager       -> 13
        _                 -> 0

    gravityBit          = X.toValue $ netMoveresizeWindow_gravity mr
    xBit                = if isJust x      then shiftL 1 8  else 0
    yBit                = if isJust y      then shiftL 1 9  else 0
    widthBit            = if isJust width  then shiftL 1 10 else 0
    heightBit           = if isJust height then shiftL 1 11 else 0

    flags = foldr (.|.) 0 [gravityBit, xBit, yBit, widthBit, heightBit, sourceIndicationBit]

    values = [flags, fromMaybe 0 x, fromMaybe 0 y, fromMaybe 0 width, fromMaybe 0 height]

requestNetWmMoveresize :: EwmhCtx m => Connection -> WINDOW -> NetWmMoveresize -> m ()
requestNetWmMoveresize c w mr = do
    sendRequest c w NET_WM_MOVERESIZE [x_root, y_root, direction, button, sourceIndication]
    where
    x_root           = fromMaybe 0 $ netWmMoveresize_x_root mr
    y_root           = fromMaybe 0 $ netWmMoveresize_y_root mr
    direction        = X.toValue $ netWmMoveresize_direction mr
    button           = X.toValue $ netWmMoveresize_button mr
    sourceIndication = X.toValue $ netWmMoveresize_source_indication mr

requestNetRestackWindow :: EwmhCtx m => Connection -> WINDOW -> NetRestackWindow -> m ()
requestNetRestackWindow c w rw = do
    sendRequest c w NET_RESTACK_WINDOW ([sourceIndication, sibling_window, detail] :: [Word32])
    where
    sourceIndication = X.toValue $ netRestackWindow_source_indication rw
    sibling_window   = X.fromXid . X.toXid $ netRestackWindow_sibling_window rw
    detail           = X.toValue $ netRestackWindow_detail rw

requestNetRequestFrameExtents :: EwmhCtx m => Connection -> WINDOW -> m ()
requestNetRequestFrameExtents c w = sendRequest c w NET_REQUEST_FRAME_EXTENTS ([] :: [Word32])

-----------------------------------
-- Application Window Properties --
-----------------------------------

getNetWmName :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError [String])
getNetWmName c w = getProp c w NET_WM_NAME UTF8_STRING

setNetWmName :: EwmhCtx m => Connection -> WINDOW -> [String] -> m ()
setNetWmName c w = setProp c w NET_WM_NAME UTF8_STRING

getNetWmVisibleName :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError [String])
getNetWmVisibleName c w = getProp c w NET_WM_VISIBLE_NAME UTF8_STRING

setNetWmVisibleName :: EwmhCtx m => Connection -> WINDOW -> [String] -> m ()
setNetWmVisibleName c w = setProp c w NET_WM_VISIBLE_NAME UTF8_STRING

getNetWmIconName :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError [String])
getNetWmIconName c w = getProp c w NET_WM_ICON_NAME UTF8_STRING

setNetWmIconName :: EwmhCtx m => Connection -> WINDOW -> [String] -> m ()
setNetWmIconName c w = setProp c w NET_WM_ICON_NAME UTF8_STRING

getNetWmVisibleIconName :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError [String])
getNetWmVisibleIconName c w = getProp c w NET_WM_VISIBLE_ICON_NAME UTF8_STRING

setNetWmVisibleIconName :: EwmhCtx m => Connection -> WINDOW -> [String] -> m ()
setNetWmVisibleIconName c w = setProp c w NET_WM_VISIBLE_ICON_NAME UTF8_STRING

getNetWmDesktop :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError Word32)
getNetWmDesktop c w = getProp c w NET_WM_DESKTOP AtomCARDINAL

setNetWmDesktop :: EwmhCtx m => Connection -> WINDOW -> Word32 -> m ()
setNetWmDesktop c w = setProp c w NET_WM_DESKTOP AtomCARDINAL

requestNetWmDesktop ::EwmhCtx m => Connection -> WINDOW -> NetWmDesktop -> m ()
requestNetWmDesktop c w d = sendRequest c w NET_WM_DESKTOP [desktop, source]
    where
    desktop = netWmDesktop_new_desktop d
    source  = X.toValue $ netWmDesktop_source_indication d

getNetWmWindowType :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError [NET_WM_WINDOW_TYPE])
getNetWmWindowType c w = runExceptT $ do
    getProp c w NET_WM_WINDOW_TYPE AtomATOM
        >>= eitherToExcept
        >>= fmap (catMaybes . map fromAtom . catMaybes) . mapM lookupAtomId

setNetWmWindowType :: EwmhCtx m => Connection -> WINDOW -> [NET_WM_WINDOW_TYPE] -> m ()
setNetWmWindowType c w vs = do
    mapM unsafeLookupATOM vs >>= setProp c w NET_WM_WINDOW_TYPE AtomATOM

getNetWmState :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError [NET_WM_STATE])
getNetWmState c w = runExceptT $ do
    getProp c w NET_WM_STATE AtomATOM
        >>= eitherToExcept
        >>= fmap (catMaybes . map fromAtom . catMaybes) . mapM lookupAtomId

setNetWmState :: EwmhCtx m => Connection -> WINDOW -> [NET_WM_STATE] -> m ()
setNetWmState c w vs = mapM unsafeLookupATOM vs >>= setProp c w NET_WM_STATE AtomATOM

requestNetWmState :: EwmhCtx m => Connection -> WINDOW -> NetWmState -> m ()
requestNetWmState c w v = do
    a1 <- p1
    a2 <- p2
    sendRequest c w NET_WM_STATE ([action, a1, a2, source] :: [Word32])
    where
    action = X.toValue . netWmState_action $ v
    source = X.toValue . netWmState_source_indication $ v
    p1 = fmap (X.fromXid . X.toXid) $ unsafeLookupATOM $ netWmState_first_property v
    p2 = case netWmState_second_property v of
        Nothing -> return 0
        Just n  -> fmap (X.fromXid . X.toXid) $ unsafeLookupATOM n

getNetWmAllowedActions :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError [NET_WM_ALLOWED_ACTIONS])
getNetWmAllowedActions c w = runExceptT $ do
    getProp c w NET_WM_ALLOWED_ACTIONS AtomATOM
        >>= eitherToExcept
        >>= fmap (catMaybes . map fromAtom . catMaybes) . mapM lookupAtomId

setNetWmAllowedActions :: EwmhCtx m => Connection -> WINDOW -> [NET_WM_ALLOWED_ACTIONS] -> m ()
setNetWmAllowedActions c w vs = mapM unsafeLookupATOM vs >>= setProp c w NET_WM_ALLOWED_ACTIONS AtomATOM

getNetWmStrut :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError NetWmStrut)
getNetWmStrut c w = getProp c w NET_WM_STRUT AtomCARDINAL

setNetWmStrut :: EwmhCtx m => Connection -> WINDOW -> NetWmStrut -> m ()
setNetWmStrut c w = setProp c w NET_WM_STRUT AtomCARDINAL

getNetWmStrutPartial :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError NetWmStrutPartial)
getNetWmStrutPartial c w = getProp c w NET_WM_STRUT_PARTIAL AtomCARDINAL

setNetWmStrutPartial :: EwmhCtx m => Connection -> WINDOW -> NetWmStrutPartial -> m ()
setNetWmStrutPartial c w = setProp c w NET_WM_STRUT_PARTIAL AtomCARDINAL

getNetWmIconGeometry :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError NetWmIconGeometry)
getNetWmIconGeometry c w = getProp c w NET_WM_ICON_GEOMETRY AtomCARDINAL

setNetWmIconGeometry :: EwmhCtx m => Connection -> WINDOW -> NetWmIconGeometry -> m ()
setNetWmIconGeometry c w = setProp c w NET_WM_ICON_GEOMETRY AtomCARDINAL

getNetWmIcon :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError NetWmIcon)
getNetWmIcon c w = getProp c w NET_WM_ICON AtomCARDINAL

setNetWmIcon :: EwmhCtx m => Connection -> WINDOW -> NetWmIcon -> m ()
setNetWmIcon c w = setProp c w NET_WM_ICON AtomCARDINAL

getNetWmPID :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError Word32)
getNetWmPID c w = getProp c w NET_WM_PID AtomCARDINAL

setNetWmPID :: EwmhCtx m => Connection -> WINDOW -> Word32 -> m ()
setNetWmPID c w = setProp c w NET_WM_PID AtomCARDINAL

getNetWmHandledIcons :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError Word32)
getNetWmHandledIcons c w = getProp c w NET_WM_HANDLED_ICONS AtomCARDINAL

setNetWmHandledIcons :: EwmhCtx m => Connection -> WINDOW -> Word32 -> m ()
setNetWmHandledIcons c w = setProp c w NET_WM_HANDLED_ICONS AtomCARDINAL

getNetWmUserTime :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError Word32)
getNetWmUserTime c w = getProp c w NET_WM_USER_TIME AtomCARDINAL

setNetWmUserTime :: EwmhCtx m => Connection -> WINDOW -> Word32 -> m ()
setNetWmUserTime c w = setProp c w NET_WM_USER_TIME AtomCARDINAL

getNetWmUserTimeWindow :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError WINDOW)
getNetWmUserTimeWindow c w = getProp c w NET_WM_USER_TIME_WINDOW AtomWINDOW

setNetWmUserTimeWindow :: EwmhCtx m => Connection -> WINDOW -> WINDOW -> m ()
setNetWmUserTimeWindow c w = setProp c w NET_WM_USER_TIME_WINDOW AtomWINDOW

getNetFrameExtents :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError NetFrameExtents)
getNetFrameExtents c w = getProp c w NET_FRAME_EXTENTS AtomCARDINAL

setNetFrameExtents :: EwmhCtx m => Connection -> WINDOW -> NetFrameExtents -> m ()
setNetFrameExtents c w = setProp c w NET_FRAME_EXTENTS AtomCARDINAL

getNetWmOpaqueRegion :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError NetWmOpaqueRegion)
getNetWmOpaqueRegion c w = getProp c w NET_WM_OPAQUE_REGION AtomCARDINAL

setNetWmOpaqueRegion :: EwmhCtx m => Connection -> WINDOW -> NetWmOpaqueRegion -> m ()
setNetWmOpaqueRegion c w = setProp c w NET_WM_OPAQUE_REGION AtomCARDINAL

getNetWmBypassCompositor :: EwmhCtx m => Connection -> WINDOW -> m (Either SomeError Word32)
getNetWmBypassCompositor c w = getProp c w NET_WM_BYPASS_COMPOSITOR AtomCARDINAL

setNetWmBypassCompositor :: EwmhCtx m => Connection -> WINDOW -> Word32 -> m ()
setNetWmBypassCompositor c w = setProp c w NET_WM_BYPASS_COMPOSITOR AtomCARDINAL

requestNetWmPing :: EwmhCtx m => Connection -> WINDOW -> m ()
requestNetWmPing c w = do
    unsafeLookupATOM NET_WM_PING >>= sendRequest c w WM_PROTOCOLS . values
    where values a = [ X.fromXid (X.toXid a)
                     , X.toValue TimeCurrentTime
                     , X.fromXid (X.toXid w)
                     ] :: [Word32]

requestNetWmSyncRequest :: EwmhCtx m => Connection -> WINDOW -> NetWmSyncRequest -> m ()
requestNetWmSyncRequest c w sr = do
    unsafeLookupATOM NET_WM_SYNC_REQUEST >>= sendRequest c w WM_PROTOCOLS . values
    where values a = [ X.fromXid (X.toXid a)
                     , X.toValue TimeCurrentTime
                     , netWmSyncRequest_low sr
                     , netWmSyncRequest_high sr
                     ]

requestNetWmFullscreenMonitors :: EwmhCtx m
                               => Connection -> WINDOW -> NetWmFullscreenMonitors -> m ()
requestNetWmFullscreenMonitors c w v = do
    sendRequest c w NET_WM_FULLSCREEN_MONITORS [top, bottom, left, right, source]
    where
    top    = netWmFullscreenMonitors_top v
    bottom = netWmFullscreenMonitors_bottom v
    left   = netWmFullscreenMonitors_left v
    right  = netWmFullscreenMonitors_right v
    source = X.toValue . netWmFullscreenMonitors_source_indication $ v
