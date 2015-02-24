{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Graphics.XHB.Ewmh.Basic
    where
    -- ( module Graphics.XHB.Ewmh.Types
    -- -- , Utf8String(..)
    -- , runEwmhT
    -- , atomToXidLike
    -- , simpleGetProperty
    -- , simpleChangeProperty
    -- -- , getString
    -- -- , getUtf8String
    -- -- , ewmhRequest
    -- -- , changeNetWmState
    -- -- , netActiveWindow
    -- -- , getNetActiveWindow
    -- -- , netRestackWindow
    -- -- , netMoveResizeWindow
    -- ) where

import qualified Data.HashMap.Lazy as M
import Data.Bits (Bits, (.|.), setBit, shiftL)
import Data.Char (chr, ord)
import Data.Word (Word8, Word32)
import Data.Maybe (isJust, catMaybes, fromMaybe)
import Control.Monad (join, void)
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)
import Control.Monad.State (gets)
import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Maybe (MaybeT(..))

import Foreign.C (CChar(..))
import Graphics.XHB (Connection, SomeError, WINDOW, ATOM, XidLike, Atom(..))
import Graphics.XHB (GetPropertyReply, GetProperty(..), ChangeProperty(..))
import Graphics.XHB (SendEvent(..), ClientMessageEvent(..), ClientMessageData(..))
import Graphics.XHB (PropMode(..), EventMask(..), UnknownError(..), Time(..))
import qualified Graphics.XHB as X
import Graphics.XHB.Atom
import Graphics.XHB.Ewmh.Bits
import Graphics.XHB.Ewmh.Atoms
import Graphics.XHB.Ewmh.Types
import Graphics.XHB.Ewmh.Serialize

type BasicEwmhCtx m = (Applicative m, MonadIO m, MonadEwmh m)

fe :: (Enum a, Integral b) => a -> b
fe = fromIntegral . fromEnum

(.=.) :: (Bits a) => a -> Int -> a
(.=.) = setBit

instance XidLike Atom where
    toXid a = X.toXid (X.toValue a :: Word32)
    fromXid a = X.fromValue (X.fromXid a :: Word32)

fromXidLike :: (XidLike a, XidLike b) => a -> b
fromXidLike = X.fromXid . X.toXid

atomToXidLike :: (XidLike a) => Atom -> a
atomToXidLike a = X.fromXid $ X.toXid (X.toValue a :: Word32)

bytesToString :: [Word8] -> String
bytesToString = map (chr . fromIntegral)

-- TODO: make type class
-- class ByteClass a where fromBytes ..
fromBytes :: [Word8] -> Maybe Word32
fromBytes bytes | length bytes == 4 = Just $ foldr accum 0 bytes
                | otherwise         = Nothing
    where accum o a = (a `shiftL` 8) .|. fromIntegral o

toWord :: (Word8, Word8, Word8, Word8) -> Word32
toWord (a,b,c,d) = fi a
               .|. fi b `shiftL` 8
               .|. fi c `shiftL` 16
               .|. fi d `shiftL` 24
    where fi = fromIntegral

toWords :: [Word8] -> [Word32]
toWords (a:b:c:d:rest) = toWord (a,b,c,d) : toWords rest
toWords _              = []

bytesToXidLike :: XidLike a => [Word8] -> Maybe a
bytesToXidLike = fmap fromXidLike . fromBytes

toXidLike :: XidLike a => GetPropertyReply -> Maybe a
toXidLike = fmap fromXidLike . fromBytes . value_GetPropertyReply

toXidLikeList :: XidLike a => GetPropertyReply -> [a]
toXidLikeList = map fromXidLike . toWords . X.value_GetPropertyReply

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delim = splitOn' [] []
    where
    splitOn' accum result [] = filter (not . null) $ map reverse $ accum:result
    splitOn' accum result (x:xs)
        | x == delim = splitOn' [] (accum:result) xs
        | otherwise  = splitOn' (x:accum) result xs

eitherToError :: MonadError e m => Either e a -> m a
eitherToError (Left  e) = throwError e
eitherToError (Right a) = return a

eitherToExcept :: Monad m => Either e a -> ExceptT e m a
eitherToExcept = ExceptT . return

toString :: [Word8] -> [String]
toString = map bytesToString . splitOn (fromIntegral (ord '\0') :: Word8)

class PropertyType a where
    propertyTypeAtom :: a -> Atom
    propertyTypeATOM :: MonadEwmh m => a -> m ATOM
    propertyTypeATOM = return . X.fromXid . X.toXid . propertyTypeAtom
    -- propertyTypeM :: MonadEwmh m => a -> m ATOM
    -- propertyTypeM = return . propertyType

instance PropertyType EWMH_ATOM where
    propertyTypeAtom a = case a of
        -- Root Window Properties
        NET_SUPPORTED -> AtomATOM
        NET_CLIENT_LIST -> AtomWINDOW
        NET_CLIENT_LIST_STACKING -> AtomWINDOW
        NET_NUMBER_OF_DESKTOPS -> AtomCARDINAL
        NET_DESKTOP_GEOMETRY -> AtomCARDINAL
        NET_DESKTOP_VIEWPORT -> AtomCARDINAL
        NET_CURRENT_DESKTOP -> AtomCARDINAL
        NET_ACTIVE_WINDOW -> AtomWINDOW
        NET_WORKAREA -> AtomCARDINAL
        NET_SUPPORTING_WM_CHECK -> AtomWINDOW
        NET_VIRTUAL_ROOTS -> AtomWINDOW
        NET_DESKTOP_LAYOUT -> AtomCARDINAL
        NET_SHOWING_DESKTOP -> AtomCARDINAL

        -- Other Root Window Messages
        NET_CLOSE_WINDOW -> error "no property type for NET_CLOSE_WINDOW"
        NET_MOVERESIZE_WINDOW -> error "no property type for NET_MOVERESIZE_WINDOW"
        NET_WM_MOVERESIZE -> error "no property type for NET_WM_MOVERESIZE"
        NET_RESTACK_WINDOW -> error "no property type for NET_RESTACK_WINDOW"
        NET_REQUEST_FRAME_EXTENTS -> error "no property type for NET_REQUEST_FRAME_EXTENTS"

        -- Application Window Property
        NET_WM_DESKTOP -> AtomCARDINAL
        NET_WM_WINDOW_TYPE -> AtomATOM
        NET_WM_STATE -> AtomATOM
        NET_WM_ALLOWED_ACTIONS -> AtomATOM
        NET_WM_STRUT -> AtomCARDINAL
        NET_WM_STRUT_PARTIAL -> AtomCARDINAL
        NET_WM_ICON_GEOMETRY -> AtomCARDINAL
        NET_WM_ICON -> AtomCARDINAL
        NET_WM_PID -> AtomCARDINAL
        NET_WM_HANDLED_ICONS -> error "no property type for NET_WM_HANDLED_ICONS"
        NET_WM_USER_TIME -> AtomCARDINAL
        NET_WM_USER_TIME_WINDOW -> AtomWINDOW
        NET_FRAME_EXTENTS -> AtomCARDINAL
        NET_WM_OPAQUE_REGION -> AtomCARDINAL
        NET_WM_BYPASS_COMPOSITOR -> AtomCARDINAL

        -- Window Manager Protocols
        NET_WM_PING -> error "no property type for NET_WM_PING"
        NET_WM_SYNC_REQUEST -> error "no property type for NET_WM_SYNC_REQUEST"
        NET_WM_SYNC_REQUEST_COUNTER -> error "no property type for NET_WM_SYNC_REQUEST_COUNTER"
        NET_WM_FULLSCREEN_MONITORS -> error "no property type for NET_WM_FULLSCREEN_MONITORS"

        -- Other Properties
        NET_WM_FULL_PLACEMENT -> error "no property type for NET_WM_FULL_PLACEMENT"

        _ -> error $ "no propertyTypeAtom for " ++ show a

    propertyTypeATOM a = case a of
        NET_DESKTOP_NAMES -> unsafeLookupATOM UTF8_STRING
        NET_WM_NAME -> unsafeLookupATOM UTF8_STRING
        NET_WM_VISIBLE_NAME -> unsafeLookupATOM UTF8_STRING
        NET_WM_ICON_NAME -> unsafeLookupATOM UTF8_STRING
        NET_WM_VISIBLE_ICON_NAME -> unsafeLookupATOM UTF8_STRING

        _ -> error $ "no propertyTypeATOM for " ++ show a


dump :: Monad m => AtomT m [AtomName]
dump = AtomT $ gets (map atomName . M.keys . fst)

runEwmhT :: (MonadIO m, Applicative m)
         => Connection -> EwmhT m a -> m (Either SomeError a)
runEwmhT c = runAtomT
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

simpleGetProperty :: MonadIO m
                  => Connection
                  -> WINDOW -- ^ Target window
                  -> ATOM -- ^ Property
                  -> ATOM -- ^ Property Type
                  -> m (Either SomeError GetPropertyReply)
simpleGetProperty c win prop prop_type = do
    liftIO $ X.getProperty c request >>= X.getReply
    where
    request = MkGetProperty
        { delete_GetProperty = False
        , window_GetProperty = win
        , property_GetProperty = prop
        , type_GetProperty = prop_type
        , long_offset_GetProperty = 0
        , long_length_GetProperty = maxBound
        }

simpleChangeProperty :: MonadIO m
                     => Connection
                     -> WINDOW -- ^ Target window
                     -> ATOM -- ^ Property to change
                     -> ATOM -- ^ Property Type
                     -> PropMode -- ^ Append, Prepend or Replace
                     -> [Word8] -- ^ values
                     -> m ()
simpleChangeProperty c window prop prop_type prop_mode values = do
    liftIO $ X.changeProperty c request
    where
    request = MkChangeProperty
        { mode_ChangeProperty = prop_mode
        , window_ChangeProperty = window
        , property_ChangeProperty = prop
        , type_ChangeProperty = prop_type
        , format_ChangeProperty = 8
        , data_len_ChangeProperty = fromIntegral $ length values
        , data_ChangeProperty = values
        }

getXid :: (AtomLike a, XidLike p, XidLike v, BasicEwmhCtx m)
       => Connection -> WINDOW -> a -> p -> m (Either SomeError v)
getXid c w al prop_type = runExceptT $ unsafeLookupATOM al >>= \a -> do
    simpleGetProperty c w a (fromXid . toXid $ prop_type)
        >>= fmap (fromBytes . value_GetPropertyReply) . eitherToExcept
        >>= eitherToExcept . maybe toLeft toRight
    where
    toLeft   = Left . toError $ UnknownError "getRootXid: no value"
    toRight  = Right . fromXidLike

setXid :: (AtomLike a, XidLike p, XidLike v, BasicEwmhCtx m)
       => Connection -> WINDOW -> a -> p -> v -> m ()
setXid c w al pt v = unsafeLookupATOM al >>= \a -> do
    simpleChangeProperty c w a type_ PropModeReplace $ toBytes value
    where type_ = X.fromXid (X.toXid pt)
          value = X.fromXid (X.toXid v) :: Word32

getRootXid :: (AtomLike a, XidLike p, XidLike v, BasicEwmhCtx m)
           => Connection -> a -> p -> m (Either SomeError v)
getRootXid c = getXid c (X.getRoot c)

setRootXid :: (AtomLike a, XidLike p, XidLike v, BasicEwmhCtx m)
           => Connection -> a -> p -> v -> m ()
setRootXid c = setXid c (X.getRoot c)

getXids :: (AtomLike a, XidLike p, XidLike v, BasicEwmhCtx m)
        => Connection -> WINDOW -> a -> p -> m (Either SomeError [v])
getXids c w al prop_type = unsafeLookupATOM al >>= \a -> do
    fmap (toXids . X.value_GetPropertyReply)
        <$> simpleGetProperty c w a (X.fromXid . X.toXid $ prop_type)
    where toXids = map (X.fromXid . X.toXid) . toWords

setProp :: (AtomLike a, XidLike p, Serialize v, BasicEwmhCtx m)
        => Connection -> WINDOW -> a -> p -> v -> m ()
setProp c w al pt v = unsafeLookupATOM al >>= \a -> do
    simpleChangeProperty c w a type_ PropModeReplace $ toBytes v
    where type_  = fromXid (toXid pt)

setXids :: (AtomLike a, XidLike p, XidLike v, BasicEwmhCtx m)
        => Connection -> WINDOW -> a -> p -> [v] -> m ()
setXids c w al pt vs = setProp c w al pt values
    where values = map (fromXid . toXid) vs :: [Word32]

getRootXids :: (AtomLike a, XidLike p, XidLike v, BasicEwmhCtx m)
            => Connection -> a -> p -> m (Either SomeError [v])
getRootXids c = getXids c (X.getRoot c)

setRootProp :: (AtomLike a, XidLike p, Serialize v, BasicEwmhCtx m)
            => Connection -> a -> p -> v -> m ()
setRootProp c = setProp c (X.getRoot c)

setRootXids :: (AtomLike a, XidLike p, XidLike v, BasicEwmhCtx m)
            => Connection -> a -> p -> [v] -> m ()
setRootXids c = setXids c (X.getRoot c)

changeRootProperty :: MonadIO m
                   => Connection -> ATOM -> ATOM -> PropMode -> [Word8] -> m ()
changeRootProperty c p t pm vs = simpleChangeProperty c (X.getRoot c) p t pm vs

getUtf8String :: BasicEwmhCtx m => Connection -> WINDOW -> ATOM -> m (Either SomeError [String])
getUtf8String c w prop = do
    type_ <- unsafeLookupATOM UTF8_STRING
    fmap (toString . X.value_GetPropertyReply) <$> simpleGetProperty c w prop type_

setUtf8String :: BasicEwmhCtx m => Connection -> WINDOW -> ATOM -> [String] -> m ()
setUtf8String c w prop strs = do
    type_ <- unsafeLookupATOM UTF8_STRING
    simpleChangeProperty c w prop type_ PropModeReplace $ toBytes (intersperse "\0" strs)
    -- fmap (toString . value_GetPropertyReply) <$> simpleGetProperty c w prop type_

{-
getString :: (Functor m, MonadEwmh m) => WINDOW -> String -> m (Either SomeError [String])
getString w prop = runEitherT $ do
    atom_ <- hoistEither =<< getAtom prop
    prop_ <- hoistEither =<< simpleGetProperty w atom_ type_
    return . toString . X.value_GetPropertyReply $ prop_
    where type_ = X.fromXid $ X.toXid (X.toValue AtomSTRING :: Word32)

getUtf8String :: (Functor m, MonadEwmh m) => WINDOW -> String -> m (Either SomeError [String])
getUtf8String w prop = runEitherT $ do
    type_ <- hoistEither =<< getAtom "UTF8_STRING"
    atom_ <- hoistEither =<< getAtom prop
    prop_ <- hoistEither =<< simpleGetProperty w atom_ type_
    return . toString . X.value_GetPropertyReply $ prop_

type PropertyName = String

newtype Utf8String = Utf8String { unUtf8String :: String }
    deriving (Ord, Eq, Read, Show, Typeable)

class PropertyClass a where
    convProp :: [Word8] -> Maybe a
    propType :: MonadEwmh m => a -> m (Either SomeError ATOM)
    getProp' :: (Functor m, MonadEwmh m) => WINDOW -> PropertyName -> m (Either SomeError a)
    getProp' w p = runEitherT $ do
        ptyp_ <- hoistEither =<< propType (undefined :: a)
        atom_ <- hoistEither =<< getAtom p
        prop_ <- hoistEither =<< simpleGetProperty w atom_ ptyp_
        hoistEither $ case convProp . X.value_GetPropertyReply $ prop_ of
            Nothing -> Left . X.toError $ UnknownError "GetPropertyClass WINDOW: no window"
            Just wn -> Right wn

instance PropertyClass WINDOW where
    convProp = X.toXidLike
    propType _ = return . Right . X.fromXid . X.toXid $ (X.toValue AtomWINDOW :: Word32)

instance PropertyClass [String] where
    convProp = Just . toString
    propType _ = return . Right . X.fromXid . X.toXid $ (X.toValue AtomSTRING :: Word32)

instance PropertyClass [Utf8String] where
    convProp = Just . map Utf8String . toString
    propType _ = getAtom "UTF8_STRING"

-- | Send an Ewmh request for `WINDOW` to the root window
ewmhRequest :: (MonadEwmh m) => WINDOW -> String -> [Word32] -> m (Either SomeError ())
ewmhRequest window prop_str values = runEitherT $ getConnection >>= \c -> do
    getAtom prop_str >>= hoistEither >>= send c
    where
    send c = liftIO . sendEvent c . request (X.getRoot c) . serializeEvent

    serializeEvent = map (CChar . fromIntegral) . toBytes . event

    event typ = MkClientMessageEvent
        { format_ClientMessageEvent = 32
        , window_ClientMessageEvent = window
        , type_ClientMessageEvent = typ
        , data_ClientMessageEvent = ClientData32 $ map fromIntegral values
        }

    request win raw = MkSendEvent
        { propagate_SendEvent = False
        , destination_SendEvent = win
        , event_mask_SendEvent = [ EventMaskSubstructureNotify
                                 , EventMaskSubstructureRedirect
                                 ]
        , event_SendEvent = raw
        }

changeNetWmState :: (MonadEwmh m)
                 => WINDOW
                 -> SOURCE_INDICATION
                 -> NET_WM_STATE_HINT
                 -> NET_WM_STATE_ACTION
                 -> m (Either SomeError ())
changeNetWmState w si hint action = runEitherT $ do
    hoistEither =<< ewmhRequest w "_NET_WM_STATE" . values
                -- <=< hoistEither
                =<< hoistEither
                =<< getAtom ("_" ++ show hint)
    where values a = [fe action, X.fromXidLike a, 0, fe si]

-- TODO: Gravity
netMoveResizeWindow :: (MonadEwmh m)
                    => WINDOW
                    -> SOURCE_INDICATION
                    -- -> WindowGravity
                    -> [(NET_MOVERESIZE_WINDOW_FLAG, Word32)]
                    -> m (Either SomeError ())
netMoveResizeWindow _ _ [] = return . Left . X.toError $ UnknownError "netMoveResizeWindow: no flags"
netMoveResizeWindow w si vp = do
    ewmhRequest w "_NET_MOVERESIZE_WINDOW" $ flags vp : map snd (M.toList $ values vp)
    where
    -- 1 ^= north west gravity
    flags = (+1) . foldl (.=.) 0 . source . map (toBit . fst)

    source = case si of
        SOURCE_NONE        -> id
        SOURCE_APPLICATION -> (12:)
        SOURCE_PAGER       -> (13:)

    values ((k,v):kvs) = M.insert k v (values kvs)
    values _           = M.fromList [ (NET_MOVERESIZE_WINDOW_X, 0)
                                    , (NET_MOVERESIZE_WINDOW_Y, 0)
                                    , (NET_MOVERESIZE_WINDOW_WIDTH, 0)
                                    , (NET_MOVERESIZE_WINDOW_HEIGHT, 0)
                                    ]

netActiveWindow :: MonadEwmh m => WINDOW -> SOURCE_INDICATION -> m (Either SomeError ())
netActiveWindow w s = ewmhRequest w "_NET_ACTIVE_WINDOW" [fromIntegral $ fromEnum s]

getNetActiveWindow :: (Applicative m, MonadEwmh m) => m (Either SomeError WINDOW)
getNetActiveWindow = runEitherT $ getConnection >>= \c -> do
    atom_ <- hoistEither =<< getAtom "_NET_ACTIVE_WINDOW"
    prop_ <- hoistEither =<< simpleGetProperty (X.getRoot c) atom_ prop_type
    hoistEither $ case X.toXidLike . X.value_GetPropertyReply $ prop_ of
        Nothing -> Left . X.toError $ UnknownError "getNetActiveWindow: no window"
        Just w  -> Right w
    where prop_type = X.fromXid $ X.toXid (X.toValue AtomWINDOW :: Word32)

netRestackWindow :: MonadEwmh m => WINDOW -> SOURCE_INDICATION -> m (Either SomeError ())
netRestackWindow win s = ewmhRequest win "_NET_RESTACK_WINDOW" [toWord s]
    where toWord = fromIntegral . fromEnum
-}

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

-- | Send an Ewmh request for `WINDOW` to the root window
sendRequest :: (AtomLike a, Serialize d, BasicEwmhCtx m)
            => Connection -> WINDOW -> a -> d -> m ()
sendRequest c w a d = void . runMaybeT $ do
    lookupATOM a >>= hoistMaybe >>= send
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

getNetSupported :: BasicEwmhCtx m => Connection -> m (Either SomeError NetSupported)
getNetSupported c = runExceptT $ do
    atomids <- mapM lookupAtomId
        =<< eitherToExcept
        =<< getRootXids c NET_SUPPORTED AtomATOM
    return $ NetSupported (atoms atomids) (states atomids) (actions atomids) (types atomids)
    where
    -- yeah..
    atoms   = catMaybes . map fromAtom . catMaybes
    states  = catMaybes . map fromAtom . catMaybes
    actions = catMaybes . map fromAtom . catMaybes
    types   = catMaybes . map fromAtom . catMaybes

setNetSupported :: BasicEwmhCtx m => Connection -> NetSupported -> m ()
setNetSupported c ns = do
    state     <- unsafeLookupATOM NET_WM_STATE
    types     <- unsafeLookupATOM NET_WM_WINDOW_TYPE
    actions   <- unsafeLookupATOM NET_WM_ALLOWED_ACTIONS

    atoms     <- mapM unsafeLookupATOM (ewmhAtoms ns)
    atoms'    <- insertAt state   atoms   <$> mapM unsafeLookupATOM (netWmStates ns)
    atoms''   <- insertAt types   atoms'  <$> mapM unsafeLookupATOM (netWmWindowTypes ns)
    atoms'''  <- insertAt actions atoms'' <$> mapM unsafeLookupATOM (netWmAllowedActions ns)

    setRootXids c NET_SUPPORTED AtomATOM atoms'''

    where
    insertAt :: Eq t => t -> [t] -> [t] -> [t]
    insertAt _ [] _      = []
    insertAt a (x:xs) as | a == x    = x : as ++ xs
                         | otherwise = x : insertAt a xs as

getNetClientList :: BasicEwmhCtx m => Connection -> m (Either SomeError [WINDOW])
getNetClientList c = runExceptT $ do
    getRootXids c NET_CLIENT_LIST AtomWINDOW
        >>= fmap (map X.fromXid) . eitherToExcept

setNetClientList :: BasicEwmhCtx m => Connection -> [WINDOW] -> m ()
setNetClientList c = setRootXids c NET_CLIENT_LIST AtomWINDOW

getNetClientListStacking :: BasicEwmhCtx m => Connection -> m (Either SomeError [WINDOW])
getNetClientListStacking c = runExceptT $ do
    getRootXids c NET_CLIENT_LIST_STACKING AtomWINDOW
        >>= fmap (map X.fromXid) . eitherToExcept

setNetClientListStacking :: BasicEwmhCtx m => Connection -> [WINDOW] -> m ()
setNetClientListStacking c = setRootXids c NET_CLIENT_LIST_STACKING AtomWINDOW

getNetNumberOfDesktops :: BasicEwmhCtx m => Connection -> m (Either SomeError Word32)
getNetNumberOfDesktops c = getRootXid c NET_NUMBER_OF_DESKTOPS AtomCARDINAL

setNetNumberOfDesktops :: BasicEwmhCtx m => Connection -> Word32 -> m ()
setNetNumberOfDesktops c = setRootXid c NET_NUMBER_OF_DESKTOPS AtomCARDINAL

getNetDesktopGeometry :: BasicEwmhCtx m => Connection -> m (Either SomeError (Word32, Word32))
getNetDesktopGeometry c = toTuple <$> getRootXids c NET_DESKTOP_GEOMETRY AtomCARDINAL
    where toTuple (Right (w:h:_)) = Right (w, h)
          toTuple (Right _)       = Left . X.toError $ UnknownError "getNetDesktopGeometry: no values"
          toTuple (Left e)        = Left e

setNetDesktopGeometry :: BasicEwmhCtx m => Connection -> (Word32, Word32) -> m ()
setNetDesktopGeometry c = setRootProp c NET_DESKTOP_GEOMETRY AtomCARDINAL

getNetDesktopViewport :: BasicEwmhCtx m => Connection -> m (Either SomeError [(Word32, Word32)])
getNetDesktopViewport c = runExceptT $ do
    getRootXids c NET_DESKTOP_VIEWPORT AtomCARDINAL
        >>= fmap (toTuples . map X.fromXid) . eitherToExcept
    where toTuples (x:y:rest) = (x,y) : toTuples rest
          toTuples _          = []

setNetDesktopViewport :: BasicEwmhCtx m => Connection -> [(Word32, Word32)] -> m ()
setNetDesktopViewport c = setRootProp c NET_DESKTOP_VIEWPORT AtomCARDINAL

getNetCurrentDesktop :: BasicEwmhCtx m => Connection -> m (Either SomeError Word32)
getNetCurrentDesktop c = getRootXid c NET_CURRENT_DESKTOP AtomCARDINAL

setNetCurrentDesktop :: BasicEwmhCtx m => Connection -> Word32 -> m ()
setNetCurrentDesktop c = setRootXid c NET_CURRENT_DESKTOP AtomCARDINAL

getNetDesktopNames :: BasicEwmhCtx m => Connection -> m (Either SomeError [String])
-- getNetDesktopNames c = unsafeLookupATOM NET_DESKTOP_NAMES >>= getUtf8String c (X.getRoot c)
getNetDesktopNames c = getProp c (X.getRoot c) NET_DESKTOP_NAMES UTF8_STRING

setNetDesktopNames :: BasicEwmhCtx m => Connection -> [String] -> m ()
setNetDesktopNames c v = do
    a <- unsafeLookupATOM NET_DESKTOP_NAMES
    setUtf8String c (X.getRoot c) a v

getActiveWindow :: BasicEwmhCtx m => Connection -> m (Either SomeError WINDOW)
getActiveWindow c = getRootXid c NET_ACTIVE_WINDOW AtomWINDOW

setActiveWindow :: BasicEwmhCtx m => Connection -> WINDOW -> m ()
setActiveWindow c = setRootXid c NET_ACTIVE_WINDOW AtomWINDOW

getNetWorkarea :: BasicEwmhCtx m => Connection -> m (Either SomeError (Word32, Word32, Word32, Word32))
getNetWorkarea c = toTuple <$> getRootXids c NET_WORKAREA AtomCARDINAL
    where toTuple (Right (x:y:w:h:_)) = Right (x, y, w, h)
          toTuple (Right _)           = Left . X.toError $ UnknownError "getNetWorkarea: no values"
          toTuple (Left e)            = Left e

setNetWorkarea :: BasicEwmhCtx m => Connection -> (Word32, Word32, Word32, Word32) -> m ()
setNetWorkarea c = setRootProp c NET_WORKAREA AtomCARDINAL

getNetSupportingWmCheck :: BasicEwmhCtx m => Connection -> m (Either SomeError WINDOW)
getNetSupportingWmCheck c = getRootXid c NET_SUPPORTING_WM_CHECK AtomWINDOW

setNetSupportingWmCheck :: BasicEwmhCtx m => Connection -> WINDOW -> m ()
setNetSupportingWmCheck c = setRootXid c NET_SUPPORTING_WM_CHECK AtomWINDOW

getNetVirtualRoots :: BasicEwmhCtx m => Connection -> m (Either SomeError [WINDOW])
getNetVirtualRoots c = runExceptT $ do
    getRootXids c NET_VIRTUAL_ROOTS AtomWINDOW
        >>= fmap (map X.fromXid) . eitherToExcept

setNetVirtualRoots :: BasicEwmhCtx m => Connection -> [WINDOW] -> m ()
setNetVirtualRoots c = setRootXids c NET_VIRTUAL_ROOTS AtomWINDOW

getNetDesktopLayout :: BasicEwmhCtx m => Connection -> m (Either SomeError NetDesktopLayout)
getNetDesktopLayout conn = do
    toNetDesktopLayout <$> getRootXids conn NET_DESKTOP_LAYOUT AtomCARDINAL
    where
    toNetDesktopLayout (Right (o:c:r:s:_)) = Right $ NetDesktopLayout
        { orientation     = X.fromBit $ fromIntegral (o :: Word32)
        , starting_corner = X.fromBit $ fromIntegral (s :: Word32)
        , columns         = fromIntegral (c :: Word32)
        , rows            = fromIntegral (r :: Word32)
        }
    toNetDesktopLayout (Left e)  = Left e
    toNetDesktopLayout (Right _) = Left . X.toError $ UnknownError "getNetDesktopLayout: no values"

setNetDesktopLayout :: BasicEwmhCtx m => Connection -> NetDesktopLayout -> m ()
setNetDesktopLayout c = setRootProp c NET_DESKTOP_LAYOUT AtomCARDINAL

getNetShowingDesktop :: BasicEwmhCtx m => Connection -> m (Either SomeError Word32)
getNetShowingDesktop c = getRootXid c NET_SHOWING_DESKTOP AtomCARDINAL

setNetShowingDesktop :: BasicEwmhCtx m => Connection -> Word32 -> m ()
setNetShowingDesktop c = setRootXid c NET_SHOWING_DESKTOP AtomCARDINAL

--------------------------------
-- Other Root Window Messages --
--------------------------------

requestNetCloseWindow :: BasicEwmhCtx m => Connection -> WINDOW -> SourceIndication -> m ()
requestNetCloseWindow c w si = do
    sendRequest c w NET_CLOSE_WINDOW ([X.toValue TimeCurrentTime, X.toValue si] :: [Word32])

requestNetMoveresizeWindow :: BasicEwmhCtx m
                           => Connection -> WINDOW -> NetMoveresizeWindow -> m ()
requestNetMoveresizeWindow c w mr = sendRequest c w NET_MOVERESIZE_WINDOW values
    where
    x      = netMoveresizeWindow_x mr
    y      = netMoveresizeWindow_y mr
    width  = fromIntegral <$> netMoveresizeWindow_width mr
    height = fromIntegral <$> netMoveresizeWindow_height mr

    sourceIndicationBit = case netMoveresizeWindow_sourceIndication mr of
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

requestNetWmMoveresize :: BasicEwmhCtx m => Connection -> WINDOW -> NetWmMoveresize -> m ()
requestNetWmMoveresize c w mr = do
    sendRequest c w NET_WM_MOVERESIZE [x_root, y_root, direction, button, sourceIndication]
    where
    x_root           = fromMaybe 0 $ netWmMoveresize_x_root mr
    y_root           = fromMaybe 0 $ netWmMoveresize_y_root mr
    direction        = X.toValue $ netWmMoveresize_direction mr
    button           = X.toValue $ netWmMoveresize_button mr
    sourceIndication = X.toValue $ netWmMoveresize_sourceIndication mr

requestNetRestackWindow :: BasicEwmhCtx m => Connection -> WINDOW -> NetRestackWindow -> m ()
requestNetRestackWindow c w rw = do
    sendRequest c w NET_RESTACK_WINDOW ([sourceIndication, sibling_window, detail] :: [Word32])
    where
    sourceIndication = X.toValue $ netRestackWindow_sourceIndication rw
    sibling_window   = X.fromXid . X.toXid $ netRestackWindow_sibling_window rw
    detail           = X.toValue $ netRestackWindow_detail rw

requestNetRequestFrameExtents :: BasicEwmhCtx m => Connection -> WINDOW -> m ()
requestNetRequestFrameExtents c w = sendRequest c w NET_REQUEST_FRAME_EXTENTS ([] :: [Word32])

-----------------------------------
-- Application Window Properties --
-----------------------------------

getNetWmName :: BasicEwmhCtx m => Connection -> WINDOW -> m (Either SomeError [String])
getNetWmName c w = unsafeLookupATOM NET_WM_NAME >>= getUtf8String c w

setNetWmName :: BasicEwmhCtx m => Connection -> WINDOW -> [String] -> m ()
setNetWmName c w v = unsafeLookupATOM NET_WM_NAME >>= flip (setUtf8String c w) v

getNetWmVisibleName :: BasicEwmhCtx m => Connection -> WINDOW -> m (Either SomeError [String])
getNetWmVisibleName c w = unsafeLookupATOM NET_WM_VISIBLE_NAME >>= getUtf8String c w

setNetWmVisibleName :: BasicEwmhCtx m => Connection -> WINDOW -> [String] -> m ()
setNetWmVisibleName c w v = unsafeLookupATOM NET_WM_VISIBLE_NAME >>= flip (setUtf8String c w) v

getNetWmIconName :: BasicEwmhCtx m => Connection -> WINDOW -> m (Either SomeError [String])
getNetWmIconName c w = unsafeLookupATOM NET_WM_ICON_NAME >>= getUtf8String c w

setNetWmIconName :: BasicEwmhCtx m => Connection -> WINDOW -> [String] -> m ()
setNetWmIconName c w v = unsafeLookupATOM NET_WM_ICON_NAME >>= flip (setUtf8String c w) v

getNetWmVisibleIconName :: BasicEwmhCtx m => Connection -> WINDOW -> m (Either SomeError [String])
getNetWmVisibleIconName c w = unsafeLookupATOM NET_WM_VISIBLE_ICON_NAME >>= getUtf8String c w

setNetWmVisibleIconName :: BasicEwmhCtx m => Connection -> WINDOW -> [String] -> m ()
setNetWmVisibleIconName c w v = unsafeLookupATOM NET_WM_VISIBLE_ICON_NAME >>= flip (setUtf8String c w) v

getNetWmDesktop :: BasicEwmhCtx m => Connection -> WINDOW -> m (Either SomeError Word32)
getNetWmDesktop c w = getXid c w NET_WM_DESKTOP AtomCARDINAL

setNetWmDesktop :: BasicEwmhCtx m => Connection -> WINDOW -> Word32 -> m ()
setNetWmDesktop c w = setXid c w NET_WM_DESKTOP AtomCARDINAL

requestNetWmDesktop ::BasicEwmhCtx m => Connection -> WINDOW -> NetWmDesktop -> m ()
requestNetWmDesktop c w d = sendRequest c w NET_WM_DESKTOP [desktop, source]
    where
    desktop = netWmDesktop_new_desktop d
    source  = fromIntegral . toBit . netWmDesktop_source_indication $ d

getNetWmWindowType :: BasicEwmhCtx m => Connection -> WINDOW -> m (Either SomeError [NET_WM_WINDOW_TYPE])
getNetWmWindowType c w = runExceptT $ do
    getXids c w NET_WM_WINDOW_TYPE AtomATOM
        >>= eitherToExcept
        >>= fmap (catMaybes . map fromAtom . catMaybes) . mapM lookupAtomId

setNetWmWindowType :: BasicEwmhCtx m => Connection -> WINDOW -> [NET_WM_WINDOW_TYPE] -> m ()
setNetWmWindowType c w vs = do
    mapM unsafeLookupATOM vs >>= setXids c w NET_WM_WINDOW_TYPE AtomATOM

getNetWmState :: BasicEwmhCtx m => Connection -> WINDOW -> m (Either SomeError [NET_WM_STATE])
getNetWmState c w = runExceptT $ do
    getXids c w NET_WM_STATE AtomATOM
        >>= eitherToExcept
        >>= fmap (catMaybes . map fromAtom . catMaybes) . mapM lookupAtomId

setNetWmState :: BasicEwmhCtx m => Connection -> WINDOW -> [NET_WM_STATE] -> m ()
setNetWmState c w vs = mapM unsafeLookupATOM vs >>= setXids c w NET_WM_STATE AtomATOM
