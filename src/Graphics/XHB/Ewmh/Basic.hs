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
import Data.Binary.Put (Put, runPut, putWord8, putWord16host, putWord32host)
import Data.ByteString.Lazy (unpack)
import Data.Maybe (catMaybes)
import Control.Monad (join, replicateM_)
import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT)
import Control.Monad.State (gets)
import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.IO.Class (MonadIO(..))

import Graphics.XHB
import Graphics.XHB.Atom
import Graphics.XHB.Ewmh.Values
import Graphics.XHB.Ewmh.Atoms
import Graphics.XHB.Ewmh.Types

type BasicEwmhCtx m = (Applicative m, MonadIO m, MonadEwmh m)

fe :: (Enum a, Integral b) => a -> b
fe = fromIntegral . fromEnum

(.=.) :: (Bits a) => a -> Int -> a
(.=.) = setBit

fromXidLike :: (XidLike a, XidLike b) => a -> b
fromXidLike = fromXid . toXid

atomToXidLike :: (XidLike a) => Atom -> a
atomToXidLike a = fromXid $ toXid (toValue a :: Word32)

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

toXidLike :: XidLike a => [Word8] -> Maybe a
toXidLike = fmap fromXidLike . fromBytes

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

putSkip8 :: Int -> Put
putSkip8 n = replicateM_ n $ putWord8 0

putSkip16 :: Int -> Put
putSkip16 n = replicateM_ n $ putWord16host 0

putSkip32 :: Int -> Put
putSkip32 n = replicateM_ n $ putWord32host 0

class Serialize a where
    serialize :: a -> Put

    toBytes :: a -> [Word8]
    toBytes = unpack . runPut . serialize

instance Serialize a => Serialize [a] where
    serialize = mapM_ serialize
    toBytes = concatMap toBytes

instance (Serialize a, Serialize b) => Serialize (a, b) where
    serialize (a,b) = serialize a >> serialize b
    toBytes (a, b) = toBytes a ++ toBytes b

instance (Serialize a, Serialize b, Serialize c) => Serialize (a, b, c) where
    serialize (a,b,c) = serialize a >> serialize b >> serialize c
    toBytes (a, b,c) = toBytes a ++ toBytes b ++ toBytes c

instance (Serialize a, Serialize b, Serialize c, Serialize d) => Serialize (a, b, c, d) where
    serialize (a,b,c,d) = serialize a >> serialize b >> serialize c >> serialize d
    toBytes (a, b,c,d) = toBytes a ++ toBytes b ++ toBytes c ++ toBytes d

instance Serialize Char where
    serialize = putWord8 . fromIntegral . ord

instance Serialize Word32 where
    serialize = putWord32host

instance Serialize Int where
    -- this might cause some breakage, but practically `#define`s are not < 0
    serialize = putWord32host . fromIntegral

instance Serialize ATOM where
    serialize v = putWord32host (fromXid $ toXid v :: Word32)

instance Serialize WINDOW where
    serialize v = putWord32host (fromXid $ toXid v :: Word32)

instance Serialize ClientMessageEvent where
    serialize (MkClientMessageEvent fmt win typ dat) = do
        putWord8 33 -- 33 ^= ClientMessageEvent
        putWord8 fmt
        putSkip8 2
        serialize win
        serialize typ
        serialize dat

instance Serialize ClientMessageData where
    serialize (ClientData8  ws) = do mapM_ putWord8 ws
                                     putSkip8 (20 - length ws)
    serialize (ClientData16 ws) = do mapM_ putWord16host ws
                                     putSkip16 (10 - length ws)
    serialize (ClientData32 ws) = do mapM_ putWord32host ws
                                     putSkip32 (5 - length ws)

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
    liftIO $ getProperty c request >>= getReply
    where
    request = MkGetProperty
        { delete_GetProperty = False
        , window_GetProperty = win
        , property_GetProperty = prop
        , type_GetProperty = prop_type
        , long_offset_GetProperty = 0
        , long_length_GetProperty = maxBound
        }

getWindowAtomList :: (MonadIO m, Functor m)
                  => Connection -> WINDOW -> ATOM -> m (Either SomeError [ATOM])
getWindowAtomList c w prop = fmap toATOMs <$> simpleGetProperty c w prop type_
    where type_   = fromXid $ toXid (toValue AtomATOM :: Word32)
          toATOMs = map fromXidLike . toWords . value_GetPropertyReply

getRootProperty :: MonadIO m => Connection -> ATOM -> ATOM -> m (Either SomeError GetPropertyReply)
getRootProperty c prop typ_ = simpleGetProperty c (getRoot c) prop typ_

getRootAtomList :: (MonadIO m, Functor m)
                => Connection -> ATOM -> m (Either SomeError [ATOM])
getRootAtomList c prop = fmap toATOMs <$> getRootProperty c prop type_
    where type_   = fromXid $ toXid (toValue AtomATOM :: Word32)
          toATOMs = map fromXidLike . toWords . value_GetPropertyReply

simpleChangeProperty :: MonadIO m
                     => Connection
                     -> WINDOW -- ^ Target window
                     -> ATOM -- ^ Property to change
                     -> ATOM -- ^ Property Type
                     -> PropMode -- ^ Append, Prepend or Replace
                     -> [Word8] -- ^ values
                     -> m ()
simpleChangeProperty c window prop prop_type prop_mode values = do
    liftIO $ changeProperty c request
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

changeRootProperty :: MonadIO m
                   => Connection -> ATOM -> ATOM -> PropMode -> [Word8] -> m ()
changeRootProperty c p t pm vs = simpleChangeProperty c (getRoot c) p t pm vs

{-
getString :: (Functor m, MonadEwmh m) => WINDOW -> String -> m (Either SomeError [String])
getString w prop = runEitherT $ do
    atom_ <- hoistEither =<< getAtom prop
    prop_ <- hoistEither =<< simpleGetProperty w atom_ type_
    return . toString . value_GetPropertyReply $ prop_
    where type_ = fromXid $ toXid (toValue AtomSTRING :: Word32)

getUtf8String :: (Functor m, MonadEwmh m) => WINDOW -> String -> m (Either SomeError [String])
getUtf8String w prop = runEitherT $ do
    type_ <- hoistEither =<< getAtom "UTF8_STRING"
    atom_ <- hoistEither =<< getAtom prop
    prop_ <- hoistEither =<< simpleGetProperty w atom_ type_
    return . toString . value_GetPropertyReply $ prop_

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
        hoistEither $ case convProp . value_GetPropertyReply $ prop_ of
            Nothing -> Left . toError $ UnknownError "GetPropertyClass WINDOW: no window"
            Just wn -> Right wn

instance PropertyClass WINDOW where
    convProp = toXidLike
    propType _ = return . Right . fromXid . toXid $ (toValue AtomWINDOW :: Word32)

instance PropertyClass [String] where
    convProp = Just . toString
    propType _ = return . Right . fromXid . toXid $ (toValue AtomSTRING :: Word32)

instance PropertyClass [Utf8String] where
    convProp = Just . map Utf8String . toString
    propType _ = getAtom "UTF8_STRING"

-- | Send an Ewmh request for `WINDOW` to the root window
ewmhRequest :: (MonadEwmh m) => WINDOW -> String -> [Word32] -> m (Either SomeError ())
ewmhRequest window prop_str values = runEitherT $ getConnection >>= \c -> do
    getAtom prop_str >>= hoistEither >>= send c
    where
    send c = liftIO . sendEvent c . request (getRoot c) . serializeEvent

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
    where values a = [fe action, fromXidLike a, 0, fe si]

-- TODO: Gravity
netMoveResizeWindow :: (MonadEwmh m)
                    => WINDOW
                    -> SOURCE_INDICATION
                    -- -> WindowGravity
                    -> [(NET_MOVERESIZE_WINDOW_FLAG, Word32)]
                    -> m (Either SomeError ())
netMoveResizeWindow _ _ [] = return . Left . toError $ UnknownError "netMoveResizeWindow: no flags"
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
    prop_ <- hoistEither =<< simpleGetProperty (getRoot c) atom_ prop_type
    hoistEither $ case toXidLike . value_GetPropertyReply $ prop_ of
        Nothing -> Left . toError $ UnknownError "getNetActiveWindow: no window"
        Just w  -> Right w
    where prop_type = fromXid $ toXid (toValue AtomWINDOW :: Word32)

netRestackWindow :: MonadEwmh m => WINDOW -> SOURCE_INDICATION -> m (Either SomeError ())
netRestackWindow win s = ewmhRequest win "_NET_RESTACK_WINDOW" [toWord s]
    where toWord = fromIntegral . fromEnum
-}

getNetSupported :: BasicEwmhCtx m => Connection -> m (Either SomeError NetSupported)
getNetSupported c = runExceptT $ do
    atomids <- mapM lookupAtomId
        =<< eitherToExcept
        =<< getRootAtomList c
        =<< unsafeLookupATOM NET_SUPPORTED
    return $ NetSupported (atoms atomids) (states atomids) (actions atomids) (types atomids)
    where
    -- yeah..
    atoms   = catMaybes . map fromAtom . catMaybes
    states  = catMaybes . map fromAtom . catMaybes
    actions = catMaybes . map fromAtom . catMaybes
    types   = catMaybes . map fromAtom . catMaybes

leftToMaybe :: Either a b -> Maybe a
leftToMaybe (Left  a) = Just a
leftToMaybe (Right _) = Nothing

setNetSupported :: BasicEwmhCtx m => Connection -> NetSupported -> m ()
setNetSupported c ns = do
    supported <- unsafeLookupATOM NET_SUPPORTED
    state     <- unsafeLookupATOM NET_WM_STATE
    types     <- unsafeLookupATOM NET_WM_WINDOW_TYPE
    actions   <- unsafeLookupATOM NET_WM_ALLOWED_ACTIONS

    atoms     <- mapM unsafeLookupATOM (ewmhAtoms ns)
    atoms'    <- insertAt state   atoms   <$> mapM unsafeLookupATOM (netWmStates ns)
    atoms''   <- insertAt types   atoms'  <$> mapM unsafeLookupATOM (netWmWindowTypes ns)
    atoms'''  <- insertAt actions atoms'' <$> mapM unsafeLookupATOM (netWmAllowedActions ns)

    changeRootProperty c supported type_ PropModeReplace $ concatMap toBytes atoms'''

    where
    type_ = fromXid $ toXid (toValue AtomATOM :: Word32)

    insertAt :: Eq t => t -> [t] -> [t] -> [t]
    insertAt _ [] _      = []
    insertAt a (x:xs) as | a == x    = x : as ++ xs
                         | otherwise = x : insertAt a xs as

-- changeRootProperty :: MonadIO m
--                    => Connection -> ATOM -> ATOM -> PropMode -> [Word8] -> m ()
    -- , netWmWindowTypes    :: [NetWmWindowType]
    -- atoms = ewmhAtoms ns
    -- ewmhAtoms           :: [EwmhAtom]
    -- states = netWmStates ns
    -- netWmAllowedActions :: [NetWmAllowedActions]
    -- , netWmWindowTypes    :: [NetWmWindowType]

-- -- getNetWmState :: (MonadIO m, MonadEwmh m) => Connection -> m [NetWmState]
-- getNetWmState :: (MonadAtom m, MonadIO m, Functor m)
--               => Connection -> WINDOW -> m (Either SomeError [NetWmState])
-- getNetWmState c w = runExceptT $ do
--     unsafeLookupATOM NET_WM_STATE
--         >>= rootWindowAtomList c
--         >>= eitherToExcept
--         >>= fmap (catMaybes . map fromAtom . catMaybes) . mapM lookupAtomId

getNetWmState :: SimpleEwmhCtx m => Connection -> WINDOW -> m (Either SomeError [NetWmState])
getNetWmState c w = runExceptT $ do
    unsafeLookupATOM NET_WM_STATE
        >>= getWindowAtomList c w
        >>= eitherToExcept
        -- >>= fmap (catMaybes . map (join . fmap fromAtom)) . mapM lookupAtomId
        >>= fmap (catMaybes . map fromAtom . catMaybes) . mapM lookupAtomId
        -- >>= fmap (catMaybes . map fromAtom . catMaybes) . mapM lookupAtomId


-- test c = map (atomName . lookupAtomId) . catMaybes <$> getNetSupported' c

-- setNetSupported :: (EwmhAtom a, MonadEwmh m) => [a] -> m ()
-- setNetSupported = undefined

-- foo :: MonadEwmh m => m ()
-- foo = do
--     getNetSupported
