{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Graphics.XHB.Ewmh.Class
    ( MonadEwmh(..)
    ) where

import qualified Data.Map as M
import Control.Monad.State (modify, gets)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.XHB (Connection, SomeError, ATOM, InternAtom(..))
import qualified Graphics.XHB as X
import Graphics.XHB.Ewmh.Types

class MonadIO m => MonadEwmh m where
    getAtom :: String -> m (Either SomeError ATOM)
    getConnection :: m Connection

instance MonadIO m => MonadEwmh (EwmhT m) where
    getAtom name = asks connection >>= \c -> do
        ps <- gets properties
        case M.lookup name ps of
            Just atom -> return (Right atom)
            Nothing -> do
                eatom <- liftIO $ X.internAtom c request >>= X.getReply
                case eatom of
                    Left err   -> return (Left err)
                    Right atom -> do
                        modify $ \e -> e { properties = M.insert name atom ps }
                        return (Right atom)
        where request = MkInternAtom True (fromIntegral $ length name) (X.stringToCList name)

    getConnection = EwmhT $ asks connection

instance (MonadTrans t, MonadEwmh m, MonadIO (t m)) => MonadEwmh (t m) where
    getAtom = lift . getAtom
    getConnection = lift getConnection
