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
import Control.Monad.Reader (ask)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.XHB (Connection, SomeError, ATOM, InternAtom(..))
import qualified Graphics.XHB as X
import qualified Graphics.XHB.Atom as XA
import Graphics.XHB.Ewmh.Types

class MonadIO m => MonadEwmh m where
    getAtom :: String -> m (Either SomeError ATOM)
    getConnection :: m Connection

instance MonadIO m => MonadEwmh (EwmhT m) where
    getAtom = EwmhT . XA.getAtom
    getConnection = EwmhT ask

instance (MonadTrans t, MonadEwmh m, MonadIO (t m)) => MonadEwmh (t m) where
    getAtom = lift . getAtom
    getConnection = lift getConnection
