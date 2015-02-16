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

import Control.Monad.Reader (ask)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.XHB (Connection, ATOM)
import qualified Graphics.XHB.Atom as XA
import Graphics.XHB.Ewmh.Atoms
import Graphics.XHB.Ewmh.Types

class MonadIO m => MonadEwmh m where
    getEwmhAtom :: EWMH_ATOM -> m ATOM
    getConnection :: m Connection

instance MonadIO m => MonadEwmh (EwmhT m) where
    getEwmhAtom = EwmhT . XA.unsafeLookupAtom . toAtomString
    getConnection = EwmhT ask

instance (MonadTrans t, MonadEwmh m, MonadIO (t m)) => MonadEwmh (t m) where
    getEwmhAtom = lift . getEwmhAtom
    getConnection = lift getConnection
