{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module Graphics.XHB.Ewmh.Types (Ewmh, EwmhT(..)) where

import Control.Applicative (Applicative)
import Control.Monad.State (MonadState(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO)
import Graphics.XHB (Connection)
import Graphics.XHB.Atom

newtype EwmhT m a = EwmhT { unEwmhT :: ReaderT Connection (AtomT m) a }
    deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             )

type Ewmh = EwmhT IO

instance MonadTrans EwmhT where
    lift m = EwmhT . lift . lift $ m

instance (MonadState s m) => MonadState s (EwmhT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (MonadReader r m) => MonadReader r (EwmhT m) where
    ask = lift ask
    local f (EwmhT m) = EwmhT . ReaderT $ local f . runReaderT m

instance (MonadWriter w m) => MonadWriter w (EwmhT m) where
    tell = lift . tell
    listen = EwmhT . listen . unEwmhT
    pass = EwmhT . pass . unEwmhT
