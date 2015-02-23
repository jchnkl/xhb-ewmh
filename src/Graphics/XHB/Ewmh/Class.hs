{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Graphics.XHB.Ewmh.Class
    where
    -- ( MonadEwmh(..)
    -- ) where

import Control.Monad.Reader (ask)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.XHB (Connection, ATOM)
import Graphics.XHB.Atom
import Graphics.XHB.Ewmh.Atoms
import Graphics.XHB.Ewmh.Types

-- instance MonadAtom 

-- class MonadAtom m => MonadEwmh m where
-- -- class (AtomNameLike l, Monad m) => MonadAtom l m where
--     insertAtom :: l -> ATOM -> m ()
--     lookupAtom :: l -> m (Maybe ATOM)
--     lookupName :: ATOM -> m (Maybe l)
--     -- getEwmhAtom :: EWMH_ATOM -> m ATOM
--     -- getConnection :: m Connection
-- --
-- -- instance MonadIO m => MonadEwmh (EwmhT m) where
-- --     getEwmhAtom = EwmhT . XA.unsafeLookupAtom . toAtomString
-- --     getConnection = EwmhT ask
-- --
-- -- instance (MonadTrans t, MonadEwmh m, MonadIO (t m)) => MonadEwmh (t m) where
-- --     getEwmhAtom = lift . getEwmhAtom
-- --     getConnection = lift getConnection
