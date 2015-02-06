{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.XHB.Either.XHB where

import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (MonadState(..), StateT)
import Control.Monad.Reader (MonadReader(..), ReaderT)
import Control.Monad.Writer (MonadWriter(..), WriterT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Either (EitherT)
import qualified Control.Monad.Trans.Either as E
import Graphics.XHB (SomeError, Receipt)
import qualified Graphics.XHB as X

class (Monad m) => MonadEither l m | m -> l where
    hoistEither :: Either l r -> m r

instance (Monad m) => MonadEither l (EitherT l m) where
    hoistEither = E.hoistEither

instance (MonadEither l m, MonadState s m) => MonadEither l (StateT s m) where
    hoistEither = lift . hoistEither

instance (MonadEither l m, MonadReader r m) => MonadEither l (ReaderT r m) where
    hoistEither = lift . hoistEither

instance (MonadEither l m, MonadWriter s m) => MonadEither l (WriterT s m) where
    hoistEither = lift . hoistEither

getReply :: (MonadIO m, MonadEither SomeError m) => Receipt a -> m a
getReply = (hoistEither =<<) . liftIO . X.getReply
