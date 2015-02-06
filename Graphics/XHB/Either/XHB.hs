{-# LANGUAGE FlexibleContexts #-}

module Graphics.XHB.Either.XHB where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Either.Class (MonadEither(..))
import Graphics.XHB (SomeError, Receipt)
import qualified Graphics.XHB as X

getReply :: (MonadIO m, MonadEither SomeError m) => Receipt a -> m a
getReply receipt = liftIO (X.getReply receipt) >>= hoistEither
