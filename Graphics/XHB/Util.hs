module Graphics.XHB.Util
    ( sync
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.XHB

sync :: (Functor m, MonadIO m) => Connection -> m ()
sync c = void $ liftIO $ getInputFocus c >>= getReply
