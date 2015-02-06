module Graphics.XHB.Ewmh
    ( getAtom
    , simpleGetProperty
    , simpleChangeProperty
    , getString
    , getUtf8String
    ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Either (EitherT(..))

import Graphics.XHB
import Graphics.XHB.Either.Ewmh (Ewmh)
import qualified Graphics.XHB.Either.Ewmh as E

simpleGetProperty :: (MonadIO m)
                  => WINDOW -- ^ Target window
                  -> ATOM -- ^ Property
                  -> ATOM -- ^ Property Type
                  -> Ewmh m (Either SomeError GetPropertyReply)
simpleGetProperty win prop prop_type = runEitherT $ E.simpleGetProperty win prop prop_type

simpleChangeProperty :: (MonadIO m)
                     => WINDOW -- ^ Target window
                     -> ATOM -- ^ Property to change
                     -> ATOM -- ^ Property Type
                     -> PropMode -- ^ Append, Prepend or Replace
                     -> [Word8] -- ^ values
                     -> EitherT SomeError (Ewmh m) ()
simpleChangeProperty window prop prop_type prop_mode values = runEitherT $ do
    E.simpleChangeProperty window prop prop_type prop_mode values

getAtom :: (MonadIO m) => String -> Ewmh m (Either SomeError ATOM)
getAtom = runEitherT . E.getAtom

getString :: (MonadIO m) => WINDOW -> String -> Ewmh m (Either SomeError [String])
getString w prop = runEitherT $ E.getString w prop

getUtf8String :: (MonadIO m) => WINDOW -> String -> Ewmh m (Either SomeError [String])
getUtf8String w prop = runEitherT $ E.getUtf8String w prop
