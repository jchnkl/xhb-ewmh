{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds    #-}

module Graphics.XHB.Ewmh.Types where

import Data.Typeable (Typeable)
import Graphics.XHB.Atom
import Graphics.XHB.Ewmh.Atoms

type EwmhT = AtomT

type Ewmh = EwmhT IO

type MonadEwmh = MonadAtom

