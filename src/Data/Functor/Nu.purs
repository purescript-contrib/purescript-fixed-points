module Data.Functor.Nu
  ( Nu
  , unfold
  , observe
  ) where

import Prelude
import Data.Exists (Exists, mkExists, runExists)

type Store s a =
  { pos :: s
  , peek :: s -> a
  }

newtype NuF f a = NuF (Store a (f a))

-- | `Nu f` is the greatest fixed point of the functor `f`, when it exists.
newtype Nu f = Nu (Exists (NuF f))

unfold :: forall f a. a -> (a -> f a) -> Nu f
unfold pos peek =
  Nu $ mkExists $ NuF
    { pos : pos
    , peek : peek
    }

observe :: forall f. (Functor f) => Nu f -> f (Nu f)
observe (Nu e) =
  runExists
    (\(NuF {peek,pos}) -> flip unfold peek <$> peek pos)
    e
