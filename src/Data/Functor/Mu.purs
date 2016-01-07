module Data.Functor.Mu
  ( Mu()
  , roll
  , unroll
  ) where

import Prelude

-- | `Mu f` is the least fixed point of a functor `f`, when it exists.
data Mu f = In (f (Mu f))

roll :: forall f. f (Mu f) -> Mu f
roll = In

unroll :: forall f. Mu f -> f (Mu f)
unroll (In x) = x
