module Data.Functor.Mu
  ( Mu()
  , roll
  , unroll
  ) where

import Prelude
import Data.TacitString as TS

import Data.Eq1
import Data.Ord1

-- | `Mu f` is the least fixed point of a functor `f`, when it exists.
data Mu f = In (f (Mu f))

roll :: forall f. f (Mu f) -> Mu f
roll = In

unroll :: forall f. Mu f -> f (Mu f)
unroll (In x) = x

-- | To implement `Eq`, we require `f` to have higher-kinded equality.
instance eqMu :: (Eq1 f) => Eq (Mu f) where
  eq (In x) (In y) = eq1 x y

-- | To implement `Ord`, we require `f` to have higher-kinded comparison.
instance ordMu :: (Eq1 f, Ord1 f) => Ord (Mu f) where
  compare (In x) (In y) = compare1 x y

-- | `Show` is compositional, so we only `f` to be able to show a single layer of structure.
-- Therefore, there is no need for `Show1`; we use `TacitString` in order to prevent
-- extra quotes from appearing.
instance showMu :: (Show (f TS.TacitString), Functor f) => Show (Mu f) where
  show (In x) = show $ x <#> (show >>> TS.hush)

