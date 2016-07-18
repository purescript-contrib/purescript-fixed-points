module Data.Ord1 where

import Prelude

class Ord1 f where
  compare1 :: forall a. Ord a => f a -> f a -> Ordering
