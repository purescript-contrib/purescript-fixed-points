module Data.Eq1 where

import Prelude

class Eq1 f where
  eq1 :: forall a. Eq a => f a -> f a -> Boolean
