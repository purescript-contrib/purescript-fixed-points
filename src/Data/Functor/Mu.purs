module Data.Functor.Mu
  ( Mu(..)
  , roll
  , unroll
  , transMu
  , cata
  , ana
  , para
  , apo
  , histo
  , Attr(..)
  ) where

import Prelude

import Data.Either (Either, either)
import Data.Eq (class Eq1, eq1)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1, compare1)
import Data.TacitString as TS
import Data.Tuple (Tuple(..))

-- | `Mu f` is the least fixed point of a functor `f`, when it exists.
newtype Mu f = In (f (Mu f))

-- | Rewrites a tree along a natural transformation.
transMu
  ∷ ∀ f g
  . (Functor g)
  ⇒ (∀ a. f a → g a)
  → Mu f
  → Mu g
transMu η =
  roll
    <<< map (transMu η)
    <<< η
    <<< unroll

roll :: forall f. f (Mu f) -> Mu f
roll = In

unroll :: forall f. Mu f -> f (Mu f)
unroll (In x) = x

derive instance newtypeMu :: Newtype (Mu f) _

-- | To implement `Eq`, we require `f` to have higher-kinded equality.
instance eqMu :: Eq1 f => Eq (Mu f) where
  eq (In x) (In y) = eq1 x y

-- | To implement `Ord`, we require `f` to have higher-kinded comparison.
instance ordMu :: (Eq1 f, Ord1 f) => Ord (Mu f) where
  compare (In x) (In y) = compare1 x y

-- | `Show` is compositional, so we only `f` to be able to show a single layer of structure.
-- Therefore, there is no need for `Show1`; we use `TacitString` in order to prevent
-- extra quotes from appearing.
instance showMu :: (Show (f TS.TacitString), Functor f) => Show (Mu f) where
  show (In x) = show $ x <#> (show >>> TS.hush)

-- | catamorphism: apply the function for the bottom up
cata :: forall f a. Functor f => (f a -> a) -> Mu f -> a
cata f =
  f
  <<< map (cata f)
  <<< unroll

-- | anamorphism
ana :: forall f a. Functor f => (a -> f a) -> a -> Mu f
ana f = In <<< map (ana f) <<< f


type RAlgebra f a = f (Tuple (Mu f) a) -> a

-- | paramorphism
para :: forall f a. Functor f => RAlgebra f a -> Mu f -> a
para rAlg = unroll >>> map fanout >>> rAlg
    where fanout :: Mu f -> Tuple (Mu f) a
          fanout t = Tuple t (para rAlg t)


type RCoalgebra f a = a -> f (Either (Mu f) a)

-- | apomorphism
apo :: forall f a. Functor f => RCoalgebra f a -> a -> Mu f
apo f = In <<< map fanin <<< f
   where fanin = either id (apo f)

type CVAlgebra f a = f (Attr f a) -> a

data Attr f a = Attr
              { attribute :: a
              , hole      :: f (Attr f a)
              }

-- | histo
histo :: forall f a. Functor f => CVAlgebra f a -> Mu f -> a
histo h = unroll >>> map worker >>> h where
    worker t = Attr {attribute : (histo h t), hole: (map worker (unroll t))}
