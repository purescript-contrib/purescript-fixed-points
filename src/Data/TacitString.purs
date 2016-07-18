module Data.TacitString
  ( TacitString
  , hush
  ) where

import Prelude

newtype TacitString = TacitString String
instance showTacitString :: Show TacitString where
  show (TacitString str) = str

hush :: String -> TacitString
hush = TacitString
