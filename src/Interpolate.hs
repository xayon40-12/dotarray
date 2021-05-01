module Interpolate where

class Interpolate a b c where
  cubical :: a -> b -> c
