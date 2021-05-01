module Segment where

import Interpolate
import Vec

--       dA -- A ---- B -- dB
-- Segment dA A B dB
data Segment = Segment Vec Vec Vec Vec

instance Interpolate Segment Double Vec where
  -- a*x^3 + b*x^2 + c*x + d
  cubical (Segment da pa pb db) x = r
    where
      d = pa
      c = da
      -- a + b + c + d = pb
      -- 3a + 2b + c = db
      b = 3 *. pb .-. db .-. 2 *. c .-. 3 *. d
      a = pb .-. d .-. c .-. b
      r = a .* x ^ 3 .+. b .* x ^ 2 .+. c .* x .+. d
