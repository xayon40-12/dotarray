module Segment where

import Vec

--       dA -- A ---- B -- dB
-- Segment dA A B dB
data Segment = Segment Vec Vec Vec Vec

-- a*x^3 + b*x^2 + c*x + d
cubical :: Segment -> Double -> Vec
cubical (Segment da pa pb db) x = r
  where
    d = pa
    c = da
    -- a + b + c + d = pb
    -- 3a + 2b + c = db
    b = 3 *. pb .-. db .-. 2 *. c .-. 3 *. d
    a = pb .-. d .-. c .-. b
    r = a .* x ** 3 .+. b .* x ** 2 .+. c .* x .+. d

dist :: Segment -> Ray -> P2 -> Double
dist s (Ray p d) (x, t) = norm $ cubical s x .-. (p .+. t *. d)

type P2 = (Double, Double)

type Fun2 = P2 -> Double

grad :: Double -> Fun2 -> P2 -> P2
grad h f (x, y) = (dx, dy)
  where
    dx = (f (x + h, y) - f (x, y)) / h
    dy = (f (x, y + h) - f (x, y)) / h

descent :: Int -> Double -> Fun2 -> P2 -> Either Double P2
descent 0 _ f p = Left $ f p
descent m h f p@(x, y) = if d < h then Right pn else descent (m -1) h f pn
  where
    (dx, dy) = grad (h / 10) f p -- FIXME should be normalized and multiplied by an epsilon, or I should use the "log grad descent"
    pn = (x - dx, y - dy)
    d = f pn

-- Ray pos dir
data Ray = Ray Vec Vec

-- Node pos normal distance
data Node = Node Vec Vec Double

{-
cast :: Segment -> Ray -> Node
{-
  pos + t*dir = a*x^3 + b*x^2 + c*x + d
  a = pb - d - c - b
  b = 3*pb - db - 2*c - 3*d
  c = da
  d = pa
-}
cast (Segment da pa pb db) (Ray pos dir) =

-}
