module Segment where

import Debug.Trace
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
    fxy = f (x, y)
    dx = (f (x + h, y) - fxy) / h
    dy = (f (x, y + h) - fxy) / h

descent :: Int -> Double -> Fun2 -> P2 -> Either Double P2
descent m h f = go m 1
  where
    gh = h / 2
    go 0 _ p = Left $ f p
    go i e p = if dn < h then trace ("i: " ++ show (m - i)) $ Right pn else go (i - is) en pn
      where
        (en, pn, dn, is) = down f e gh p

down :: Fun2 -> Double -> Double -> P2 -> (Double, P2, Double, Int)
down f e' h p' = go e' p' (f p') 1
  where
    n = 10 -- FIXME have a better way to fix it
    g = grad h f
    go e p@(x, y) d i = if dn > d then (e / n, p, d, i) else go en pn dn (i + 1)
      where
        (dx, dy) = g p
        pn = (x - dx * e, y - dy * e)
        dn = f pn
        en = e * n

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
