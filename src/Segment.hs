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

cast :: Segment -> Ray -> Maybe P2
cast s r = res >>= (\res'@(_, x) -> if x >= 0 && x <= 1 then Just res' else Nothing)
  where
    res = descent 1000 1e-5 (dist r s) (0, 0.5)

type P2 = (Double, Double)

type Fun2 = P2 -> Double

dist :: Ray -> Segment -> P2 -> Double
dist (Ray p d) s (t, x) = norm $ cubical s x .-. (p .+. t *. d)

grad :: Double -> Fun2 -> P2 -> P2
grad h f (x, y) = (dx, dy)
  where
    fxy = f (x, y)
    dx = (f (x + h, y) - fxy) / h
    dy = (f (x, y + h) - fxy) / h

descent :: Int -> Double -> Fun2 -> P2 -> Maybe P2
descent m' h f = go m' 1
  where
    gh = h / 2
    go 0 _ _ = Nothing
    go m e p = if dn < h then Just pn else go mn en pn
      where
        (en, pn, dn, mn) = down (m -1) f e gh p

down :: Int -> Fun2 -> Double -> Double -> P2 -> (Double, P2, Double, Int)
down m' f e' h p' = go m' e' p' (f p')
  where
    n = 10 -- FIXME have a better way to fix it
    g = grad h f
    go m e p@(x, y) d = if dn > d || m == 0 then (e / n, p, d, m) else go (m -1) en pn dn
      where
        (dx, dy) = g p
        pn = (x - dx * 2 * e, y - dy * e) -- NOTE *2 to prioritize first coord
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
