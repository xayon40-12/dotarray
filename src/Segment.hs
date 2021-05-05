module Segment where

import Debug.Trace (trace)
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

-- Ray pos dir
data Ray = Ray Vec Vec deriving (Show)

-- min -> max -> x -> res
clamp :: Double -> Double -> Double -> Double
clamp mi ma x = min ma $ max mi x

cast :: Segment -> Ray -> Maybe (P2, Int)
cast s r = res
  where
    h = 1e-3
    post (i, j) = (max 0 i, clamp 0 1 j) -- NOTE max 0 i as t must be >=0, clamp 0 1 as the x coordinate should stay in [0,1]
    res = descent 1000 post h (dist r s) (0, 0.5)

type P2 = (Double, Double)

type Fun2 = P2 -> Double

type Post = P2 -> P2

dist :: Ray -> Segment -> P2 -> Double
dist (Ray p d) s (t, x) = norm2 $ cubical s x .-. (p .+. t *. d) -- NOTE using norm2 is a lot better than norm

grad :: Double -> Fun2 -> P2 -> P2
grad h f (x, y) = (dx, dy)
  where
    fxy = f (x, y)
    dx = (f (x + h, y) - fxy) / h
    dy = (f (x, y + h) - fxy) / h

descent :: Int -> Post -> Double -> Fun2 -> P2 -> Maybe (P2, Int)
descent m' post h f = go m' 1
  where
    gh = h / 10
    go 0 _ _ = Nothing
    go m e p = if dn < h then Just (pn, m' - mn) else go mn en pn
      where
        (en, pn, dn, mn) = down (m -1) post f e gh p

down :: Int -> Post -> Fun2 -> Double -> Double -> P2 -> (Double, P2, Double, Int)
down m' post f e' h p' = go m' e' p' (f p')
  where
    n = 2 -- FIXME have a better way to fix it
    g = grad h f
    go m e p@(x, y) d = if dn > d || m == 0 then (e / n, p, d, m) else go (m -1) en pn dn
      where
        (dx, dy) = g p
        no = 1 / sqrt (dx * dx + dy * dy)
        pn = post (x - dx * no * e, y - dy * no * e)
        dn = f pn
        en = e * n
