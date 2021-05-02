module Square where

import Segment
import Vec

{-
      dA'    dB'
       |      |
 dA -- A ---- B -- dB
       |      |
       |      |
 dD -- D ---- C -- dC
       |      |
      dD'    dC'
-}
-- Square dA' dB' (Segment dA A B dB) (Segment dD D C dC) dD' dC'
data Square = Square Vec Vec Segment Segment Vec Vec

cubical2 :: Square -> (Double, Double) -> Vec
cubical2 (Square da' db' sa@(Segment da _ _ db) sd@(Segment dd _ _ dc) dd' dc') (x, y) = r
  where
    ab = cubical sa x
    dab = cubical (Segment da da' db' db) x
    dc_ = cubical sd x
    ddc = cubical (Segment dd dd' dc' dc) x
    r = cubical (Segment ddc dc_ ab dab) y
