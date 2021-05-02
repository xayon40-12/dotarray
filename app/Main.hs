module Main where

import Segment
import Square
import Vec

main :: IO ()
main = do
  let p = cubical2 sq (0.5, 0.5)
  print p
  let s = Segment (Vec 0 1 0) (Vec 0 0 0) (Vec 1 0 0) (Vec 0 (-1) 0)
  let r = Ray (Vec 0.5 10 0) (Vec 0 (-1) 0)
  print $ descent 1000 1e-4 (dist s r) (0, 0)

sq :: Square
sq = Square dht dht ab dc dhb dhb
  where
    a = Vec 0 1 0
    b = Vec 1 1 0
    c = Vec 1 0 0
    d = Vec 0 0 0
    dwl = Vec 1 0 1
    dwr = Vec 1 0 (-1)
    dht = Vec 0 1 (-1)
    dhb = Vec 0 1 1
    ab = Segment dwl a b dwr
    dc = Segment dwl d c dwr
