module Lib
    ( someFunc
    ) where

import Linear ((^+^))
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix ((!*!), (!*), (*!))

position = V2 1 0

vec = V3 2 2 2

someFunc :: IO ()
someFunc = do
  let a = V4 (V3 1 2 3) (V3 2 3 4) (V3 3 4 5) (V3 4 5 6)
      b = V3 (V4 1 2 3 4) (V4 2 3 4 5) (V4 3 4 5 6)
      c = a !*! b :: V4 (V4 Integer)
      c' = b !*! a
      d = (V4 1 2 3 4) :: (V4 Integer)
      e = c !* d
      f = d *! c
      g = V4 (V2 2 3) (V2 4 5) (V2 6 7) (V2 8 9)
      h = (a !*! b) !*! g
      i = a !*! (b !*! g)
      --j = (b !*! a) !*! g

  print c
  print c'
  print e
  print f
  print h
  print i
  --print j

