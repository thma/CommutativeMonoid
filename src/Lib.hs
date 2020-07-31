module Lib where

import Linear ((^+^))
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix ((!*!), (!*), (*!))
import Linear.Vector (zero)

import           Control.Parallel (pseq)
import           Control.Parallel.Strategies (rseq, using, parMap)

position = V2 1 0

vec = V3 2 2 2



simpleMapReduce ::
     (a -> b)   -- map function
  -> ([b] -> c) -- reduce function
  -> [a]        -- list to map over
  -> c          -- result
simpleMapReduce mapFunc reduceFunc = reduceFunc . map mapFunc

parMapReduce :: (a -> b) -> ([b] -> c) -> [a] -> c
parMapReduce mapFunc reduceFunc input =
    mapResult `pseq` reduceResult
    where mapResult    = parMap rseq mapFunc input
          reduceResult = reduceFunc mapResult `using` rseq


matrixReduce :: [V3 (V3 Integer)] -> V3 (V3 Integer)
matrixReduce = foldr (!*!) one
  where one = V3 (V3 1 1 1) (V3 1 1 1) (V3 1 1 1)


someFunc :: IO ()
someFunc = do
  let a = V3 (V3 1 2 3) (V3 2 3 4) (V3 3 4 5)
      --a' = V4 (V3 1 2 3) (V3 2 3 4) (V3 3 4 5) (V3 4 5 6)
      b = V3 (V3 2 3 5) (V3 7 11 13) (V3 17 19 23)
      
      m = simpleMapReduce id matrixReduce [a,b,a,b] 
      m' = simpleMapReduce id matrixReduce [b,a,b,a] 
      
      m'' = parMapReduce id matrixReduce [a,b,a,b] 
      m''' = parMapReduce id matrixReduce [a,b,a,b] 
      
--      c = a !*! b :: V4 (V4 Integer)
--      c' = b !*! a
--      d = V4 1 2 3 4 :: (V4 Integer)
--      e = c !* d
--      f = d *! c
--      g = V4 (V2 2 3) (V2 4 5) (V2 6 7) (V2 8 9)
--      h = (a !*! b) !*! g
--      i = a !*! (b !*! g)
--      --j = (b !*! a) !*! g

  print m
  print m'
  print m''
  print m'''

