module Lib where

import           Control.Parallel (pseq)
import           Control.Parallel.Strategies (rseq, using, parMap)

simpleMapReduce 
  :: (a -> b)   -- map function
  -> ([b] -> c) -- reduce function
  -> [a]        -- list to map over
  -> c          -- result
simpleMapReduce mapFunc reduceFunc = reduceFunc . map mapFunc


parMapReduce 
  :: (a -> b)   -- map function
  -> ([b] -> c) -- reduce function
  -> [a]        -- list to map over
  -> c          -- result
parMapReduce mapFunc reduceFunc input =
    mapResult `pseq` reduceResult
    where mapResult    = parMap rseq mapFunc input
          reduceResult = reduceFunc mapResult `using` rseq


