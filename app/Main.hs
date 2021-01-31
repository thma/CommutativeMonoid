module Main where

import Lib ( parMapReduce, text )

-- to produce a ThreadScope eventlog execute as
-- CommutativeMonoid +RTS -ls -N
-- then execute threadscope CommutativeMonoid.eventlog

main :: IO ()
main = do
  let result = parMapReduce reverse concat text
  print result
