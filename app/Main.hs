module Main where

import Lib ( parMapReduce, text )

fib :: Integer -> Integer
fib = fst . fib2

-- | Return (fib n, fib (n + 1))
fib2 :: (Integral a1, Num a2) => a1 -> (a2, a2)
fib2 0 = (1, 1)
fib2 1 = (1, 2)
fib2 n
 | even n    = (a*a + b*b, c*c - a*a)
 | otherwise = (c*c - a*a, b*b + c*c)
 where (a,b) = fib2 (n `div` 2 - 1)
       c     = a + b

main :: IO ()
main = do
  -- let nums = [20..400]
  -- let result = parMapReduce (force . show . fib) concat nums
  -- print result
  let result = parMapReduce reverse concat [" olleh"," ym"," raed"," sklof"]
  print result
