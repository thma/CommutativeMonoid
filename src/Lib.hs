module Lib where

import           Control.Parallel (par)
import           Control.Parallel.Strategies (using, parMap, rpar)

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
    mapResult `par` reduceResult
    where mapResult    = parMap rpar mapFunc input
          reduceResult = reduceFunc mapResult `using` rpar
          
(⊕) :: String -> String -> String
(⊕) a b = a ++ b


text :: [String]
text = words "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."

-- " olleh lla raed sklof"

text1 :: [String]
text1 =  [" olleh"," ym"," raed"," sklof"]          

--test :: [Integer]
test = foldMap reverse ["hello", "my", "old", "friend"]
