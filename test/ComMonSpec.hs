{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ComMonSpec where

import           Test.Hspec            hiding (it)
import           TestUtils        (it)
import           Test.QuickCheck
import           Prelude hiding (concat)
import Lib
import qualified Test.QuickCheck.Property as P (reason, callbacks, mapResult)
import GHC.Natural (Natural)

concat :: String -> String -> String
concat a b = a ++ b

--forAll :: (Show a, Testable prop)
--       => Gen a -> (a -> prop) -> Property

exists :: (Show a, Arbitrary a) => (a -> Bool) -> Property
exists pred = disjoin $ replicate 100 $ forAll (resize 500 arbitrary) pred

take5 :: String -> String
take5 = take 5



spec :: Spec
spec =
  describe "String concat Monoid" $ do
    
    it "is associative" $ 
      property $ \a b c -> (((a ++ b) ++ c) :: String) `shouldBe` (a ++ (b ++ c))

    it "is not commutative" $
      exists $ \((x,y) :: (String, String)) -> x ++ y /= y ++ x



--    it "parallel and sequential evaluation return the same result" $
--      property $ \a b c d e f g h j k l m o p q r s t u v w x y z 
--        -> (parMapReduce id concat [a, b, c, d, e, f, g, h, j, k, l, m, o, p, q, r, s, t, u, v, w, x, y, z]  )
--        `shouldBe` (simpleMapReduce id concat [a, b, c, d, e, f, g, h, j, k, l, m, o, p, q, r, s, t, u, v, w, x, y, z])



--      m = simpleMapReduce id matrixReduce [a,b,a,b]
--      m' = simpleMapReduce id matrixReduce [b,a,b,a]
--
--      m'' = parMapReduce id matrixReduce [a,b,a,b]







