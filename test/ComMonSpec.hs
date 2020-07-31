{-# LANGUAGE FlexibleInstances #-}

module ComMonSpec where

import           Test.Hspec            hiding (it)
import           TestUtils        (it)
import           Test.QuickCheck

import Linear
import Lib

instance Arbitrary (V3 (V3 Integer)) where
    arbitrary = do
      v1 <- arbitrary
      v2 <- arbitrary
      v3 <- arbitrary
      v4 <- arbitrary
      v5 <- arbitrary
      v6 <- arbitrary
      v7 <- arbitrary
      v8 <- arbitrary
      v9 <- arbitrary
      
      return $ V3 (V3 v1 v2 v3) (V3 v4 v5 v6) (V3 v7 v8 v9)

spec :: Spec
spec =
  describe "commutative Monoid" $ do

    it "parallel and sequential evaluation return the same result" $
      property $ \a b c d e f g h j k l m o p q r s t u v w x y z-> parMapReduce id matrixReduce [a, b, c, d,e, f, g, h, j, k, l, m, o, p, q, r, s, t, u, v, w, x, y, z]  `shouldBe` simpleMapReduce id matrixReduce [a, b, c, d,e, f, g, h, j, k, l, m, o, p, q, r, s, t, u, v, w, x, y, z]



--      m = simpleMapReduce id matrixReduce [a,b,a,b]
--      m' = simpleMapReduce id matrixReduce [b,a,b,a]
--
--      m'' = parMapReduce id matrixReduce [a,b,a,b]

--    it "computes the manhattanDistance of 0.0-1.1" $
--     manhattanDistance(Point 0 0, Point 1 1) === 2
--
--    it "computes the manhattanDistance of 1.1-1.2" $
--     manhattanDistance(Point 1 1, Point 1 2) `shouldBe` 1
--
--    it "computes the manhattanDistance of 1.1-2.2" $
--     manhattanDistance(Point 1 1, Point 2 2) `shouldBe` 2
--
--    it "computes the manhattanDistance of 2.2-5.4" $
--     manhattanDistance(Point 2 2, Point 5 4) === 5
--
--    it "computes the manhattanDistance of 5.4-2-2" $
--     manhattanDistance(Point 5 4, Point 2 2) === 5





