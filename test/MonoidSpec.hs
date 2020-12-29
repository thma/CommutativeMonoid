{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MonoidSpec where

import Lib

import           Test.Hspec            --hiding (it)
-- import           TestUtils             (it)
import           Test.QuickCheck
import GHC.Natural (Natural, naturalFromInteger)

instance Arbitrary Natural where
  arbitrary = do
    NonNegative nonNegative <- arbitrary
    return $ naturalFromInteger nonNegative

-- see https://stackoverflow.com/questions/42764847/is-there-a-there-exists-quantifier-in-quickcheck
exists :: (Show a, Arbitrary a) 
       => (a -> Bool) -> Property
exists = forSome $ resize 10000 arbitrary
    
forSome :: (Show a, Testable prop)
        => Gen a -> (a -> prop) -> Property
forSome gen prop = once $ disjoin $ replicate 10000 $ forAll gen prop

spec :: Spec
spec = do
  describe "The Monoid 'Natural Numbers under Addition'" $ do
    it "is associative" $
      property $ \x y z -> ((x + y) + z) `shouldBe` ((x + (y + z)) :: Natural)
      
    it "has 0 as left and right identity element" $
      property $ \x -> (x + 0 `shouldBe` (x :: Natural)) .&&. (0 + x `shouldBe` x)
      
    it "is commutative" $
      property $ \x y -> x + y `shouldBe` (y + x :: Natural)

    it "is commutative (via forAll)" $
      forAll (resize 10000 arbitrary) $ \(x, y) -> x + y == (y + x :: Natural)
    
--    it "is not commutative (via exists)" $
--      exists $ \(x, y) -> x + y /= (y + x :: Natural)
  
  
  describe "The Monoid 'Strings under concatenation'" $ do
    
    it "is associative" $ 
      property $ \x y z -> ((x ⊕ y) ⊕ z) `shouldBe` (x ⊕ (y ⊕ z))
      
    it "has \"\" as left and right identity element" $
      property $ \x -> (x ⊕ "" `shouldBe` x) .&&. ("" ⊕ x `shouldBe` x)
      
--    it "is NOT commutative" $
--      property $ \x y -> x ⊕ y `shouldNotBe` y ⊕ x
--
--    it "is not commutative (with filtering)" $
--      property $ \x y ->  x /= "" && y /= "" && x /= y
--                          ==> (x ⊕ y) /= (y ⊕ x)

    it "is not commutative (via exists)" $
      exists $ \(x,y) -> x ⊕ y /= y ⊕ x

    it "works correctly with a sequential map-reduce" $
      property $ \a b c d -> (simpleMapReduce reverse (foldr (⊕) "") [a,b,c,d]) 
                     `shouldBe` (reverse a) ⊕ (reverse b) ⊕ (reverse c) ⊕ (reverse d)

    it "has some cases where parallel reduction deviates from sequential reduction" $
      exists $ \() -> parMapReduce reverse (foldr (⊕) "") text
                  /= simpleMapReduce reverse (foldr (⊕) "") text
    





