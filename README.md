# Proving me wrong — How QuickCheck destroyed my favourite theory

## Introduction

Quite a while back I wrote a larger article on patterns in functional programming
which also covered the [MapReduce algorithm](https://thma.github.io/posts/2018-11-24-lambda-the-ultimate-pattern-factory.html#map-reduce).

In that article I provided very simple implementation for sequential and parallel MapReduce 
implementations based on [RealWorld Haskell](http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html).

At that time I was researching the algebraic foundation of software patterns. 
So I digged out a paper on [algebraic properties of distributed big data analytics](https://pdfs.semanticscholar.org/0498/3a1c0d6343e21129aaffca2a1b3eec419523.pdf),
which explained that a MapReduce will always work correctly when the intermediate data structure resulting from the
`map`-phase is a Monoid under the `reduce` operation.

For some reason, I was not convinced that this Monoid-condition was enough, because all the typical examples
like word-frequency maps are even **commutative** Monoids under the respective reduce operation.

So theory that I came up with was:

> Only if the intermediate data structure resulting from the `map`-phase is a **commutative Monoid** 
> under the `reduce` operation, then a massive parallel MapReduce will produce correct results.

I tried to prove this property using the Haskell 
[QuickCheck property based test framework](https://wiki.haskell.org/Introduction_to_QuickCheck2).

Interestingly QuickCheck was able to find counter examples which proved my theory wrong!
This finally convinced me that I was wrong, and after some deeper thinking I was also able to understand why.

I was impressed by the power of QuickCheck and thus thought it might me a good idea to share 
this lesson in falsification.

## Commutative Monoids

In abstract algebra, a monoid is a set equipped with an associative 
binary operation and an identity element.

The Simplest example are the natural numbers under addition with 0 as the identity (or neutral) element. 
We can use QuickCheck to verify that indeed the Monoid laws are maintained.

If we want to use `GHC.Natural` type to represent natural numbers, 
we first have to make `Natural` instantiate the `Arbitrary` type class which is
used by QuickCheck to automatically generate test data:

```haskell
import           Test.QuickCheck (Arbitrary, arbitrary, NonNegative (..))
import           GHC.Natural     (Natural, naturalFromInteger)

instance Arbitrary Natural where
  arbitrary = do
    NonNegative nonNegative <- arbitrary
    return $ naturalFromInteger nonNegative
```

Now we can start to write our property based tests. For algebraic structures it is
straightforward to come up with properties: we just write the required
laws (associativity, 0 is identity element and commutativity) as properties.

I am using Hspec as a wrapper around QuickCheck as it provides a very nice testing DSL which makes
it easy to read the code and the output of the test suite:

```haskell
import           Test.Hspec

spec :: Spec
spec = do
  describe "The Monoid 'Natural Numbers under Addition'" $ do
    it "is associative" $
      property $ \x y z -> ((x + y) + z) `shouldBe` ((x + (y + z)) :: Natural)
      
    it "has 0 as left and right identity element" $
      property $ \x -> (x + 0 `shouldBe` (x :: Natural)) .&&. (0 + x `shouldBe` x)
      
    it "is commutative" $
      property $ \x y -> x + y `shouldBe` (y + x :: Natural)
```

The output of these tests will be as follows:

```bash
Monoid
  The Monoid 'Natural Numbers under Addition'
    is associative
      +++ OK, passed 100 tests.
    has 0 as identity (or neutral) element
      +++ OK, passed 100 tests.
    is commutative
      +++ OK, passed 100 tests.
```

So behind the scenes, QuickCheck has generated test data for 100 test cases for each
property under test. For all these data the test cases passed.

This is definitely not a proof. But it gives us some confidence that our math text-books
are correct when giving Natural Numbers under addition as an example for a commutative Monoid.

OK, that was easy! Now let's move to non-commutative Monoids.

## Non-commutative Monoids

Strings (or any other Lists) under concatenation are a typical example. 
It's easy to see that `"hello" ++ ("dear" ++ "people")` equals `"(hello" ++ "dear") ++ "people"`,
but that `"hello" ++ "world"` differs from `"world" ++ "hello"`.

Now let's try to formalize these intuitions as QuickCheck property based tests again.

First I'm introducing an alias for `(++)`, as it is defined on any list type,
it would be required to have type signatures in all properties (as we had all those `:: Natural` 
signatures in the examples above). So I define an operation `(⊕)` which is
only available on `String`s:

```haskell
(⊕) :: String -> String -> String
(⊕) a b = a ++ b
```

Now we can extend our test suite with the following test cases:

```haskell
  describe "The Monoid 'Strings under concatenation'" $ do
    
    it "is associative" $ 
      property $ \x y z -> ((x ⊕ y) ⊕ z) `shouldBe` (x ⊕ (y ⊕ z))
      
    it "has \"\" as left and right identity element" $
      property $ \x -> (x ⊕ "" `shouldBe` x) .&&. ("" ⊕ x `shouldBe` x)
```

The output looks promising:

```bash
  The Monoid 'Strings under concatenation'
    is associative
      +++ OK, passed 100 tests.
    has "" as left and right identity element
      +++ OK, passed 100 tests.
```

Now let's try to test the non-commutativity:

```haskell
    it "is NOT commutative" $
      property $ \x y -> x ⊕ y `shouldNotBe` y ⊕ x
```

But unfortunately the output tells us that this is not true:

```bash
    is NOT commutative FAILED [1]

  1) Monoid, The Monoid 'Strings under concatenation', is NOT commutative
       Falsifiable (after 1 test):
         ""
         ""
       not expected: ""
```

We formulated the property in the wrong way. The `(⊕)` *may be commutative for some*
edge cases, e.g. when one or both of the arguments are `""`.
But it is not commutative *in general* – that is for all possible arguments.

We could rephrase this property as *"There exists at least one pair of arguments
for which `(⊕)` is not commutative"*.

QuickCheck does not ships with a mechanism for *existential quantification*. 
But as is has `forAll` that is *universal quantification*. So we can build our own
tool for existential quantification 
[based on a discussion on Stackoverflow](https://stackoverflow.com/questions/42764847/is-there-a-there-exists-quantifier-in-quickcheck).

```haskell
exists :: (Show a, Arbitrary a) 
       => (a -> Bool) -> Property
exists = forSome $ resize 100 arbitrary
    
forSome :: (Show a, Testable prop)
        => Gen a -> (a -> prop) -> Property
forSome gen prop = once $ disjoin $ replicate 100 $ forAll gen prop
```

Now we can rewrite the property "There exists at least one pair of arguments
for which `(⊕)` is not commutative" as follows:

```haskell
    it "is not commutative (via exists)" $
      exists $ \(x,y) -> x ⊕ y /= y ⊕ x
```

