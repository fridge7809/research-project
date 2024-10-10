{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import AsyncRattus
import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import Test.QuickCheck
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

-- arbitrary class from quickcheck, defining a custom instance of the class to handle arbitrary generation of Sig a
-- we used "sized" from quickcheck to avoid infinite recursion

-- client calls an arbitrarysig of size n
-- custom implementation of typeclass to define how to generate an arbitrary sig a
-- We supply arbitrarySig as the (fixed sized) generator Gen (Sig a). sized :: (Int -> Gen a )-> Gen A

instance (Arbitrary a) => Arbitrary (Sig a) where
    arbitrary = sized arbitrarySig

-- could also look into a probability based implementation that generates O (a) with lower probability
-- ... but then we couldnt promise the count of values needed for take and show

-- naive implementations reccurs infinitely 
{- instance Arbitrary a ⇒ Arbitrary (Sig a) where
    arbitrary = do
        x <- arbitrary :: Gen a
        s <- getSize
        if s > 0 then
            return x ::: (do xs <- reSize(n-1) arbitrary)
            return $ x ::: Delay (Singleton 0) (\_ -> xs)

            else
            return x ::: (xs ::: never)
 -}

-- => type constraint, arguments must derive arbitrary typeclass
arbitrarySig :: Arbitrary a => Int -> Gen (Sig a)
-- base case
-- same as scala for comprehension, sugar to chain monadic calls in an imperative way
arbitrarySig 0 = do
    x <- arbitrary
    return (const x)

arbitrarySig n = do
    x <- arbitrary
    xs <- arbitrarySig (n - 1)
    return (x ::: delay xs)


-- headache tracker
-- quickcheck requires the signal data type to derive (implement) show (aka .toString) and arbitrary
-- show is esentially an interface (typeclass) that defines functions the data type must implement (derive)
-- to show first n elements, we must implement take n elements but probasbly need to use async rattus modalities
-- how to define take with box, later, adv etc??
-- we must define some equality operator that compares first n elements of a signal using take function
-- practice using the asyncrattus api, specifically primitives delay, adv, box, unbox

-- implement show typeclass for show Sig a
instance Show a => Show (Sig a) where
  show sig = "Sig " ++ show (takeSig 20 sig)

-- 30 should probably not be hard coded but it is for now
instance Eq a => Eq (Sig a) where
    (==) sig1 sig2 = takeSig 30 sig1 == takeSig 30 sig2

takeSig :: Int -> Sig a -> [a]
takeSig 0 _ = []
takeSig n (x ::: Delay cl f) = x : takeSig (n-1) (f (InputValue 0 ()))

ints :: Sig Int
ints = 0 ::: Delay (IntSet.singleton 0) (\_ -> ints)

-- Property ideas for map. Specification for map:

-- 1. Mapping id function is the signal itsself
-- 2. Mapping does not change the size of the signal, only content
-- 3. Mapping is assosciatve
-- 4. Perhaps also being able to test causality

-- Making a signal with negative n values makes no sense, therefore Positive n
-- This needs to be refactored to make id a user supplied function to map over the signal with
-- => type constraint, arguments must derive typeclasses eq, arb and show
-- prop_map_id :: (Eq a, Arbitrary a, Show a) => Positive Int -> Sig a -> Bool
-- prop_map_id (Positive n) sig = eqSig n (map (box id) sig) sig

prop_test :: Positive Int -> Sig Int -> Bool
prop_test (Positive n) sig = sig == sig

-- Property map is associative, f after g. map f sig (map g sig) == map (g . f) sig
-- prop_map_associative :: (Eq a, Arbitrary a, Show a) => Positive Int -> Sig a -> Bool

-- Property map does not change the size
-- prop_map_size :: (Eq a, Arbitrary a, Show a) => Positive Int -> Sig a -> Bool

main :: IO ()
main = quickCheck (prop_test (Positive 5) ints)