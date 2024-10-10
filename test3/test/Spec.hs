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

instance Stable Int where
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
    return (x ::: never)
arbitrarySig n = do
    x <- arbitrary
    xs <- arbitrarySig (n - 1) -- not evaluated yet?
    list <- arbitrary :: Gen [Int]
    let later = Delay (IntSet.fromList list) (\_ -> xs)
    return (x ::: later)

-- headache tracker
-- quickcheck requires the signal data type to derive (implement) show (aka .toString) and arbitrary
-- show is esentially an interface (typeclass) that defines functions the data type must implement (derive)
-- to show first n elements, we must implement take n elements but probasbly need to use async rattus modalities
-- how to define take with box, later, adv etc??
-- we must define some equality operator that compares first n elements of a signal using take function
-- practice using the asyncrattus api, specifically primitives delay, adv, box, unbox
-- we dont want the compiler plugin to type check our code, but our type checked async rattus code depends on non type checked code
-- how do we avoid a cyclic dependency between non-type checked and type checked code

-- implement show typeclass for show Sig a
instance Show a => Show (Sig a) where
  show (x ::: xs) = "Sig " ++ show (takeSigExhaustive (x ::: xs)) ++ " clocky: " ++ show (extractClock xs)

-- 30 should probably not be hard coded but it is for now
instance Eq a => Eq (Sig a) where
    (==) sig1 sig2 = takeSigExhaustive sig1 == takeSigExhaustive sig2

takeSig :: Int -> Sig a -> [a]
takeSig 0 _ = []
takeSig n (x ::: Delay cl f) = x : takeSig (n-1) (f (InputValue 0 ()))

takeSigExhaustive :: Sig a -> [a]
takeSigExhaustive (x ::: Delay cl f) = 
    if IntSet.null cl then
        []
    else x : takeSigExhaustive (f (InputValue 0 ()))

sizeSig :: Sig a -> Int -> Int
sizeSig (x ::: Delay cl f) acc =
    if IntSet.null cl then
        acc
    else sizeSig (f (InputValue 0 ())) (acc+1)

ints :: Sig Int
ints = 0 ::: Delay (IntSet.singleton 0) (\_ -> ints)

-- how do we make quickcheck work for polymorphic types
-- what are the considerations when generating clocks
-- totally random, at least one, overlap between?
-- should we maximise "clock tick" contention in some cases, minimize it in other cases
-- should we have a syncronous clock generator
-- should we have a constant clock generator

-- Property ideas for map. Specification for map:
-- => type constraint, arguments must derive typeclasses eq, arb and show

-- 1. Mapping id function is the signal itsself
-- 2. Mapping does not change the size of the signal, only content
-- 3. Mapping is assosciatve
-- 4. Causality??

-- 1.
-- prop_map_id :: (Eq a, Arbitrary a, Show a) => Positive Int -> Sig a -> Bool
-- prop_map_id (Positive n) sig = eqSig n (map (box id) sig) sig

prop_id :: Sig Int -> Bool
prop_id sig = sig == (map (box (id)) sig)

-- 2.
-- Property map is associative, f after g. map f sig (map g sig) == map (g . f) sig
-- prop_map_associative :: (Eq a, Arbitrary a, Show a) => Positive Int -> Sig a -> Bool

prop_associative :: Box (Int -> Int) -> Sig Int -> Bool
prop_associative f sig = do
    let composed = unbox f . unbox f
    map f (map f sig) == map (box composed) sig

-- 3.
-- Property map does not change the size
-- prop_map_size :: (Eq a, Arbitrary a, Show a) => Positive Int -> Sig a -> Bool


prop_size :: Box (Int -> Int) -> Sig Int -> Bool
prop_size f sig = 
    sizeSig sig 0 == sizeSig (map f sig) 0

-- zip property ideas

-- 1.
-- zip with a constant empty signal is the signal itself
-- hacky by making stable int. why do we need this

prop_zip_const :: Box (Int -> Int -> Int) -> Sig Int -> Bool
prop_zip_const f sig = zipWith (f) (sig) (0 ::: never) == sig

-- 2.
-- zip length increases with each tick of distinct streams by 2, except when they tick at the same time, in which case it only increases by one
-- this test is trivial untill we make an arbitrary clock generator

-- 3.
-- zip associativity hold for async streams zip A (zip A B) == zip (zip A B) A

prop_zip_associative :: Box (Int -> Int -> Int) -> Sig Int -> Sig Int -> Bool
prop_zip_associative f sig1 sig2 = zipWith f sig1 (zipWith f sig1 sig2) == zipWith f (zipWith f sig1 sig2) sig1
    

main :: IO ()
main = do
    quickCheck (prop_zip_associative (box (+)))
    value <- generate (arbitrary :: Gen (Sig Int))
    print (sizeSig value 0)