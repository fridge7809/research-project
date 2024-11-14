{-# LANGUAGE TypeOperators #-}

module Name.Properties (
    prop_map_id,
    prop_map_associative,
    prop_map_size,
    prop_map_id_Int,
    prop_map_id_Float,
    prop_zip_isStuttering,
    strip
) where

import AsyncRattus.Signal
import AsyncRattus.InternalPrimitives
import AsyncRattus.Strict
import Name.Generators
import Name.Utilities
import Name.Rat
import Test.QuickCheck
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

strip :: Sig (Int:*Int) -> Sig Int
strip a = do
    let boxed = Box snd'
    map boxed a


instance Stable Int where
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

prop_map_id :: Eq a => Sig a -> Bool
prop_map_id sig = sig == map (box id) sig

prop_map_id_Int :: Sig Int -> Bool
prop_map_id_Int = prop_map_id

prop_map_id_Float :: Sig Float -> Bool
prop_map_id_Float = prop_map_id

-- 2.
-- Property map is associative, f after g. map f sig (map g sig) == map (g . f) sig
-- prop_map_associative :: (Eq a, Arbitrary a, Show a) => Positive Int -> Sig a -> Bool

prop_map_associative :: Box (Int -> Int) -> Sig Int -> Bool
prop_map_associative f sig = do
    let composed = unbox f . unbox f
    map f (map f sig) == map (box composed) sig

-- 3.
-- Property map does not change the size
-- prop_map_size :: (Eq a, Arbitrary a, Show a) => Positive Int -> Sig a -> Bool


prop_map_size :: Box (Int -> Int) -> Sig Int -> Bool
prop_map_size f sig =
    sizeSig sig 0 == sizeSig (map f sig) 0

-- zip property ideas

-- 1.
-- zip with a constant empty signal is the signal itself
-- hacky by making stable int. why do we need this

-- -- could not deduce stable type a

-- prop_zip_const :: (Eq a, Arbitrary a, Show a) => Box (a -> a -> a) -> Sig a -> Bool
-- prop_zip_const sig = zip sig (0 ::: never) == sig

-- 2.
-- zip length increases with each tick of distinct streams by 2, except when they tick at the same time, in which case it only increases by one
-- this test is trivial untill we make an arbitrary clock generator

-- 3.
-- zip associativity hold for async streams zip A (zip A B) == zip (zip A B) A

-- could not deduce stable type a

-- prop_zip_associative :: (Eq a, Arbitrary a, Show a) => Box (a -> a -> a) -> Sig a -> Sig a -> Bool
-- prop_zip_associative f sig1 sig2 = zipWith f sig1 (zipWith f sig1 sig2) == zipWith f (zipWith f sig1 sig2) sig1

-- POTENTIALLY stuttering
-- either list 1 equals list 2
-- or list 2 is a stutter of list 1, where some element stutters 0 .. n times


prop_zip_isStuttering :: [Int] -> [Int] -> Bool
prop_zip_isStuttering a b
    | length b < length a = False                 
    | otherwise           = prop_zip_isStuttering' a b 0


prop_zip_isStuttering' :: [Int] -> [Int] -> Int -> Bool
prop_zip_isStuttering' _ [] _ = True
prop_zip_isStuttering' [] (_:_) _ = False
prop_zip_isStuttering' (x:xs) (y:ys) n
    | n == 2      = False                                   
    | x == y      = prop_zip_isStuttering' (x:xs) ys 0      
    | otherwise   = prop_zip_isStuttering' xs (y:ys) (n + 1)

prop_zip_then_strip :: Sig Int -> Sig Int -> [Int]
prop_zip_then_strip a b = do
        let zipped = prop_zip_zipped a b
        let stripped = strip zipped
        takeSig 50 stripped


-- prop_zip_isStutteringSig :: Sig Int -> Sig Int -> Bool
-- prop_zip_isStutterignSig _ 

-- filter properties

-- 1. filter (const True) = id

-- 2. filter can only reduce or maintain the size of the signal; it never adds new elements.

-- 3. filter is imdepotent, subsequent filter calls on the same signal does not alter the signal

-- 4. filter composition