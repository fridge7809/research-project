{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
module Main (main) where
import Test.QuickCheck
import Name.AsyncRat
import Name.Generators
import Name.Properties


main :: IO ()
main = do
    xs <- generate (arbitrary :: Gen (Sig Int))
    ys <- generate (arbitrary :: Gen (Sig Int))
    let stutter = stuttering xs ys
    let zipped = aRatZip xs ys
    let isStuttering = prop_is_stuttering xs ys
    putStrLn (show xs)
    putStrLn (show ys)
    putStrLn (show zipped)
    putStrLn (show stutter)
    putStrLn (show isStuttering)