{-# OPTIONS -fplugin=Rattus.Plugin #-}
module Main (main) where

import Rattus
import Rattus.Stream

incStream :: Str Int -> Str Int
incStream (x ::: xs) = (x + 1) ::: delay (incStream (adv xs))

constStream :: Int -> Str Int
constStream x = x ::: delay (constStream x)

main :: IO ()
main = do
  -- Generate a constant stream of 5's
  let stream = constStream 5
  -- Apply the increment function to the stream
  let incStreamResult = incStream stream
  -- Print the first 5 elements of the incremented stream
  printStream 5 incStreamResult

printStream :: Int -> Str Int -> IO ()
printStream 0 _ = return ()
printStream n (x ::: xs) = do
  print x
  printStream (n - 1) (adv xs)