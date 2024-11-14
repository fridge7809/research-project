{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}

module Name.Rat (
    prop_zip_zipped
) where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import AsyncRattus.Strict
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

prop_zip_zipped :: Sig Int -> Sig Int -> Sig (Int :* Int)
prop_zip_zipped a b = zip a b