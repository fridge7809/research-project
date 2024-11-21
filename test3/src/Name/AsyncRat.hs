{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

-- AsyncRattus code goes here. 
-- The code is type checked by the AsyncRattus compiler plugin.

module Name.AsyncRat (
    aRatZip
) where

import AsyncRattus.Signal
import AsyncRattus.Strict
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

aRatZip :: Sig Int -> Sig Int -> Sig (Int :* Int)
aRatZip a b = zip a b