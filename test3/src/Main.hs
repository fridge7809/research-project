module Main (main) where
import Test.QuickCheck
import Name.Rat
import Name.Generators
import Name.Utilities
import Name.Properties


main :: IO ()
main = do
    value <- generate (arbitrary :: Gen (Sig Int))
    value2 <- generate (arbitrary :: Gen (Sig Int))
    let final = prop_zip_zipped value value2
    let stripped = strip final
    putStrLn (show value)
    putStrLn (show value2)
    putStrLn (show final)
    putStrLn (show stripped)
