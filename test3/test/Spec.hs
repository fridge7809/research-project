import Test.QuickCheck
import Name.Properties


main :: IO ()
main = do
    quickCheck prop_zip_then_strip_sig
