import Test.QuickCheck
import Name.Properties (prop_is_stuttering)

main :: IO ()
main = do
    quickCheck prop_is_stuttering
