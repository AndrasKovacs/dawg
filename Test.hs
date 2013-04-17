
import qualified Data.DAWG as D
import Data.List
import Data.Maybe

import Test.HUnit
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Data.Monoid

prop_preserve_elems :: Property
prop_preserve_elems = forAll (arbitrary :: Gen [String])
    (\xs -> (sort $ D.toList $ D.fromList xs) == (sort $ nub xs))

prop_elem :: [String] -> Property
prop_elem xs = (not $ null xs) ==> collect (length xs) $
    forAll (elements xs) (\x -> D.elem x dawg)
    where dawg = D.fromList xs

prop_discriminate_elems :: [String] -> Property
prop_discriminate_elems xs = (not $ null xs) ==> collect (length xs) $
    forAll (arbitrary :: Gen String) 
        (\x -> elem x xs == D.elem x dawg) 
    where dawg = D.fromList xs


prop_lookupPrefix :: [String] -> Property
prop_lookupPrefix xs = (not $ null xs) ==> collect (length xs) $
    forAll (elements xs >>= elements . inits) 
        (\x -> isJust (D.lookupPrefix x dawg))
    where dawg = D.fromList xs
 
    
main = do

    defaultMain [
        testProperty "preserving source elements" prop_preserve_elems,
        testProperty "elem" prop_elem,
        testProperty "discriminate elem" prop_discriminate_elems,
        testProperty "lookupPrefix" prop_lookupPrefix
        ] 



