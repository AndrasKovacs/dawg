
import qualified Data.DAWG.Packed as D
import Paths_packed_dawg
import Data.List
import Data.Maybe

import Test.HUnit
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

prop_preserve_elems :: Property
prop_preserve_elems = forAll (arbitrary :: Gen [String])
    (\xs -> (sort $ D.toList $ D.fromList xs) == (sort $ nub xs))

prop_elem :: [String] -> Property
prop_elem xs = (not $ null xs) ==> 
    forAll (elements xs) (\x -> D.elem x dawg) 
    where dawg = D.fromList xs

prop_discriminate_elems :: [String] -> Property
prop_discriminate_elems xs = (not $ null xs) ==>
    forAll (arbitrary :: Gen String) 
        (\x -> elem x xs == D.elem x dawg) 
    where dawg = D.fromList xs

prop_lookupPrefix :: [String] -> Property
prop_lookupPrefix xs = (not $ null xs) ==> 
    forAll (elements xs >>= elements . inits) 
        (\x -> isJust (D.lookupPrefix x dawg))
    where dawg = D.fromList xs

test_twl06 :: FilePath -> Assertion
test_twl06 path = do 
    ws <- fmap lines $ readFile path
    ws @?= (sort $ D.toList $ D.fromList ws)

test_serialization :: FilePath -> Assertion
test_serialization path = do
    dawg <- fmap (D.fromList . lines) $ readFile path
    D.toFile "twl06.dawg" dawg
    dawg' <- D.fromFile "twl06.dawg"
    dawg @?= dawg'


main = do
    dictPath <- getDataFileName "TWL06.txt"
    defaultMain [
        testGroup "Properties" [
            testProperty "preserving source elements" prop_preserve_elems,
            testProperty "elem" prop_elem,
            testProperty "discriminate elem" prop_discriminate_elems,
            testProperty "lookupPrefix" prop_lookupPrefix],
        testCase "twl06 DAWG validity" (test_twl06 dictPath),
        testCase "serialization" (test_serialization dictPath)
        ] 



