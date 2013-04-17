import Criterion.Main
import Criterion.Config
import Data.DAWG

test =  defaultMainWith (defaultConfig  {cfgSamples = ljust 5}) (return())

main = do
    ws <- fmap lines $ readFile "TWL06.txt"
    test [
        bench "dawg generation" $ print $ value $ fromList ws]
