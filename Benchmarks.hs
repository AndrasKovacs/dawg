import Criterion.Main
import Criterion.Types
import Data.DAWG.Packed
import Paths_packed_dawg

test =  defaultMainWith (defaultConfig {timeLimit = 10})
 
main = do
    dictPath <- getDataFileName "TWL06.txt"
    ws <- fmap lines $ readFile dictPath
    let dawg = fromList ws
    test [
        bench "dawg generation" $ whnf fromList ws,
        bench "enlist suffixes" $ nf toList dawg,
        bench "serialize"       $ nfIO $ toFile "twl06.dawg" dawg,
        bench "deserialize"     $ nfIO $ fromFile "twl06.dawg"
        ]   
