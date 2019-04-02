-- @Author: ariesduanmu
-- @Date:   2019-03-24 17:47:00
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-03-24 18:36:25
import System.IO
import Control.Monad
import Data.List

game :: IO()
game = do
    results <- replicateM 3 $ do
        input_line <- getLine
        let mountainh = read input_line :: Int
        return mountainh
    let r = [(results!!i, i) | i <- [0..length results-1]]
    putStrLn (show (snd (last (sort r))))