-- @Author: ariesduanmu
-- @Date:   2019-03-24 18:51:45
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-03-24 18:54:01
import System.IO
import Control.Monad
import Data.List

game :: IO()
game = do
    input_line <- getLine
    let n = read input_line :: Int
    input_line <- getLine
    let input = words input_line

    temperates <- forM [0..(n-1)] $ \i -> do
        let t = read (input!!(i)) :: Int -- a temperature expressed as an integer ranging from -273 to 5526
        return t
    let s = sortOn (\x -> abs x) (sortOn (\x -> (-1)*x) temperates)
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write answer to stdout
    if n>0 then
        putStrLn (show (s!!0))
    else
        putStrLn "0"
    return ()