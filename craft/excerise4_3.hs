-- @Author: ariesduanmu
-- @Date:   2019-02-27 01:01:19
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-02-27 02:05:41

data Move = Rock | Paper | Scissors deriving (Show, Eq)

beat :: Move -> Move

beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose _ = Paper


data Result = Win | Lose | Draw deriving (Show, Eq)

outcome :: Move -> Move -> Result

outcome a b
  | (beat a) == b = Lose
  | (lose a) == b = Win
  | otherwise = Draw

data Season = Spring | Summer | Autumn | Winter deriving (Show, Eq, Ord)
data Temp = Cold | Hot deriving (Eq, Show, Ord)
data Month = January | 
             February | 
             March | 
             April |
             May |
             June |
             July |
             August |
             September |
             October |
             November |
             December deriving (Show, Eq, Ord)


season_temp :: Season -> Temp

season_temp s
  | elem s [Spring, Winter] = Cold
  | elem s [Summer, Autumn] = Hot

month_season m
  | elem m [December, January, February] = Winter
  | elem m [March, April, May] = Spring
  | elem m [June, July, August] = Summer
  | otherwise = Autumn