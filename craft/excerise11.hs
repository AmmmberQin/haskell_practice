-- @Author: ariesduanmu
-- @Date:   2019-03-17 18:40:12
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-03-17 19:36:47
isChar :: Char -> Bool

isChar c = not$elem c " \t\n" 

f :: Integer -> Integer -> Integer
f x y = x^y

g a b = f b a

filp :: (Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer)
filp f = g

multiply :: Int -> Int -> Int
multiply x y = x*y

doubleAll :: [Int] -> [Int]
doubleAll = map (multiply 2)