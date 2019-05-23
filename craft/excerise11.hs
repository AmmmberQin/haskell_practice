-- @Author: ariesduanmu
-- @Date:   2019-03-17 18:40:12
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-03-18 23:11:17
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

double :: Integer -> Integer
double n = n * 2

trible :: Integer -> Integer
trible n = n * 3

iter :: Integer -> (a -> a) -> a -> a
iter n f
  | n >=0 = f . iter (n-1) f
  | otherwise = id

iter1 :: Integer -> (a -> a) -> a -> a
iter1 n f
  | n >=0 = foldr1 (.) [f, iter1 (n-1) f]
  | otherwise = id

mapFuns :: [a->b] -> a -> [b]
mapFuns fs x = map (\f->f x) fs

mapFuns1 :: [a->b] -> a -> [b]
mapFuns1 fs = funs1
  where funs1 x = map (\f -> f x) fs
