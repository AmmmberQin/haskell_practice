-- @Author: Li Qin
-- @Date:   2019-03-15 15:51:11
-- @Last Modified by:   Li Qin
-- @Last Modified time: 2019-03-15 16:24:59
import Prelude hiding (zipWith, length)


zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith f _ _ = []

length :: [a] -> Int
length xs = sum (map (\x -> 1) xs)

greaterOne n = n>1
addOne n = n+1

addUp :: [Integer] -> [Integer]
addUp ns = filter greaterOne (map addOne ns)


greaterZero n = n>0

addUp1 :: [Integer] -> [Integer]
addUp1 ns = map addOne (filter greaterZero ns)

squareList :: [Integer] -> [Integer]
squareList xs = map (\x -> x*x) xs

squareSum :: [Integer] -> Integer
squareSum xs = sum (squareList xs)

