-- @Author: ariesduanmu
-- @Date:   2019-03-17 11:13:18
-- @Last Modified by:   Li Qin
-- @Last Modified time: 2019-05-23 11:23:51
import Prelude hiding (length, takeWhile)
import Data.List hiding (length, takeWhile)
import Test.QuickCheck
import Excerise8 hiding (putStrLn, tournamentOutcome)

length :: [a] -> Int
length xs = sum (map (\x -> 1) xs)

addOne n = n+1

readN :: Int -> [Integer] -> IO [Integer]
readN 0 xs = do return xs
readN n xs = do x <- getLine
                readN (n-1) (xs ++ [(read x :: Integer)])

minN :: Int -> IO()
minN n  
  | n <= 0 = error "n must greater than zero"
  | otherwise = do xs <- readN n []
                   putStrLn (show ((sort xs)!!0))

isEqualN :: Int -> IO()
isEqualN n
  | n <= 0 = error "n must greater than zero"
  | otherwise = do xs <- readN n []
                   putStrLn (show (all (==xs!!0) xs))
isIncrease :: Int -> IO()
isIncrease n
  | n <= 0 = error "n must greater than zero"
  | otherwise = do xs <- readN n []
                   putStrLn (show (all (\(x,y) -> x < y) (zip (init xs) (tail xs))))

twice :: (Integer -> Integer) -> Integer -> Integer
twice f x = f (f x)

iter :: Integer -> (a -> a) -> a -> a
iter 0 _ x = x
iter n f x = iter (n-1) f (f x)

double :: Integer -> Integer
double x = x*2

idemp :: Integer -> Integer
idemp n = iter n double 1


rev :: [a] -> [a]
rev xs = foldr snoc [] xs

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

square :: Integer -> Integer
square n = foldr (+) 0 (map (^2) [1..n])

squareN :: [Integer] -> Integer
squareN xs = foldr (+) 0 (map (^2) (filter (>0) xs))

l :: a -> [a] -> [a]
l x [] = [x]
l x (y:ys) = l y ys

ilast :: [a] -> a
ilast xs = (foldr l [] xs)!!0

i :: a -> ([a], Bool) -> ([a], Bool)
i x ([],isLast) 
  | isLast = ([], False)
  | otherwise = ([x], isLast)
i x (ys,isLast) = (x:ys, isLast)


iinit :: [a] -> [a]
iinit [] = []
iinit xs = fst (foldr i ([], True) xs)

u :: (a,a) -> ([a], [a]) -> ([a],[a])
u (x,y) ([],[]) = ([x],[y])
u (x,y) (z1, z2) = (x:z1, y:z2)

unZip :: [(a,a)] -> ([a],[a])
unZip xs = foldr u ([],[]) xs


switchMap :: (a->a) -> (a->a) -> [a] -> [a]
switchMap foo1 foo2 [] = []
switchMap foo1 foo2 (x:[]) = [foo1 x]
switchMap foo1 foo2 (x:y:ys) = (foo1 x):(foo2 y):(switchMap foo1 foo2 ys)


splitfun :: a -> ([a],[a]) -> ([a],[a])
splitfun x (ys,zs)
  | (length ys) <= (length zs) = (ys++[x], zs)
  | otherwise = (ys, zs++[x])

split :: [a] -> ([a],[a])
split xs = foldr splitfun ([],[]) (rev xs)


merge :: ([a],[a]) -> [a]
merge ([], ys) = ys
merge (xs, []) = xs
merge ((x:xs), (y:ys)) = [x,y] ++ (merge (xs,ys))

prop_split_merge xs = xs == merge (split xs)

whitespace = [' ', '\n', '\t']

getUntil :: (a -> Bool) -> [a] -> [a]
getUntil p [] = []
getUntil p (x:xs)
  | p x = []
  | otherwise = x:getUntil p xs

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p [] = []
dropUntil p (x:xs)
  | p x = (x:xs)
  | otherwise = dropUntil p xs

dropWord xs = dropUntil (\x -> elem x whitespace) xs
dropSpace xs = dropUntil (\x -> not (elem x whitespace)) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p xs = getUntil (not.p) xs

invertColor xs = map (\x -> mod (x+1) 2) xs

rotate90 :: [[Integer]] -> [[Integer]]
rotate90 xs = foldr rotate [] xs

rotateS :: [Integer] -> [[Integer]]
rotateS [] = []
rotateS (x:xs) = [[x]] ++ (rotateS xs)

rotate :: [Integer] -> [[Integer]] -> [[Integer]]
rotate xs [] = rotateS xs
rotate xs ys = [zs!!i ++ ys!!i | i <- [0..(length zs-1)]]
  where zs = rotateS xs

t :: (Move, Move) -> Integer -> Integer
t (a, b) n = (outcome a b) + n

tournamentOutcome :: Tournament -> Integer
tournamentOutcome tour = foldr t 0 (zip (fst tour) (snd tour))

