-- Practice C
import Data.Char

modernise :: String -> String
modernise title = unwords (map (\t -> toUpper (head t): map toLower (tail t)) (words title))

-- Practice D

-- head (map f xs) Beaver count n times, Susan count 1 time

first :: (a -> Bool) -> [a] -> a
first p xs 
    |null xs   = error "Empty list"
    |p x       = x
    |otherwise = first p (tail xs)
    where x = head xs 

-- Practice E

first1 p xs
    |null ys = Nothing
    |otherwise = Just (head ys)
    where ys   = filter p xs
-- Practice F

-- exp x n
--     |n==0 = 1
--     |n==1 = x
--     |otherwise =x*exp x (n-1)

-- exp x n need multiplicate n times (if n == 0 or n == 1, 0 time)

exp1 x n
    |n==0 = 1
    |n==1 = x
    |even n = (exp1 x (n`div`2))^2
    |odd n = ((exp1 x ((n-1)`div`2))^2) * x

-- multiplicate lgn times

-- Practice G
months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
postfix day
    |day==1 || day==21 || day==31 = "st"
    |day==2 || day==22 = "nd"
    |day==3 || day==23 = "rd"
    |otherwise = "th"

type Date = (Int, Int, Int)
showDate :: Date -> String
showDate date = (show day)++(postfix day)++" "++(months!!(month-1))++", "++(show year) 
    where (day, month, year) = date

-- Practice H
type CIN = String
addSum :: CIN -> CIN
addSum cin = cin ++ (show (sum (map (\t -> (read [t] :: Int)) cin)))

valid :: CIN -> Bool
valid n = (sum (map (\t -> (read [t] :: Int)) (take 8 n))) == (read (drop 8 n) :: Int)
-- Practice I
