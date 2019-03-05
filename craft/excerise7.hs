-- @Author: ariesduanmu
-- @Date:   2019-03-06 01:36:11
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-03-06 01:55:23
import Data.Char

digits :: String -> String
digits st = [ch | ch <- st, isDigit ch]

firstDigit :: String -> Char
firstDigit st
  = case (digits st) of
    []      -> '\0'
    (x:_)   -> x

firstDigitNumber :: String -> Integer
firstDigitNumber st
  = case (digits st) of 
    [] -> 0
    (x:_) -> (read [x]) + 1

weridMatch :: String -> String
weridMatch st
  = case (digits st) of 
    (x:y:_) -> show ((read [x]) + (read [y]))
    (x:_)   -> [(head st)]
    []      -> "0"

product1 :: [Integer] -> Integer

product1 ns
  = case ns of 
    []     -> 1
    (x:xs) -> product1 xs * x

add1 :: [Bool] -> Bool

add1 bs
  = case bs of 
    []     -> True
    (x:xs) -> add1 xs && x

or1 :: [Bool] -> Bool
or1 bs
  = case bs of 
    []     -> True
    (x:xs) -> or1 xs || x
