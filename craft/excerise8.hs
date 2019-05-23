-- @Author: ariesduanmu
-- @Date:   2019-03-11 22:18:42
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-03-17 18:15:42
module Excerise8 where

import System.Random
import Data.Array.IO
import Control.Monad
import Prelude hiding (putStrLn, cycle)
import Data.Char
import Data.List hiding (cycle)
import Test.QuickCheck

data Move = Rock | Paper | Scissors deriving (Show, Eq)
type Tournament = ([Move], [Move])

beat :: Move -> Move
beat Rock = Scissors
beat Paper = Rock
beat Scissors = Paper

lose :: Move -> Move
lose Scissors = Rock
lose Rock = Paper
lose Paper = Scissors

outcome :: Move -> Move -> Integer
outcome m1 m2
  | m1 == m2 = 0
  | beat m1 == m2 = 1
  | otherwise = -1

tournamentOutcome :: Tournament -> Integer
tournamentOutcome tour = sum [outcome (tour1!!i) (tour2!!i) | i <- [0..(length tour1)-1] ]
  where (tour1, tour2) = tour

type Strategy = [Move] -> Move
rock, paper, scissors :: Strategy

rock _ = Rock
paper _ = Paper
scissors _ = Scissors

cycle :: Strategy

cycle moves
  = case (length moves) `rem` 3 of
    0 -> Rock
    1 -> Paper
    2 -> Scissors

echo :: Strategy

echo [] = Rock
echo (latest:rest) = latest

winLast :: Strategy
winLast [] = Rock
winLast (latest:rest) = beat latest

loseLast :: Strategy
loseLast [] = Rock
loseLast (latest:rest) = lose latest

validMoves = [Paper, Scissors, Rock]

restMove :: Move -> [Move]
restMove Rock = [Paper, Scissors]
restMove Paper = [Rock, Scissors]
restMove Scissors = [Rock, Paper]

bestChoice :: Move -> Move -> Move
bestChoice m1 m2
  | outcome m1 m2 >= 0 = m1
  | otherwise = m2


-- TODO IO Int -> Int
-- r = randomRIO (0,(length validMoves)-1)
-- randomMove = validMoves!!r

-- randomStrategy :: Strategy
-- randomStrategy (latest:premove:rest)
--   | latest == premove = bestChoice r1 r2
--   | otherwise = randomMove
  -- where [r1, r2] = restMove latest

frequence :: [Move] -> [Int]

frequence moves = [paper, rock, scissors]
  where paper = length [m | m<-moves, m==Paper]
        rock = length [m | m<-moves, m==Rock]
        scissors = length [m | m<-moves, m==Scissors]

fewestStrategy :: Strategy
fewestStrategy moves 
  | f!!0 <= f!!1 && f!!0 <= f!!2 = lose Paper
  | f!!1 <= f!!0 && f!!1 <= f!!2 = lose Rock
  | otherwise = lose Scissors
  where f = frequence moves


-- alternate :: Strategy -> Strategy -> Strategy
-- alternate st1 st2

putStrLn :: String -> IO()
putStrLn = putStr . (++ "\n")

helloWorld :: IO()
helloWorld = putStrLn "hello World!"

read2lines :: IO()
read2lines
  = do first <- getLine
       second <- getLine
       putStrLn ("Two Lines read " ++ (reverse first) ++ " " ++ second)

getInt :: IO Integer
getInt = do line <- getLine
            return ((read line :: Integer) + 10)

isPalindrome :: IO()
isPalindrome
  = do str <- getLine
       if [toLower s | s<-str, isAlpha s] == reverse [toLower s | s<-str, isAlpha s]
          then putStrLn "Good job"
          else putStrLn "NO"

addNum :: IO Integer
addNum
  = do putStrLn "Input two numbers"
       i <- getLine
       j <- getLine
       putStr ("Sum: "++i++"+"++j++"=")
       return ((read i :: Integer) + (read j :: Integer))

putNtimes :: Integer -> String -> IO()
putNtimes 0 _ = do putStr ""
putNtimes n str = do putStrLn str
                     putNtimes (n-1) str

addN :: Integer -> Integer -> IO Integer

addN 0 s = do return s
addN n s = do i <- getLine
              addN (n-1) ((read i :: Integer) + s)
sumN :: IO Integer
sumN = do putStr "How many numbers you wanna input: "
          n <- getLine
          addN (read n :: Integer) 0

copy :: IO()
copy = 
  do line <- getLine
     putStrLn line
     copy

copyN :: Integer -> IO()
copyN n =
  if n<=0
    then return ()
    else do line <- getLine
            putStrLn line
            copyN (n-1)

copyEmpty :: IO()
copyEmpty =
  do line <- getLine
     if line == ""
        then return ()
        else do putStrLn line
                copyEmpty

copyCount :: Integer -> IO()
copyCount n =
  do line <- getLine
     if line == ""
        then putStrLn (show n ++ " lines copied.")
        else do putStrLn line
                copyCount (n+1)

copyInfo :: Int -> Int -> Int -> IO()
copyInfo lines words chars = 
  do line <- getLine
     if line == ""
        then putStrLn (show lines ++ " " ++ show words ++ " " ++ show chars)
        else do copyInfo (lines+1) (words+(length [l | l <- line, l==' '] + 1)) (chars+(length line))


wc :: IO()
wc = copyInfo 0 0 0

numberList :: Integer -> IO()
numberList n = 
  do line <- getLine
     if line == "" || not (all (isNumber) line)
        then numberList n
        else do
             if line=="0"
                then putStrLn ("Sum is: " ++ show n)
                else numberList (n+(read line :: Integer))

numberSortList :: [Integer] -> IO()
numberSortList list =
  do line <- getLine
     if line == "" || not (all (isNumber) line)
        then numberSortList list
        else do
             if line=="0"
                then putStrLn ("Sorted List is: " ++ (show list))
                else numberSortList (mergeSort list (read line :: Integer))

slice :: Int -> Int -> [Integer] -> [Integer]
slice from to xs = take (to-from+1) (drop from xs)

mergeSort :: [Integer] -> Integer -> [Integer]
mergeSort [] n = [n]
mergeSort (x:[]) n = [a,b]
  where a = min x n
        b = max x n
mergeSort list n
  | n <= mid = (mergeSort (take m list) n) ++ (drop m list)
  | otherwise = (take m list) ++ (mergeSort (drop m list) n)
  where m = div (length list) 2
        mid = list!!m


prop_mergeSort xs k = mergeSort (sort xs) k == sort (k:xs)

play :: Strategy -> IO()
play strategy = 
  playInteractive strategy ([], [])

playInteractive :: Strategy -> Tournament -> IO()
playInteractive s t@(mine, yours) = 
  do
    ch <- getChar
    if not (ch `elem` "rpsRPS")
      then showResults t
      else do let next = s yours
              putStrLn ("\nI play: " ++ show next ++
                        " you play: " ++ [ch])
              let yourMove = convertMove ch
              playInteractive s (next:mine, yourMove:yours)

convertMove :: Char -> Move
convertMove c = 
  case toLower c of
    'r' -> Rock
    'p' -> Paper
    's' -> Scissors

showResults :: Tournament -> IO()
showResults t 
  | r > 0 = putStrLn "I won!"
  | r == 0 = putStrLn "Draw"
  | otherwise = putStrLn "You won: well done!"
  where r = tournamentOutcome t

playSvsS :: Strategy -> Strategy -> Integer -> Tournament
playSvsS strategyA strategyB 0 = ([],[])
playSvsS strategyA strategyB n = step strategyA strategyB (playSvsS strategyA strategyB (n-1))


step :: Strategy -> Strategy -> Tournament -> Tournament
step strategyA strategyB (movesA, movesB) = 
  (strategyA movesB : movesA, strategyB movesA : movesB)