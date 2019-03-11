-- @Author: ariesduanmu
-- @Date:   2019-03-11 22:18:42
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-03-12 01:21:18
import System.Random
import Data.Array.IO
import Control.Monad
import Prelude hiding (putStrLn)
import Data.Char

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
       if str == reverse str
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