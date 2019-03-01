-- @Author: ariesduanmu
-- @Date:   2019-02-28 01:13:32
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-02-28 01:51:10
blackWhite :: Integer -> Picture
blackWhite n 
  | n<=1 = black
  | otherwise = black `beside` whiteBlack (n-1)

whiteBlack :: Integer -> Picture
whiteBlack n 
  | n<=1 = white
  | otherwise = white `beside` blackWhite (n-1)

blackChess :: Integer -> Integer -> Picture

blackChess n m
  | n<=1 = blackWhite m
  | otherwise = blackWhite m `above` whiteChess (n-1) m

whiteChess :: Integer -> Integer -> Picture

whiteChess n m
  | n<=1 = whiteBlack m
  | otherwise = whiteBlack m `above` blackChess (n-1) m

column :: Picture -> Integer -> Picture
column p n
  | n <= 1 = p
  | otherwise = p `above` column p (n-1)

row :: Picture -> Integer -> Picture
row p n
  | n<=1 = p
  | otherwise = p `beside` row p (n-1)

leftSideRight :: Integer -> Picture
leftSideRight n
  | n >= 2 = column white (n-1)
  | otherwise = error "leftSideRight only for n >= 2"

leftSideBottom :: Integer -> Picture
leftSideBottom n
  | n <= 1 = black
  | otherwise = (row white (n-1)) `beside` black


leftAcross :: Integer -> Picture
leftAcross n
  | n<=1 = leftSideBottom n
  | otherwise = ((leftAcross (n-1)) `beside` (leftSideRight n)) `above` (leftSideBottom n)


rightSideRight :: Integer -> Picture
rightSideRight n
  | n >= 2 = column white (n-1)
  | otherwise = error "leftSideRight only for n >= 2"

rightSideBottom :: Integer -> Picture
rightSideBottom n
  | n <= 1 = black
  | otherwise = black `beside` (row white (n-1))


rightAcross :: Integer -> Picture
rightAcross n
  | n<=1 = rightSideBottom n
  | otherwise = ((rightAcross (n-1)) `beside` (rightSideRight n)) `above` (rightSideBottom n)

doubleAcross :: Integer -> Picture
doubleAcross n
  | n == 1 = black
  | n == 2 = (column black 2) `beside` (column black 2)
  | otherwise = r `above` (c `beside` (doubleAcross (n-2)) `beside` c) `above` r
  where 
    r = black `beside` (row white (n-2)) `beside` black
    c = column white (n-2)

whiteBlackChessColumn :: Integer -> Picture
whiteBlackChessColumn n
  | n == 1 = white
  | otherwise = white `above` blackWhiteChessColumn (n-1)

blackWhiteChessColumn :: Integer -> Picture
blackWhiteChessColumn n
  | n == 1 = black
  | otherwise = black `above` whiteBlackChessColumn (n-1)

chessBoard :: Integer -> Picture
chessBoard n
  | n == 1 = black
  | otherwise = blackWhite n `above` (whiteBlackChessColumn (n-1) `beside` chessBoard (n-1))