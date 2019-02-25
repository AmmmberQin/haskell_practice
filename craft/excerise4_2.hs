-- @Author: ariesduanmu
-- @Date:   2019-02-26 01:19:44
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-02-26 01:47:11

-- fourPics :: Picture -> Picture

-- fourPics pic = top `above` bottom
--   where top = pic `beside` flipH (invertColour pic)
--         bottom = invertColour top

triArea :: Float -> Float -> Float -> Float

triArea a b c
  | possible = sqrt(s*(s-a)*(s-b)*(s-c))
  | otherwise = 0
  where
    s = (a+b+c)/2
    possible = (a+b)>c && (a+c)>b && (b+c)>a

maxThreeOccures :: Int -> Int -> Int -> (Int, Int)

maxThreeOccures a b c
  | k == a && k == b && k == c = (k, 3)
  | (k == a && k == b) || (k == a && k == c) || (k == b && k == c) = (k, 2)
  | otherwise = (k, 1)
  where k = max (max a b) c