-- data Maybe a = Nothing | Just a deriving (Eq, Ord, Read, Show)
errDiv :: Integer -> Integer -> Maybe Integer
errDiv n m
  |(m /= 0) = Just (div n m)
  |otherwise = Nothing

mayMaybe :: (a -> b) -> Maybe a -> Maybe b
mayMaybe g Nothing = Nothing
mayMaybe g (Just x) = Just (g x)

mayBe :: b -> (a -> b) -> Maybe a -> b
mayBe n f Nothing = n
mayBe n f (Just x) = f x

iIndex :: [Int] -> Int -> Int -> Maybe [Int]
iIndex xs n m
  |length xs>n && n >=0 && length xs>m && m >=0 = Just [xs!!n, xs!!m]
  |otherwise = Nothing

process :: [Int] -> Int -> Int -> Int
process xs n m = mayBe 0 sum (iIndex xs n m)