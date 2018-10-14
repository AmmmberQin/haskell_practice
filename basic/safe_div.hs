safeDiv :: Int -> Int -> Maybe Int

safeDiv n m = if m == 0 then Nothing else Just (div n m)

data Expr = Val Int | Div Expr Expr deriving (Show)
eval :: Expr -> Maybe Int

eval (Val n) = return n
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safeDiv n m 
