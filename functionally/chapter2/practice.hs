-- Practice C
import Data.Char

modernise :: String -> String
modernise title = unwords (map (\t -> toUpper (head t): map toLower (tail t)) (words title))