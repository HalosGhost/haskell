module Main where

import Data.List (intercalate)

(=%=) :: Integral a => a -> a -> Bool
n =%= d = n `rem` d == 0

fb :: (Integral a, Show a) => a -> String
fb n | n =%= 15  = "FizzBuzz"
     | n =%= 5   = "Buzz"
     | n =%= 3   = "Fizz"
     | otherwise = show n

fzbz :: (Integral a, Show a) => a -> [String]
fzbz n = fb <$> [1..n]

main = putStrLn . intercalate " " $ fzbz 100
