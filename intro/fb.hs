import Data.List (intercalate)

fb :: Integer -> String
fb n | n `rem` 15 == 0 = "FizzBuzz"
     | n `rem` 5  == 0 = "Buzz"
     | n `rem` 3  == 0 = "Fizz"
     | otherwise       = show n

main = putStrLn . intercalate " " $ take 100 fzbz
     where fzbz = [fb x | x <- [1..]]
