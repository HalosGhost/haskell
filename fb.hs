fb :: Integer -> String
fb n | n `rem` 15 == 0 = "FizzBuzz"
     | n `rem` 5  == 0 = "Buzz"
     | n `rem` 3  == 0 = "Fizz"
     | otherwise       = show n

fibs = [ fb x | x <- [1..]]

main = putStrLn . concat $ map (++ " ") $ take 100 fibs
