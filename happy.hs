import Data.Char

happy :: Int -> Int
happy = sum . map (^2) . map Data.Char.digitToInt . show

{--
 - TODO:
 --
 - unhappy sequences should terminate on the first repetition
 --}

happies :: Int -> [Int]
happies 1 = []
happies n = happy n : happies (happy n)

isHappy :: Int -> Bool
isHappy n = last (happies n) == 1

main = putStrLn . show $ happies 31413
