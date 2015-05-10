import Data.Char

nextHappy :: Int -> Int
nextHappy = sum . map (^2) . map Data.Char.digitToInt . show

happySeq :: Int -> [Int]
happySeq n = next : (takeWhile (/= next) . happySeq $ next)
         where next = nextHappy n

happy :: Int -> Bool
happy n = last (happySeq n) == 1

{--
 - TODO:
 --
 - main should loop on reading lines from the user to test happiness
 --}
main = putStrLn . show $ happy 31413
