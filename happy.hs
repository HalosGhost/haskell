import Data.Char
import System.Environment
import System.Exit

nextHappy :: Int -> Int
nextHappy = sum . map (^2) . map Data.Char.digitToInt . show

happySeq :: Int -> [Int]
happySeq n = next : (takeWhile (/= next) . happySeq $ next)
         where next = nextHappy n

happy :: Int -> Bool
happy n = last (happySeq n) == 1

unhappy :: Int -> Bool
unhappy n = not $ happy n

{--
 - TODO:
 --
 - main should either loop on user input from stdin or parse cli args
 - cf. https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling#Getting_in_arguments
 --}
main = getArgs >>= parse >>= putStr

parse ["-h"] = usage >> exitSucc
parse ["-v"] = ver   >> exitSucc
parse []     = usage >> exitFail

usage    = putStrLn "Usage: happy [-vh] [FILE ..]"
ver      = putStrLn "happy 0.0.1"
exitSucc = exitWith ExitSuccess
exitFail = exitWith $ ExitFailure 1
