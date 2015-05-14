import qualified Data.Char as Char
import qualified Data.List as List
import System.Environment
import System.Exit

nextHappy :: Int -> Int
nextHappy = sum . map (^2) . map Char.digitToInt . show

happySeq :: Int -> [Int]
happySeq n = next : (takeWhile (/= next) . happySeq $ next)
           where next = nextHappy n

happy :: Int -> Bool
happy n = last (happySeq n) == 1

happiness :: Int -> String
happiness n | happy n   = "Happy"
            | otherwise = "Unhappy"

main :: IO ()
main = getArgs >>= parse >>= putStrLn

parse :: [String] -> IO String
parse a | elem "-h" a || elem "--help"    a = usage >> exitSucc
        | elem "-v" a || elem "--version" a = ver   >> exitSucc
        | a == []     || elem "-"         a = stdin
        | otherwise                         = args a
        where
           args  = return . List.intercalate " " . map (happiness . read)
           stdin = getContents >>= (return . lines) >>= args

usage    = putStrLn "Usage: happy [[[-h|--help]|[-v|--version]]|[[NUM ..]|-]]"
ver      = putStrLn "happy 0.0.2"
exitSucc = exitWith ExitSuccess
exitFail = exitWith $ ExitFailure 1
