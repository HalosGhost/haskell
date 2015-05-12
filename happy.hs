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

main :: IO ()
main = getArgs >>= parse >>= putStrLn

parse :: [String] -> IO String
parse a | elem "-h" a || elem "--help"    a = usage >> exitSucc
        | elem "-v" a || elem "--version" a = ver   >> exitSucc
        | a == []     || elem "-"         a = stdin
        | otherwise                         = args a
        where
           args  = return . List.intercalate " " . map (show . happy . read)
           stdin = getContents >>= (return . lines) >>= args

usage    = putStrLn "Usage: happy [-vh] [[NUM ..]|-]"
ver      = putStrLn "happy 0.0.1"
exitSucc = exitWith ExitSuccess
exitFail = exitWith $ ExitFailure 1
