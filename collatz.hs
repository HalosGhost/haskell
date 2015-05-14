import qualified Data.Char as Char
import qualified Data.List as List
import System.Environment
import System.Exit

nextCollatz :: Int -> Int
nextCollatz n | even n    = n `div` 2
              | otherwise = 3 * n + 1

collatzSeq :: Int -> [Int]
collatzSeq 1 = []
collatzSeq n = next : (collatzSeq next)
             where next = nextCollatz n

collatz :: Int -> Bool
collatz n = last (collatzSeq n) == 1

collatzConject :: Int -> String
collatzConject n = show $ collatz n

main :: IO ()
main = getArgs >>= parse >>= putStrLn

parse :: [String] -> IO String
parse a | elem "-h" a || elem "--help"    a = usage >> exitSucc
        | elem "-v" a || elem "--version" a = ver   >> exitSucc
        | a == []     || elem "-"         a = stdin
        | otherwise                         = args a
        where
           args  = return . List.intercalate " " . map (collatzConject . read)
           stdin = getContents >>= (return . lines) >>= args

usage    = putStrLn "Usage: collatz [[[-h|--help]|[-v|--version]]|[[NUM ..]|-]]"
ver      = putStrLn "collatz 0.0.2"
exitSucc = exitWith ExitSuccess
exitFail = exitWith $ ExitFailure 1
