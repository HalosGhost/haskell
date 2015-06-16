import qualified Data.Char as Char
import Data.List
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

disp :: Int -> String
disp n = collatzConject n ++ " " ++ show (collatzSeq n)

main :: IO ()
main = getArgs >>= parse >>= putStrLn

parse :: [String] -> IO String
parse a | elem "-h" a    || elem "--help"    a = usage >> exitSucc
        | elem "-v" a    || elem "--version" a = ver   >> exitSucc
        | head a == "-s" || head a == "--seq"  = seq' $ tail a
        | elem "-" a                           = truthStdin
        | otherwise                            = truthArgs a
        where
           seq' l | head l == "-" = listStdin
                  | otherwise     = listArgs l
           listArgs   = return . intercalate " " . map (disp . read)
           listStdin  = getContents >>= (return . lines) >>= listArgs
           truthArgs  = return . intercalate " " . map (collatzConject . read)
           truthStdin = getContents >>= (return . lines) >>= truthArgs

ver   = putStrLn "happy 1.0.0"
usage = putStrLn $ intercalate "\n" help
      where help = [ "Usage: collatz [options] [[NUM ..]|-]\n"
                   , "Options:"
                   , "  -h, --help      Show this help and exit"
                   , "  -v, --version   Show the version and exit"
                   , "  -s, --seq       Include the sequence for each input"
                   ]

exitSucc = exitWith ExitSuccess
exitFail = exitWith $ ExitFailure 1
