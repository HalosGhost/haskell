import qualified Data.Char as Char
import Data.List
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

disp :: Int -> String
disp n = happiness n ++ " " ++ show (happySeq n)

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
           truthArgs  = return . intercalate " " . map (happiness . read)
           truthStdin = getContents >>= (return . lines) >>= truthArgs

ver   = putStrLn "happy 1.0.0"
usage = putStrLn $ intercalate "\n" help
      where help = [ "Usage: happy [options] [[NUM ..]|-]\n"
                   , "Options:"
                   , "  -h, --help      Show this help and exit"
                   , "  -v, --version   Show the version and exit"
                   , "  -s, --seq       Include the sequence for each input"
                   ]

exitSucc = exitWith ExitSuccess
exitFail = exitWith $ ExitFailure 1
