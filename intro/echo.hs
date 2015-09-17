module Main where

import Data.List          (intercalate)
import System.Exit        (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getArgs)

version, usage :: String
version = "echo 1.0.0"
usage = intercalate "\n" [ "Usage: echo [options] [strings]\n"
                         , "Options:"
                         , "  -h, --help      Print this help and exit"
                         , "  -v, --version   Print version and exit"
                         , "  -e, --eval      Enable escape evaluation"
                         , "  -n, --nolf      Do not add a trailing newline"
                         ]

main :: IO ()
main = getArgs >>= dispatch . parseArgs

data EchoConf = Config { eHelp :: Bool
                       , eVer  :: Bool
                       , eEval :: Bool
                       , eNoLF :: Bool
                       , eStr  :: String
                       }

parseArgs :: [String] -> EchoConf
parseArgs a = Config { eHelp = isPresent ("--help","-h")
                     , eVer  = isPresent ("--version", "-v")
                     , eEval = isPresent ("--eval", "-e")
                     , eNoLF = isPresent ("--nolf", "-n")
                     , eStr  = intercalate " " $ dropWhile ((== '-') . head) a
                     } where isPresent (f,s) = (elem f a || elem s a)

dispatch :: EchoConf -> IO ()
dispatch c | length s == 0 = putStrLn usage >> exitFail
           | eHelp c   = putStrLn usage   >> exitSucc
           | eVer  c   = putStrLn version >> exitSucc
           | eNoLF c   = putStr   str     >> exitSucc
           | otherwise = putStrLn str     >> exitSucc
           where str | eEval c   = s
                     | otherwise = escape s
                 s   = eStr c

escape :: String -> String
escape s = take l s
         where l = (length $ show s) - 2

exitSucc, exitFail :: IO a
exitSucc = exitWith   ExitSuccess
exitFail = exitWith $ ExitFailure 1

