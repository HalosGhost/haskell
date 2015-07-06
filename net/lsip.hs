module Main where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as L (putStr)
import Data.List (intercalate)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getArgs)

listIP :: IO ()
listIP = simpleHttp "https://icanhazip.com" >>= L.putStr

data LsipConfig = Config { help :: Bool
                         , ver  :: Bool
                         , ipv4 :: Bool
                         , ipv6 :: Bool
                         , both :: Bool
                         }

parseArgs :: [String] -> LsipConfig
parseArgs a = Config { help = isPresent ("-h", "--help") a
                     , ver  = isPresent ("-v", "--ver" ) a
                     , ipv4 = isPresent ("-4", "--ipv4") a
                     , ipv6 = isPresent ("-6", "--ipv6") a
                     , both = isPresent ("-a", "--all" ) a
                     } where  isPresent (f, s) l = elem f l || elem s l

dispatch :: LsipConfig -> IO ()
dispatch c | help c    = usage   >> exitSucc >>= putStrLn
           | ver  c    = version >> exitSucc >>= putStrLn
           | ipv4 c    = listIP
           | ipv6 c    = listIP
           | both c    = listIP  >> listIP
           | otherwise = listIP

exitSucc, exitFail :: IO a
exitSucc = exitWith   ExitSuccess
exitFail = exitWith $ ExitFailure 1

version = putStrLn "lsip 0.8.0"
usage   = putStrLn $ intercalate "\n" help
        where help = [ "Usage: lsip [options]\n"
                     , "Options:"
                     , "  -h, --help      Show this help and exit"
                     , "  -v, --ver       Show the version and exit"
                     , "  -4, --ipv4      Check ipv4 external address"
                     , "  -6, --ipv6      Check ipv6 external address"
                     , "  -a, --all       Equivalent to \"-4 -6\""
                     ]

main :: IO ()
main = getArgs >>= dispatch . parseArgs
