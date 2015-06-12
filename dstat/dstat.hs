module Main where

import Data.List (intercalate, elemIndex)
import Data.Maybe (fromJust)
import System.Process (readProcess)
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import Control.Concurrent (threadDelay)
import System.Command (runCommand, waitForProcess, exitCode)
import Graphics.X11.Xlib.Event (sync)
import Graphics.X11.Xlib.Display (defaultRootWindow, openDisplay, closeDisplay)
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Xlib.Window (storeName)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getArgs)

bat_loc = "/sys/class/power_supply/"
en_loc  = "/sys/class/net/"
wl_loc  = "/proc/net/wireless"

bat_level :: String -> IO Int
bat_level b = do cap <- readFile $ bat_loc ++ b ++ "/capacity"
                 return . read $ take 2 cap

bat_status :: String -> IO Char
bat_status b = do stat <- readFile $ bat_loc ++ b ++ "/status"
                  return $ if head stat == 'C' then 'ϟ' else 'D'

bat_comp :: String -> IO String
bat_comp b = do l <- bat_level b
                s <- bat_status b
                return $ "B: " ++ show l ++ [s]

vol :: IO Int
vol = do v <- readProcess "ponymix" ["get-volume"] []
         return $ read $ init v

muted :: IO Char
muted = do p <- runCommand "ponymix is-muted"
           e <- waitForProcess p
           return $ if e /= (exitCode 0) then '%' else 'X'

vol_comp :: IO String
vol_comp = do v <- vol
              m <- muted
              return $ "A: " ++ show v ++ [m]

en_stat :: String -> IO String
en_stat e = do stat <- readFile $ en_loc ++ e ++ "/operstate"
               return $ "E: " ++ (if head stat == 'u' then "U" else "D")

wl_str :: String -> IO Int
wl_str s = do stat <- readFile $ wl_loc
              let status_line = words . last . lines $ stat
              let cnctd       = (init . head $ status_line) == s
              return $ if cnctd then (read $ take 2 $ status_line!!2) else -1

wl_bars :: Int -> String
wl_bars s | s < 0     = "No Signal" | s <= 10 = "▂"
          | s <= 20   = "▂▃"        | s <= 30 = "▂▃▄"
          | s <= 40   = "▂▃▄▅"      | s <= 50 = "▂▃▄▅▆"
          | s <= 60   = "▂▃▄▅▆▇"    | s <= 70 = "▂▃▄▅▆▇█"
          | otherwise = "No Signal"

cur_time :: String -> IO String
cur_time f = getZonedTime >>= return . formatTime defaultTimeLocale f

(<|>) :: String -> String -> String
f <|> s = f ++ " | " ++ s

type StatOpts = (String, String, String, String)

stats :: StatOpts -> IO String
stats (bt, en, wl, cl) = do b <- bat_comp bt
                            v <- vol_comp
                            e <- en_stat en
                            w <- wl_str wl
                            let wc = "W: " ++ wl_bars w
                            t <- cur_time cl
                            return $ e <|> wc <|> v <|> b <|> t

status :: Maybe Display -> StatOpts -> IO ()
status Nothing  sts = do s <- stats sts
                         putStrLn s
                         threadDelay 6000000
                         status Nothing sts
status (Just d) sts = do let w = defaultRootWindow d
                         s <- stats sts
                         storeName d w s
                         sync d False
                         threadDelay 6000000
                         status (Just d) sts

ver   = putStrLn "dstat 0.8.0"
usage = putStrLn $ intercalate "\n" help
      where help = [ "Usage: dstat [options]\n"
                   , "Options:"
                   , "  -h, --help      Show this help and exit"
                   , "  -v, --ver       Show the version and exit"
                   , "  -s, --stdout    Print output to stdout instead"
                   , "  -e, --en ETH    Use ETH for wired iface (def: en0)"
                   , "  -w, --wl WLN    Use WLN for wireless iface (def: wl0)"
                   , "  -b, --bat BAT   Use BAT for battery (def: BAT0)"
                   , "  -c, --clk FMT   Use FMT to format the clock"
                   ]

exitSucc, exitFail :: IO a
exitSucc = exitWith   ExitSuccess
exitFail = exitWith $ ExitFailure 1

main :: IO ()
main = do ga <- getArgs
          let dc = parse ga
          dispatch dc

data DstatConfig = Config { help     :: Bool
                          , version  :: Bool
                          , stdout   :: Bool
                          , batDev   :: String
                          , wiredDev :: String
                          , wifiDev  :: String
                          , timeFmt  :: String
                          } deriving (Eq, Show)

parse :: [String] -> DstatConfig
parse a = Config { help     = (elem "-h" a || elem "--help"    a)
                 , version  = (elem "-v" a || elem "--version" a)
                 , stdout   = (elem "-s" a || elem "--stdout"  a)
                 , batDev   = batteryDevice
                 , wiredDev = wiredDevice
                 , wifiDev  = wirelessDevice
                 , timeFmt  = timeFormat
                 } where
                 nextArg s l = (drop (1 + (fromJust $ elemIndex s l)) l) !! 0
                 optArg (s,l,d) ls | elem s ls = nextArg s ls
                                   | elem l ls = nextArg l ls
                                   | otherwise = d
                 batteryDevice  = optArg ("-b", "--bat", "BAT0") a
                 wiredDevice    = optArg ("-e", "--en",  "en0")  a
                 wirelessDevice = optArg ("-w", "--wl", "wl0")   a
                 timeFormat     = optArg ("-c", "--clk", "%H.%M (%Z) | %A, %d %B %Y") a

dispatch :: DstatConfig -> IO ()
dispatch c | help c    = usage >> exitSucc >>= putStrLn
           | version c = ver >> exitSucc >>= putStrLn
           | stdout c  = status Nothing cfg
           | otherwise = do d <- openDisplay ""
                            status (Just d) cfg
                            closeDisplay d
           where cfg = ( batDev   c
                       , wiredDev c
                       , wifiDev  c
                       , timeFmt  c
                       )
