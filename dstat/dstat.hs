module Main where

import Data.List (intercalate, elemIndex)
import Data.Maybe (fromJust)
import System.Process (readProcess)
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Command (runCommand, waitForProcess, exitCode)
import Graphics.X11.Xlib.Event (sync)
import Graphics.X11.Xlib.Display (defaultRootWindow, openDisplay, closeDisplay)
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Xlib.Window (storeName)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getArgs)

batLoc = "/sys/class/power_supply/"
enLoc  = "/sys/class/net/"
wlLoc  = "/proc/net/wireless"

batLevel :: String -> IO Int
batLevel b = do cap <- readFile $ batLoc ++ b ++ "/capacity"
                return . read $ take 2 cap

batStatus :: String -> IO Char
batStatus b = do stat <- readFile $ batLoc ++ b ++ "/status"
                 return $ if head stat == 'C' then 'ϟ' else 'D'

batComp :: String -> IO String
batComp b = do l <- batLevel b
               s <- batStatus b
               return $ "B: " ++ show l ++ [s]

vol :: IO Int
vol = do v <- readProcess "ponymix" ["get-volume"] []
         return $ read $ init v

muted :: IO Char
muted = do p <- runCommand "ponymix is-muted"
           e <- waitForProcess p
           return $ if e /= (exitCode 0) then '%' else 'X'

volComp :: IO String
volComp = do v <- vol
             m <- muted
             return $ "A: " ++ show v ++ [m]

enStatus :: String -> IO String
enStatus e = do stat <- readFile $ enLoc ++ e ++ "/operstate"
                return $ "E: " ++ (if head stat == 'u' then "U" else "D")

wlStr :: String -> IO Int
wlStr s = do stat <- readFile $ wlLoc
             let status_line = words . last . lines $ stat
             let cnctd       = (init . head $ status_line) == s
             return $ if cnctd then (read $ take 2 $ status_line!!2) else -1

wlBars :: Int -> String
wlBars s | s > 60 = "▂▃▄▅▆▇█" | s > 50    = "▂▃▄▅▆▇"
         | s > 40 = "▂▃▄▅▆"   | s > 30    = "▂▃▄▅"
         | s > 20 = "▂▃▄"     | s > 10    = "▂▃"
         | s >= 0 = "▂"       | otherwise = "No Signal"

curTime :: String -> IO String
curTime f = getZonedTime >>= return . formatTime defaultTimeLocale f

(<|>) :: String -> String -> String
f <|> s = f ++ " | " ++ s

type StatOpts = (String, String, String, String)

stats :: StatOpts -> IO String
stats (bt, en, wl, cl) = do b <- batComp bt
                            v <- volComp
                            e <- enStatus en
                            w <- wlStr wl
                            let wc = "W: " ++ wlBars w
                            t <- curTime cl
                            return $ e <|> wc <|> v <|> b <|> t

status :: Maybe Display -> StatOpts -> IO ()
status disp sts = let updateDwm d s = do let w = defaultRootWindow d
                                         storeName d w s
                                         sync d False
                      in forever $ do s <- stats sts
                                      case disp of
                                           Nothing  -> putStrLn s
                                           (Just d) -> updateDwm d s
                                      threadDelay 6000000

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
parse a = Config { help     = isPresent ("-h", "--help")    a
                 , version  = isPresent ("-v", "--version") a
                 , stdout   = isPresent ("-s", "--stdout")  a
                 , batDev   = batteryDevice
                 , wiredDev = wiredDevice
                 , wifiDev  = wirelessDevice
                 , timeFmt  = timeFormat
                 } where
                 isPresent (f,s) l = (elem f l || elem s l)
                 nextArg s l = (drop (1 + (fromJust $ elemIndex s l)) l) !! 0
                 optArg (s,l,d) ls | elem s ls = nextArg s ls
                                   | elem l ls = nextArg l ls
                                   | otherwise = d
                 batteryDevice  = optArg ("-b", "--bat", "BAT0") a
                 wiredDevice    = optArg ("-e", "--en",  "en0")  a
                 wirelessDevice = optArg ("-w", "--wl",  "wl0")  a
                 timeFormat     = optArg ("-c", "--clk", "%H.%M (%Z) | %A, %d %B %Y") a

dispatch :: DstatConfig -> IO ()
dispatch c | help c    = usage >> exitSucc >>= putStrLn
           | version c = ver   >> exitSucc >>= putStrLn
           | stdout c  = status Nothing cfg
           | otherwise = do d <- openDisplay ""
                            status (Just d) cfg
                            closeDisplay d
           where cfg = ( batDev   c
                       , wiredDev c
                       , wifiDev  c
                       , timeFmt  c
                       )
