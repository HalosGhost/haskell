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
batComp "" = return ""
batComp b  = do l <- batLevel b
                s <- batStatus b
                return $ "B: " ++ show l ++ [s]

vol :: IO Int
vol = do v <- readProcess "ponymix" ["get-volume"] []
         return $ read $ init v

muted :: IO Char
muted = do p <- runCommand "ponymix is-muted"
           e <- waitForProcess p
           return $ if e /= (exitCode 0) then '%' else 'X'

volComp :: String -> IO String
volComp "" = return ""
volComp _  = do v <- vol
                m <- muted
                return $ "A: " ++ show v ++ [m]

enStatus :: String -> IO String
enStatus "" = return ""
enStatus e  = do stat <- readFile $ enLoc ++ e ++ "/operstate"
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

wlComp :: String -> IO String
wlComp "" = return ""
wlComp s  = do w <- wlStr s
               return $ "W: " ++ wlBars w

curTime :: String -> IO String
curTime "" = return ""
curTime f  = getZonedTime >>= return . formatTime defaultTimeLocale f

stats :: DstatConfig -> IO String
stats cfg = let s *|* l = intercalate s $ filter (/= "") l
             in do b <- batComp  $ batDev   cfg
                   v <- volComp  $ volDev   cfg
                   e <- enStatus $ wiredDev cfg
                   w <- wlComp   $ wifiDev  cfg
                   t <- curTime  $ timeFmt  cfg
                   return $ " | " *|* [e, w, v, b, t]

status :: Maybe Display -> DstatConfig -> IO ()
status disp sts = let interval      = 1000000 * (read $ ival sts) :: Int
                      updateDwm d s = do let w = defaultRootWindow d
                                         storeName d w s
                                         sync d False
                         in forever $ do s <- stats sts
                                         case disp of
                                              Nothing  -> putStrLn s
                                              (Just d) -> updateDwm d s
                                         threadDelay interval

ver, usage :: IO ()
ver   = putStrLn "dstat 0.9.0"
usage = putStrLn $ intercalate "\n" help
      where help = [ "Usage: dstat [options]\n"
                   , "Options:"
                   , "  -e, --en ETH    Use ETH for wired iface (def: en0)"
                   , "  -E, --noen      Do not display ethernet module"
                   , "  -w, --wl WLN    Use WLN for wireless iface (def: wl0)"
                   , "  -W, --nowl      Do not display wireless module"
                   , "  -b, --bat BAT   Use BAT for battery (def: BAT0)"
                   , "  -B, --nobat     Do not display battery module"
                   , "  -c, --clk FMT   Use FMT to format the clock (def: “%H.%M (%Z) | %A, %d %B %Y”)"
                   , "  -C, --noclk     Do not display the clock module"
                   , "  -V, --novol     Do not display the volume module\n"
                   , "  -i, --ival TM   Pause for TM seconds between refreshes (def: 6)"
                   , "  -h, --help      Show this help and exit"
                   , "  -v, --version   Show the version and exit"
                   , "  -s, --stdout    Print output to stdout instead"
                   ]

exitSucc, exitFail :: IO a
exitSucc = exitWith   ExitSuccess
exitFail = exitWith $ ExitFailure 1

main :: IO ()
main = getArgs >>= dispatch . parseArgs

data DstatConfig = Config { help     :: Bool
                          , version  :: Bool
                          , stdout   :: Bool
                          , ival     :: String
                          , batDev   :: String
                          , volDev   :: String
                          , wiredDev :: String
                          , wifiDev  :: String
                          , timeFmt  :: String
                          } deriving (Eq, Show)

parseArgs :: [String] -> DstatConfig
parseArgs a = Config
            { help     = isPresent ("-h", "--help"   )
            , version  = isPresent ("-v", "--version")
            , stdout   = isPresent ("-s", "--stdout" )
            , ival     = timeInterval
            , batDev   = noDispIf  ("-B", "--nobat"  ) batteryDevice
            , volDev   = noDispIf  ("-V", "--novol"  ) "placeholder"
            , wiredDev = noDispIf  ("-E", "--noen"   ) wiredDevice
            , wifiDev  = noDispIf  ("-W", "--nowl"   ) wirelessDevice
            , timeFmt  = noDispIf  ("-C", "--noclk"  ) timeFormat
            } where
            noDispIf t def  = if isPresent t then "" else def
            isPresent (f,s) = (elem f a || elem s a)
            nextArg s l = (drop (1 + (fromJust $ elemIndex s l)) l) !! 0
            optArg (s,l,d) ls | elem s ls = nextArg s ls
                              | elem l ls = nextArg l ls
                              | otherwise = d
            batteryDevice  = optArg ("-b", "--bat",  "BAT0") a
            wiredDevice    = optArg ("-e", "--en",   "en0" ) a
            wirelessDevice = optArg ("-w", "--wl",   "wl0" ) a
            timeInterval   = optArg ("-i", "--ival", "6"   ) a
            timeFormat     = optArg ( "-c"
                                    , "--clk"
                                    , "%H.%M (%Z) | %A, %d %B %Y"
                                    ) a

dispatch :: DstatConfig -> IO ()
dispatch c | help c    = usage >> exitSucc >>= putStrLn
           | version c = ver   >> exitSucc >>= putStrLn
           | stdout c  = status Nothing c
           | otherwise = do d <- openDisplay ""
                            status (Just d) c
                            closeDisplay d
