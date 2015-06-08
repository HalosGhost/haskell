module Main where

import System.Process (readProcess)
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import Control.Concurrent (threadDelay)
import System.Command (runCommand, waitForProcess, exitCode)
import Graphics.X11.Xlib.Event (sync)
import Graphics.X11.Xlib.Display (defaultRootWindow, openDisplay, closeDisplay)
import Graphics.X11.Xlib.Types (Display)
import Graphics.X11.Xlib.Window (storeName)

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

stats ::IO String
stats = do b <- bat_comp "BAT0"
           v <- vol_comp
           e <- en_stat "enp0s25"
           wl <- wl_str "wlp3s0"
           let w = "W: " ++ wl_bars wl
           t <- cur_time "%H.%M (%Z) | %A, %d %B %Y"
           return $ e <|> w <|> v <|> b <|> t

status :: Display -> IO ()
status d = do let w = defaultRootWindow d
              s <- stats
              storeName d w s
              sync d False
              threadDelay 6000000
              status d

main :: IO ()
main = do d <- openDisplay ""
          status d
          closeDisplay d
