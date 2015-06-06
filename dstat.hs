import System.Process
import Data.Time

bat_loc = "/sys/class/power_supply/"
en_loc  = "/sys/class/net/"
wl_loc  = "/proc/net/wireless"

bat_level :: String -> IO Int
bat_level b = do cap <- readFile $ bat_loc ++ b ++ "/capacity"
                 return . read $ take 2 cap

-- My very own ternary operator!
infix 1 ?
(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y

-- eventually replace with an ACPI module
bat_status :: String -> IO Char
bat_status b = do stat <- readFile $ bat_loc ++ b ++ "/status"
                  return $ head stat == 'C' ? 'ϟ' $ 'D'

bat_comp :: String -> IO String
bat_comp b = do l <- bat_level b
                s <- bat_status b
                return $ show l ++ [s]

-- eventually replace with a pulseaudio module
vol :: IO Int
vol = do v <- readProcess "ponymix" ["get-volume"] []
         return $ read $ init v

en_stat :: String -> IO String
en_stat e = do stat <- readFile $ en_loc ++ e ++ "/operstate"
               return $ head stat == 'u' ? "U" $ "D"

wl_str :: String -> IO Int
wl_str s = do stat <- readFile $ wl_loc
              let status_line = words . last . lines $ stat
              let connected   = (init . head $ status_line) == s
              return $ connected ? (read $ take 2 $ status_line!!2) $ -1

wl_bars :: Int -> String
wl_bars s | s < 0     = "No Signal" | s <= 10 = "▂"
          | s <= 20   = "▂▃"        | s <= 30 = "▂▃▄"
          | s <= 40   = "▂▃▄▅"      | s <= 50 = "▂▃▄▅▆"
          | s <= 60   = "▂▃▄▅▆▇"    | s <= 70 = "▂▃▄▅▆▇█"
          | otherwise = "No Signal"

cur_time :: String -> IO String
cur_time f = getZonedTime >>= return . formatTime defaultTimeLocale f

main :: IO ()
--main = vol >>= putStrLn . show
--main = bat_comp "BAT0" >>= putStrLn
--main = en_stat "enp0s25" >>= putStrLn
--main = wl_str "wlp3s0" >>= putStrLn . wl_bars
main = cur_time "%H.%M (%Z) | %A, %d %B %Y" >>= putStrLn
