module Main where

import Data.List (intercalate)
import Control.Monad (replicateM_)

invaders = [ ["   ▀▄   ▄▀   ", "  ▄▄▄████▄▄▄  ", "   ▄██▄   ", "   ▀▄   ▄▀   ", "  ▄▄▄████▄▄▄  ", "   ▄██▄   ", "  ▀▄   ▄▀   " ]
           , [ " ▄█▀███▀█▄  ", " ███▀▀██▀▀███ ", " ▄█▀██▀█▄ ", "  ▄█▀███▀█▄  ", " ███▀▀██▀▀███ ", " ▄█▀██▀█▄ ", " ▄█▀███▀█▄  " ]
           , [ "█▀███████▀█ ", " ▀▀███▀▀███▀▀ ", " ▀█▀██▀█▀ ", " █▀███████▀█ ", " ▀▀███▀▀███▀▀ ", " ▀█▀██▀█▀ ", "█▀███████▀█ " ]
           , [ "▀ ▀▄▄ ▄▄▀ ▀ ", "  ▀█▄ ▀▀ ▄█▀  ", " ▀▄    ▄▀ ", " ▀ ▀▄▄ ▄▄▀ ▀ ", "  ▀█▄ ▀▀ ▄█▀  ", " ▀▄    ▄▀ ", "▀ ▀▄▄ ▄▄▀ ▀ " ]
           ]

tank = [ "\t\t\t\t\t  ▌\n"
       , "\t\t\t\t\t▌\n"
       , "\t\t\t\t      ▄█▄"
       , "\t\t\t\t  ▄█████████▄"
       , "\t\t\t\t  ▀▀▀▀▀▀▀▀▀▀▀"
       ]

main = do replicateM_ 2 . putStrLn $ (intercalate " " $ intercalate ["\n"] invaders) ++ "\n"
          putStrLn $ intercalate "\n" tank
