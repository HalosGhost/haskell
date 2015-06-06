module Main where

import HR
import System.Environment

main :: IO ()
main = getArgs >>= HR.putHR
