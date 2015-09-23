module Main where

import Spark
import System.Environment (getArgs)

main :: IO ()
main = do as <- getArgs
          putStrLn . sparkLine $ (read <$> as :: [Double])
