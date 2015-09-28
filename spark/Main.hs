module Main where

import Spark
import System.Environment (getArgs)

main :: IO ()
main = do as <- getArgs
          let output | null as   = "Usage: spark [natural numbers]"
                     | otherwise = sparkLine $ (read <$> as :: [Double])
          putStrLn output
