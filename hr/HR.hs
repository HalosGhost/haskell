module HR where

import Data.Maybe
import Control.Monad
import qualified System.Console.Terminal.Size as Size

hr :: [String] -> Int -> [String]
hr _  0 = [""]
hr [] n = [[x | x <- take n $ repeat '#']]
hr xs n = map (take n . concat . repeat) xs

putHR :: [String] -> IO ()
putHR l = Size.size >>= mapM_ putStrLn . hr l . fromMaybe 80 . liftM Size.width
