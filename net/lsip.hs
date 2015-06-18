module Main where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as L (putStr)

main = simpleHttp "https://icanhazip.com" >>= L.putStr
