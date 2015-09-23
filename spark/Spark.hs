module Spark (sparkLine) where

import Data.Foldable (toList)

sparkChar :: RealFrac f => f -> Char
sparkChar n = "▂▃▄▅▆▇█" !! (round $ n * 6)

normalize :: (Functor t, Foldable t, RealFrac f) => t f -> t f
normalize is = (/ (maximum is)) <$> is

sparkLine :: (Functor t, Foldable t, RealFrac a) => t a -> String
sparkLine is = toList $ sparkChar <$> normalize is
