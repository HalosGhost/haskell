{-# LANGUAGE FlexibleContexts #-}

module Spark (sparkLine) where

import qualified Data.Foldable as F

sparkChar :: RealFrac f => f -> Char
sparkChar n = "▂▃▄▅▆▇█" !! (round $ n * 6)

normalize :: (Traversable t, Foldable t, Ord a, RealFrac a) => t a -> t a
normalize is = (/n) <$> is
        where n = F.maximum is

sparkLine :: (Traversable t, Foldable t, Ord a, RealFrac a) => t a -> t Char
sparkLine is = sparkChar <$> (normalize is)
