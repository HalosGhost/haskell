{-# LANGUAGE FlexibleContexts #-}

module Spark (sparkLine) where

import qualified Data.ListLike as LL
import           Data.ListLike    (ListLike, StringLike)

sparkChar :: RealFrac f => f -> Char
sparkChar n = "▂▃▄▅▆▇█" !! (round $ n * 6)

normalize :: (ListLike l a, RealFrac a) => l -> l
normalize is = (/n) `LL.map` is
        where n = LL.maximum is

sparkLine :: (ListLike l a, RealFrac a, ListLike s Char, StringLike s) => l -> s
sparkLine is = LL.map sparkChar $ normalize is
