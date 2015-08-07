module Radix ( RadixEncodedInt
             , fromRadixString, (♯)
             , toRadixString  , (♭)
             ) where

import Data.List
import Data.Maybe

type RadixEncodedInt = String

fromRadixChar :: Integral a => Char -> a
fromRadixChar c = fromIntegral . fromJust $ elemIndex c vals
                where vals = ['0'..'9'] ++ ['a'..'z']

valuesPerPlace :: Integral a => a -> Int -> RadixEncodedInt -> [a]
valuesPerPlace r p []     = []
valuesPerPlace r p (c:cs) | cs == []  = [n]
                          | otherwise =  n : valuesPerPlace r (p + 1) cs
                          where n = (fromRadixChar c) * r ^ p

fromRadixString, (♯) :: (Show a, Integral a) => a -> RadixEncodedInt -> a
fromRadixString r s | r < 1 || r > 36 = error rangeErrMsg
                    | invalidStr      = error $ glyphErrMsg ++ show r
                    | r == 1          = genericLength s
                    | otherwise       = sum . valuesPerPlace r 0 $ reverse s
                    where glyphErrMsg = '“' : s ++ "” is invalid in radix "
                          rangeErrMsg = "Radix must be in the range [1..36]"
                          cs          = genericTake r chars
                          chars       = '1' : '0' : ['2'..'9'] ++ ['a'..'z']
                          invalidStr  = elem False $ map (`elem` cs) s

r♯s = fromRadixString r s

toRadixChar :: Integral a => a -> Char
toRadixChar i = vals !! fromIntegral i
              where vals = ['0' .. '9'] ++ ['a' .. 'z']

toRadixString, (♭) :: Integral a => a -> a -> RadixEncodedInt
toRadixString r i | r < 1 || r > 36 = error "Radix must be in the range [1..36]"
                  | r == 1          = genericReplicate i '1'
                  | otherwise       = reverse [toRadixChar x | x <- as i r]
                  where as 0 _ = []
                        as n b = n `rem` b : as (n `div` b) b

r♭i = toRadixString r i
