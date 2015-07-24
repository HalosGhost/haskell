module Radix ( Radix          , RadixEncodedInt
             , fromRadixString, (♯)
             , toRadixString  , (♭)
             ) where

import Data.List
import Data.Maybe

type Radix           = Integer
type RadixEncodedInt = String

fromRadixChar :: Char -> Integer
fromRadixChar c = toInteger . fromJust $ elemIndex c vals
                where vals = ['0' .. '9'] ++ ['a' .. 'z']

fromRadixString :: Radix -> RadixEncodedInt -> Integer
fromRadixString r s | r < 1 || r > 36  = error rangeErrMsg
                    | invalidStr       = error $ glyphErrMsg ++ show r
                    | otherwise        = sum . valuesPerPlace r 0 $ reverse s
                    where glyphErrMsg = '“' : s ++ "” is invalid in radix "
                          rangeErrMsg = "Radix must be in the range [1..36]"
                          cs          = genericTake r chars
                          chars       = '1' : '0' : ['2' .. '9'] ++ ['a' .. 'z']
                          invalidStr  = elem False $ map (`elem` cs) s

valuesPerPlace :: Radix -> Integer -> RadixEncodedInt -> [Integer]
valuesPerPlace r p []     = []
valuesPerPlace r p (c:cs) | cs == []  = [n]
                          | otherwise =  n : valuesPerPlace r (p + 1) cs
                          where n = (fromRadixChar c) * r ^ p

(♯) :: Radix -> RadixEncodedInt -> Integer
r♯s = fromRadixString r s

toRadixChar :: Integer -> Char
toRadixChar i = vals !! fromInteger i
              where vals = ['0' .. '9'] ++ ['a' .. 'z']

toRadixString :: Radix -> Integer -> RadixEncodedInt
toRadixString r i | r < 1 || r > 36 = error "Radix must be in the range [1..36]"
                  | otherwise       = "1" -- Finish implementing

(♭) :: Radix -> Integer -> RadixEncodedInt
r♭i = toRadixString r i
