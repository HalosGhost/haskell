module Roman ( toRoman
             , fromRoman
             , RomanFormat(..)
             ) where

import Data.List (group, stripPrefix)
import Data.Tuple (swap)

-- | Data type for specifying the format of the Roman Numeral
data RomanFormat
  = Additive     -- ^ Strictly Additive (e.g., IIII, not IV) and 0 == ""
  | AdditiveN    -- ^ Strictly Additive (e.g., IIII, not IV) and 0 == "N"
  | Subtractive  -- ^ Strictly Subtractive (e.g., IV, not IIII) and 0 == ""
  | SubtractiveN -- ^ Strictly Subtractive (e.g., IV, not IIII) and 0 == "N"
  deriving (Eq)

-- | Convert an integral to a Roman Numeral of the specified format
toRoman :: Integral a => RomanFormat -> a -> String
toRoman f n = case f of
  Additive     -> toAdditiveRoman'    n
  AdditiveN    -> toAdditiveRoman     n
  Subtractive  -> toSubtractiveRoman' n
  SubtractiveN -> toSubtractiveRoman  n

-- | Convert a Roman Numeral of the specified format to an integral
fromRoman :: Integral a => RomanFormat -> String -> a
fromRoman f s = case f of
  Additive     -> fromAdditiveRoman'    s
  AdditiveN    -> fromAdditiveRoman     s
  Subtractive  -> fromSubtractiveRoman' s
  SubtractiveN -> fromSubtractiveRoman  s

toAdditiveRoman :: Integral a => a -> String
toAdditiveRoman 0 = "N"
toAdditiveRoman n = toAdditiveRoman' n

toSubtractiveRoman :: Integral a => a -> String
toSubtractiveRoman = additiveToSubtractive . toAdditiveRoman

fromAdditiveRoman :: Integral a => String -> a
fromAdditiveRoman "N"    = 0
fromAdditiveRoman n      = fromAdditiveRoman' n

fromSubtractiveRoman :: Integral a => String -> a
fromSubtractiveRoman = fromAdditiveRoman . subtractiveToAdditive

toAdditiveRoman' :: Integral a => a -> String
toAdditiveRoman' 0 = ""
toAdditiveRoman' n = toRomanGlyph n' ++ (toAdditiveRoman' $ n - n')
  where n' = valToSubtract n . fst $ unzip glyphMap

toSubtractiveRoman' :: Integral a => a -> String
toSubtractiveRoman' = additiveToSubtractive . toAdditiveRoman'

fromAdditiveRoman' ""     = 0
fromAdditiveRoman' (n:ns) = fromRomanGlyph [n] + (fromAdditiveRoman' ns)

fromSubtractiveRoman' :: Integral a => String -> a
fromSubtractiveRoman' = fromAdditiveRoman' . subtractiveToAdditive

additiveToSubtractive :: String -> String
additiveToSubtractive = replace "IIII"  "IV" . replace "VIIII" "IX"
                      . replace "XXXX"  "XL" . replace "LXXXX" "XC"
                      . replace "CCCC"  "CD" . replace "DCCCC" "CM"

subtractiveToAdditive :: String -> String
subtractiveToAdditive = replace "IV" "IIII"  . replace "IX" "VIIII"
                      . replace "XL" "XXXX"  . replace "XC" "LXXXX"
                      . replace "CD" "CCCC"  . replace "CM" "DCCCC"

valToSubtract :: Integral a => a -> [a] -> a
valToSubtract n []     = 0
valToSubtract n (v:vs) = if n `div` v >= 1 then v else valToSubtract n vs

-- http://haskell.1045720.n5.nabble.com/Newbie-Replacing-substring-td3113520.html
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new xs@(y:ys) =
  case stripPrefix old xs of
    Nothing -> y : replace old new ys
    Just ys' -> new ++ replace old new ys'

glyphMap :: Integral a => [(a, String)]
glyphMap = zip [1000, 500, 100,  50,  10,   5,   1]
               ["M" , "D", "C", "L", "X", "V", "I"]

toRomanGlyph :: Integral a => a -> String
toRomanGlyph n = case lookup (fromIntegral n) glyphMap of
  Just str -> str
  Nothing  -> ""

fromRomanGlyph :: Integral a => String -> a
fromRomanGlyph s = case lookup s (map swap glyphMap) of
  Just n   -> n
  Nothing  -> 0
