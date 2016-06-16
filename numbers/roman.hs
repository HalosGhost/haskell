module Roman ( toRoman
             , fromRoman
             , RomanFormat(..)
             ) where

import Data.List (stripPrefix)
import Data.Tuple (swap)

-- | Data type for specifying the format of the Roman Numeral.
data RomanFormat
  = Additive     -- ^ Strictly Additive (e.g., IIII, not IV) and 0 == ""
  | AdditiveN    -- ^ Strictly Additive (e.g., IIII, not IV) and 0 == "N"
  | Subtractive  -- ^ Strictly Subtractive (e.g., IV, not IIII) and 0 == ""
  | SubtractiveN -- ^ Strictly Subtractive (e.g., IV, not IIII) and 0 == "N"
  | Lenient      -- ^ Lenient Subtractive (e.g., IM == 999) and 0 == ""
  | LenientN     -- ^ Lenient Subtractive (e.g., IM == 999) and 0 == "N"
  deriving (Eq)

-- | Convert an integral to a Roman Numeral of the specified format.
-- Because Lenient forms do not have single representation of all numbers
-- and because all Subtractive forms are valid Lenient forms,
-- `toRoman Lenient{,N}` will use the Subtractive forms.
toRoman :: Integral a => RomanFormat -> a -> String
toRoman f n = case f of
  Additive     -> toAdditiveRoman'    n
  AdditiveN    -> toAdditiveRoman     n
  Subtractive  -> toSubtractiveRoman' n
  SubtractiveN -> toSubtractiveRoman  n
  Lenient      -> toSubtractiveRoman' n
  LenientN     -> toSubtractiveRoman  n

-- | Convert a Roman Numeral of the specified format to an integral.
fromRoman :: Integral a => RomanFormat -> String -> a
fromRoman f s = case f of
  Additive     -> fromAdditiveRoman'    s
  AdditiveN    -> fromAdditiveRoman     s
  Subtractive  -> fromSubtractiveRoman' s
  SubtractiveN -> fromSubtractiveRoman  s
  Lenient      -> fromLenientRoman'     s
  LenientN     -> fromLenientRoman      s

toAdditiveRoman :: Integral a => a -> String
toAdditiveRoman 0 = "N"
toAdditiveRoman n = toAdditiveRoman' n

toSubtractiveRoman :: Integral a => a -> String
toSubtractiveRoman = additiveToSubtractive . toAdditiveRoman

fromAdditiveRoman :: Integral a => String -> a
fromAdditiveRoman "N" = 0
fromAdditiveRoman n   = fromAdditiveRoman' n

fromSubtractiveRoman :: Integral a => String -> a
fromSubtractiveRoman = fromAdditiveRoman . subtractiveToAdditive

fromLenientRoman :: Integral a => String -> a
fromLenientRoman "N" = 0
fromLenientRoman s   = fromLenientRoman' s

toAdditiveRoman' :: Integral a => a -> String
toAdditiveRoman' 0 = ""
toAdditiveRoman' n = toRomanGlyph n' ++ (toAdditiveRoman' $ n - n')
  where n' = valToSubtract n . fst $ unzip glyphMap

toSubtractiveRoman' :: Integral a => a -> String
toSubtractiveRoman' = additiveToSubtractive . toAdditiveRoman'

fromAdditiveRoman' :: Integral a => String -> a
fromAdditiveRoman' ""     = 0
fromAdditiveRoman' (n:ns) = fromRomanGlyph [n] + (fromAdditiveRoman' ns)

fromSubtractiveRoman' :: Integral a => String -> a
fromSubtractiveRoman' = fromAdditiveRoman' . subtractiveToAdditive

fromLenientRoman' :: Integral a => String -> a
fromLenientRoman' ""     = 0
fromLenientRoman' [n]    = fromRomanGlyph [n]
fromLenientRoman' (n:ns) = (fromLenientRoman' ns) + if c >= x then c else (- c)
  where c = fromRomanGlyph [n]
        x = fromRomanGlyph [head ns]

substitutionMap = zip ["IIII", "VIIII", "XXXX", "LXXXX", "CCCC", "DCCCC"]
                      ["IV",   "IX",    "XL",   "XC",    "CD",   "CM"]

additiveToSubtractive :: String -> String
additiveToSubtractive s = foldr replace s substitutionMap

subtractiveToAdditive :: String -> String
subtractiveToAdditive s = foldr replace s $ map swap substitutionMap

valToSubtract :: Integral a => a -> [a] -> a
valToSubtract n []     = 0
valToSubtract n (v:vs) = if n `div` v >= 1 then v else valToSubtract n vs

-- http://haskell.1045720.n5.nabble.com/Newbie-Replacing-substring-td3113520.html
replace :: Eq a => ([a],[a]) -> [a] -> [a]
replace _ [] = []
replace (old,new) xs@(y:ys) =
  case stripPrefix old xs of
    Nothing -> y : replace (old,new) ys
    Just ys' -> new ++ replace (old,new) ys'

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
