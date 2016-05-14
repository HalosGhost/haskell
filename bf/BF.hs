module Brainfuck where

data BFInstruction
  = BFIgnored
  -- | BFPragma BFPragmaValue
  | BFIncrement
  | BFDecrement
  | BFRightShift
  | BFLeftShift
  | BFPutChar
  | BFGetChar
  | BFLoopStart
  | BFLoopEnd
  deriving (Eq)

{-
data BFPragmaValue
  = BFToggleDebug
  | BFEOFBehavior Int
  | BF
-}

toBFInst :: Char -> BFInstruction
toBFInst c
  = case c of
      '+' -> BFIncrement
      '-' -> BFDecrement
      '>' -> BFRightShift
      '<' -> BFLeftShift
      '.' -> BFPutChar
      ',' -> BFGetChar
      '[' -> BFLoopStart
      ']' -> BFLoopEnd
      _   -> BFIgnored

