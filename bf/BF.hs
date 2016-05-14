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

type BFState = (Int, [Int])

executeBF :: [BFInstruction] -> BFState -> IO BFState
executeBF []     s       = return s
executeBF (i:is) s@(p,t) = case i of
    BFRightShift -> return (p + 1, t)
    BFLeftShift  -> return (p - 1, t)
    BFIncrement  -> return (p, incr)
    BFDecrement  -> return (p, decr)
    BFPutChar    -> putChar (toEnum $ t !! p) >> return s
    BFGetChar    -> do c <- getChar; return (p, chard c)
    BFLoopStart  -> executeBF is s
    _            -> return s
  where (b, e)  = splitAt p t
        cadre   = head e
        cabse   = tail e
        incr    = concat [b, [cadre + 1], cabse]
        decr    = concat [b, [cadre - 1], cabse]
        chard c = concat [b, [fromEnum c  ], cabse]
