-- ErrorCheck.hs
-- Error checking for parse and quad gen

module ErrorCheck where

  import TypeDeclarations
  import Helpers
  import Stack

  -- Get the real name of the class at the current index
  indexToString :: Int -> String
  indexToString i | i == -1 = "identifier or literal"
                  | i ==  0 = "semicolon"
                  | i ==  1 = "assignment"
                  | i ==  2 = "add/subtract"
                  | i ==  3 = "open parenthesis"
                  | i ==  4 = "close parenthesis"
                  | i ==  5 = "multiply/divide"
                  | i ==  6 = "IF"
                  | i ==  7 = "THEN"
                  | i ==  8 = "ODD"
                  | i ==  9 = "comparison operator"
                  | i == 10 = "begin block"
                  | i == 11 = "end block"
                  | i == 12 = "CALL"
                  | i == 13 = "WHILE"
                  | i == 14 = "DO"
                  | i == 15 = "comma"
                  | i == 16 = "CLASS"
                  | i == 17 = "VAR"
                  | i == 18 = "PROCEDURE"
                  | i == 19 = "CONST"
                  | i == 20 = "PRINT"
                  | i == 21 = "GET"
                  | i == 22 = "ARRAY"
                  | i == 23 = "begin subscript"
                  | i == 24 = "end subscript"
  
  -- Print all stack elements up to the first quad encountered
  printExpression :: Stack TokenOrQuad -> IO ()
  printExpression [] = putStr "..."
  printExpression (Right quad:rest) = putStr "..."
  printExpression (Left tok:rest) 
    | tok_class tok == intEnum LB = putStr "..."
    | otherwise                   = do
      printExpression rest
      name <- peekTname tok
      putStr $ "\ESC[33m " ++ name

  -- Print the list of tokens/quads in a failed quad
  printFailedQuad :: Stack TokenOrQuad -> Int -> IO ()
  printFailedQuad [] _ = return ()
  printFailedQuad (Right quad:rest) counter = do
    putStr $ "expr" ++ (show counter) ++ " "
    printFailedQuad rest $ counter + 1
  printFailedQuad (Left tok:rest) counter = do
    printToken tok
    putStr " "
    printFailedQuad rest counter

  -- Print what was expected based on the row of the previous terminal
  printExpected :: [Action] -> Int -> IO ()
  printExpected [] _ = return ()
  printExpected (act:rest) counter
    |  act == Yields
    || act == Takes
    || act == Skip = do
      putStr $ indexToString counter ++ ", "
      printExpected rest $ counter + 1
    | otherwise    = printExpected rest $ counter + 1
