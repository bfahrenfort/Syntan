{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Helpers where
  import Foreign.C.Types
  import Foreign.C.String
  import Foreign.Ptr
  import Data.Tuple (swap)
  import Data.Maybe (fromJust, isJust)
  import Text.Read (readMaybe)

  import TypeDeclarations

  printSymbol :: Symbol -> IO ()
  printSymbol s = do
    str <- peekCString $ sname s
    putStr str
  printToken :: Token -> IO ()
  printToken t = do
    if tname t /= nullPtr then do
      s <- peekCString $ tname t
      putStr s
    else putStr "NULL STR"
  printQuad :: Quad -> IO ()
  printQuad (QuadQQS op src tf dest) = do
    putStr "("
    printToken op
    putStr ", "
    printQuad src
    putStr ", "
    printQuad tf
    putStr ", "
    printSymbol dest
    putStr ")"
  printQuad (QuadSSS op src tf dest) = do
    putStr "("
    printToken op
    putStr ", "
    printSymbol src
    putStr ", "
    printSymbol tf
    putStr ", "
    printSymbol dest
    putStr ")"
  printQuad (QuadSQS op src tf dest) = do
    putStr "("
    printToken op
    putStr ", "
    printSymbol src
    putStr ", "
    printQuad tf
    putStr ", "
    printSymbol dest
    putStr ")"
  printQuad (QuadQSS op src tf dest) = do
    putStr "("
    printToken op
    putStr ", "
    printQuad src
    putStr ", "
    printSymbol tf
    putStr ", "
    printSymbol dest
    putStr ")"
  printQuad (QuadSS op src dest) = do
    putStr "("
    printToken op
    putStr ", "
    printSymbol src
    putStr ", -, "
    printSymbol dest
    putStr ")"
  printQuad (QuadQS op src dest) = do
    putStr "("
    printToken op
    putStr ", "
    printQuad src
    putStr ", -, "
    printSymbol dest
    putStr ")"
  printQuad (QuadQ op src ) = do
    putStr "("
    printToken op
    putStr ", "
    printQuad src
    putStr ", -, -)"
  printQuad (QuadS op src ) = do
    putStr "("
    printToken op
    putStr ", "
    printSymbol src
    putStr ", -, -)"
  printQuad (QuadIW iwop cond stmt) = do
    putStr "("
    printToken iwop
    putStr ", "
    printQuad cond
    putStr ", "
    printQuad stmt
    putStr ", -)"
  printQuad (QuadB block) = do
    printSymbol block
  printQuad (QuadP xproc sproc block) = do
    putStr "("
    printToken xproc
    putStr ", "
    printSymbol sproc
    putStr ", "
    printQuad block
    putStr ", -)"
  printQuad Invalid = putStrLn "invalid quad"

  printTokenOrQuad :: TokenOrQuad -> IO ()
  printTokenOrQuad (Left t) = printToken t
  printTokenOrQuad (Right t) = printQuad t

  printTokens :: [Token] -> IO ()
  printTokens [] = putStrLn ""
  printTokens (e:l) = do
    printToken e
    putStrLn ""
    printTokens l
  printTokensAndQuads :: [TokenOrQuad] -> IO ()
  printTokensAndQuads [] = putStrLn ""
  printTokensAndQuads (e:l) = do
    printTokenOrQuad e
    putStrLn ""
    printTokensAndQuads l

  -- Convert from an enum value to compare against a tok_class field
  intEnum :: TokenClass -> CInt 
  intEnum = fromIntegral . fromEnum 

  -- Convert to/from an enum value to an index in the precedence matrix
  toIndex :: TokenClass -> Integer
  toIndex IDENT     = -1
  toIndex INTEGER   = -1
  toIndex cls       = fromJust (lookup cls token_pairs_index)
  fromIndex :: Integer -> TokenClass
  fromIndex i = fromJust (lookup i (map swap token_pairs_index))
  idx :: TokenOrQuad -> Integer -- Index in table of a token/quad
  idx (Left t)  = toIndex . toEnum . fromIntegral $ tok_class t
  idx (Right _) = -2

  -- Pattern match helper
  initPlusLast :: [a] -> Maybe ([a], a)
  initPlusLast lst@(_:_) = Just (init lst, last lst)
  initPlusLast _ = Nothing

  -- Pattern for when a list is an element, a list, and another element
  -- Used in combination with tok_class guards to identify code blocks
  pattern Block lfst lmid lend <- lfst : (initPlusLast -> Just (lmid, lend))

  pattern Program lschwarz lres lname lfst lmid lend <- lschwarz
                                                      : lres
                                                      : lname
                                                      : lfst
                                                      : (initPlusLast -> Just (lmid, lend))
  -- Lookup a symbol in the table by name or value
  lookupSymbol :: String -> [Symbol] -> IO (Maybe Symbol)
  lookupSymbol _ [] = do return Nothing
  lookupSymbol target (sym:rest) = do
    name <- peekCString $ sname sym
    if name == target then return $ Just sym
    else lookupSymbol target rest
  --lookupSymbol target (sym:rest) False = do -- look by value

  -- Perform a different lookup type based on the token class of a token
  -- Note: (Maybe Symbol -> Symbol) <$> IO (Maybe Symbol) gives an IO Symbol
  symbolFromToken :: Token -> [Symbol] -> IO Symbol
  symbolFromToken tok symbols 
    | tok_class tok == intEnum INTEGER = do -- Lookup as INT(name)
      name <- peekCString $ tname tok 
      fromJust <$> lookupSymbol ("INT" ++ name) symbols
    | otherwise                        = do -- Lookup by passed in name
      name <- peekCString $ tname tok
      fromJust <$> lookupSymbol name symbols

  -- Is this string only digits? (Integer literal)
  isDigitString :: String -> Bool
  isDigitString s = isJust (readMaybe s :: Maybe Integer)

  -- Is this Token/Quad a terminal? (operator, part of the precedence matrix)
  isTerminal :: TokenOrQuad -> Bool -- Filter for filterTop
  isTerminal e = idx e >= 0

  -- Is this Token a terminal? (operator, part of the precedence matrix)
  isTerminalToken :: Token -> Bool -- Filter for filterTop
  isTerminalToken e = idx (Left e) >= 0

  isBlock :: TokenOrQuad -> Bool
  isBlock (Right (QuadB _)) = True
  isBlock _ = False