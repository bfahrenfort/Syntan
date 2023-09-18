-- Helpers.hs
-- Various functions for printing or going back and forth between representations

{-# LANGUAGE PatternSynonyms #-}          -- Like a macro, but for pattern matching
{-# LANGUAGE ViewPatterns #-}             -- Like a function, but for pattern matching
{-# LANGUAGE ForeignFunctionInterface #-} -- The usual
{-# LANGUAGE CApiFFI #-}

module Helpers where
  import Foreign.C.Types
  import Foreign.C.String
  import Foreign.Ptr
  import Foreign.Storable
  import Data.Tuple (swap)
  import Data.Maybe (fromJust, isJust)
  import Text.Read (readMaybe)

  import TypeDeclarations
  import Stack
  import Assembly

  printSymbol :: Symbol -> IO ()
  printSymbol s = do
    str <- peekCString $ sname s
    putStr str
  printToken :: Token -> IO ()
  printToken t = do
    if tname t /= nullPtr then do
      s <- peekTname t
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
  printQuad (QuadIdS src idx dest) = do
    putStr "("
    printSymbol src
    putStr "["
    printSymbol idx
    putStr "], -, "
    printSymbol dest
    putStr ")"
  printQuad (QuadIdQ src idx dest) = do
    putStr "("
    printSymbol src
    putStr "["
    printQuad idx
    putStr "], -, "
    printSymbol dest
    putStr ")"
  printQuad (QuadQQ op src dest) = do
    putStr "("
    printToken op
    putStr ", "
    printQuad src
    putStr ", -, "
    printQuad dest
    putStr ")"
  printQuad (QuadSQ op src dest) = do
    putStr "("
    printToken op
    putStr ", "
    printSymbol src
    putStr ", -, "
    printQuad dest
    putStr ")"
  printQuad Invalid = putStrLn "invalid quad"

  printTokenOrQuad :: TokenOrQuad -> IO ()
  printTokenOrQuad (Left t) = printToken t
  printTokenOrQuad (Right t) = printQuad t

  printSymbols :: [Symbol] -> IO ()
  printSymbols [] = putStrLn ""
  printSymbols (e:l) = do
    printSymbol e
    putStrLn ""
    printSymbols l
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

  evaluatorDummy :: Symbol -> IO ()
  evaluatorDummy _ = return ()

  -- Analyze a stack of quads/tokens to turn it into a real quad
  toQuad :: Stack TokenOrQuad -> [Symbol] -> IO (Quad, [Symbol])
  toQuad [Left lft, Left mid, Left rt] symbols
    | tok_class mid == intEnum ASSIGN = do
      dest_sym <- symbolFromToken lft symbols
      src_sym <- symbolFromToken rt symbols
      return (QuadSS mid src_sym dest_sym, symbols)                -- =, src_sym, -, dest_sym
    | otherwise = do
      src_sym <- symbolFromToken lft symbols
      tf_sym <- symbolFromToken rt symbols
      temp <- tempAdd >>= peek
      return (QuadSSS mid src_sym tf_sym temp, temp:symbols)            -- +, src_sym, tf_sym, temp
  toQuad [Left lft, Left mid, Right rt] symbols
    | tok_class mid == intEnum ASSIGN = do
      dest_sym <- symbolFromToken lft symbols
      return (QuadQS mid rt dest_sym, symbols)                    -- =, rt, -, dest_sym,
    | otherwise = do
      src_sym <- symbolFromToken lft symbols
      temp <- tempAdd >>= peek
      return (QuadSQS mid src_sym rt temp, temp:symbols)               -- +, src_sym, rt (actually tf), temp
  toQuad [Right lft, Left mid, Left rt] symbols
    |  tok_class mid == intEnum ASSIGN = do
      src_sym <- symbolFromToken rt symbols
      return (QuadSQ mid src_sym lft, symbols)                         -- =, src, -, arr[0]
    | otherwise                       = do
      tf_sym <- symbolFromToken rt symbols
      temp <- tempAdd >>= peek
      return (QuadQSS mid lft tf_sym temp, temp:symbols)               -- +, lft, tf_sym, temp
  toQuad [Right lft, Left mid, Right rt] symbols
    | tok_class mid == intEnum ASSIGN = do
      return (QuadQQ mid rt lft, symbols)                           -- =, T1, -, arr[0]
    | otherwise                       = do
      temp <- tempAdd >>= peek
      return (QuadQQS mid lft rt temp, temp:symbols)                      -- +, lft, rt, temp
  toQuad [Left op, Left src] symbols = do
    src_sym <- symbolFromToken src symbols
    return (QuadS op src_sym, symbols)                            -- ODD, src, -, -
  toQuad [Left op, Right src] symbols = do
    return (QuadQ op src, symbols)                                -- ODD, src, -. -
  toQuad [Left lft, Left ml, Left mr, Left rt] symbols 
    |  tok_class lft == intEnum CALL
    && tok_class mr  == intEnum LP
    && tok_class rt  == intEnum RP = do
      src_sym <- symbolFromToken ml symbols
      return (QuadS lft src_sym, symbols)                        -- PROCEDURE, Multiply, B1
    |  tok_class lft == intEnum IDENT
    && tok_class ml  == intEnum LS
    && tok_class rt  == intEnum RS = do -- Subscripting
      src_sym <- symbolFromToken lft symbols
      idx_sym <- symbolFromToken mr symbols
      temp <- tempAdd >>= peek
      return (QuadIdS src_sym idx_sym temp, temp:symbols)                     -- arr, INT0
  toQuad [Left lft, Left ml, Right mr, Left rt] symbols |  tok_class lft == intEnum IDENT
                                                        && tok_class ml  == intEnum LS
                                                        && tok_class rt  == intEnum RS = do
    src_sym <- symbolFromToken lft symbols
    temp <- tempAdd >>= peek
    return (QuadIdQ src_sym mr temp, temp:symbols)

  toQuad [Left iwop, Right cond, Left thenop, Right stmt] symbols = do
    return (QuadIW iwop cond stmt, symbols)
  toQuad (Block (Left lb) quads (Left rb)) symbols |  tok_class lb == intEnum LB
                                                  && tok_class rb == intEnum RB = do
    block <- blockAdd >>= peek
    block_name <- peekCString $ sname block
    putStrLn $ "Quads in block " ++ block_name ++ ":"
    printTokensAndQuads quads
    generateASM quads $ Just block
    return (QuadB block, symbols)
  toQuad [Left xproc, Left tproc, Left lp, Left rp, Right block] symbols = do
    proc_sym <- symbolFromToken tproc symbols
    return (QuadP xproc proc_sym block, symbols)
  toQuad _ symbols = return (Invalid, symbols)

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
      name <- peekTname tok 
      fromJust <$> lookupSymbol ("INT" ++ name) symbols
    | otherwise                        = do -- Lookup by passed in name
      name <- peekTname tok
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