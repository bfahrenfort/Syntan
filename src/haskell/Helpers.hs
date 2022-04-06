module Helpers where
  import Foreign.C.Types
  import Foreign.C.String
  import Foreign.Ptr
  import Data.Tuple (swap)
  import Data.Maybe (fromJust, isJust)
  import Text.Read (readMaybe)

  import TypeDeclarations
    
  -- Various utilities
  isDigitString :: String -> Bool
  isDigitString s = isJust (readMaybe s :: Maybe Integer)
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
  printTokenOrQuad :: TokenOrQuad -> IO ()
  printTokenOrQuad (Left t) = printToken t
  printTokenOrQuad (Right (QuadQQS op src tf dest)) = do
    putStr "("
    printTokenOrQuad $ Left op
    putStr ", "
    printTokenOrQuad $ Right src
    putStr ", "
    printTokenOrQuad $ Right tf
    putStr ", "
    printSymbol dest
    putStr ")"
  printTokenOrQuad (Right (QuadSSS op src tf dest)) = do
    putStr "("
    printTokenOrQuad $ Left op
    putStr ", "
    printSymbol src
    putStr ", "
    printSymbol tf
    putStr ", "
    printSymbol dest
    putStr ")"
  printTokenOrQuad (Right (QuadSQS op src tf dest)) = do
    putStr "("
    printTokenOrQuad $ Left op
    putStr ", "
    printSymbol src
    putStr ", "
    printTokenOrQuad $ Right tf
    putStr ", "
    printSymbol dest
    putStr ")"
  printTokenOrQuad (Right (QuadQSS op src tf dest)) = do
    putStr "("
    printTokenOrQuad $ Left op
    putStr ", "
    printTokenOrQuad $ Right src
    putStr ", "
    printSymbol tf
    putStr ", "
    printSymbol dest
    putStr ")"
  printTokenOrQuad (Right (QuadSS op src dest)) = do
    putStr "("
    printTokenOrQuad $ Left op
    putStr ", "
    printSymbol src
    putStr ", -, "
    printSymbol dest
    putStr ")"
  printTokenOrQuad (Right (QuadQS op src dest)) = do
    putStr "("
    printTokenOrQuad $ Left op
    putStr ", "
    printTokenOrQuad $ Right src
    putStr ", -, "
    printSymbol dest
  -- printTokenOrQuad (Right (QuadS op src)) = do
  --   putStr "("
  --   printTokenOrQuad $ Left op
  --   putStr ", "
  --   printSymbol src
  --   putStr ", -, -)"
  -- printTokenOrQuad (Right (QuadQ op src)) = do
  --   putStr "("
  --   printTokenOrQuad $ Left op
  --   putStr ", "
  --   printTokenOrQuad $ Right src
  --   putStr ", -, -)"
  printTokenOrQuad (Right Invalid) = putStrLn "invalid quad"

  printTokens :: [Token] -> IO ()
  printTokens [] = putStrLn ""
  printTokens (e:l) = do
    printTokenOrQuad $ Left e
    putStrLn ""
    printTokens l
  printTokensAndQuads :: [TokenOrQuad] -> IO ()
  printTokensAndQuads [] = putStrLn ""
  printTokensAndQuads (e:l) = do
    printTokenOrQuad e
    putStrLn ""
    printTokensAndQuads l

  -- Enum helpers
  intEnum :: TokenClass -> CInt 
  intEnum = fromIntegral . fromEnum -- Quick conversion
  toIndex :: TokenClass -> Integer -- Slightly modified quick conversion
  toIndex IDENT     = -1
  toIndex INTEGER   = -1
  toIndex cls       = fromJust (lookup cls token_pairs_index)
  fromIndex :: Integer -> TokenClass
  fromIndex i = fromJust (lookup i (map swap token_pairs_index))
  idx :: TokenOrQuad -> Integer -- Index in table of a token/quad
  idx (Left t)  = toIndex . toEnum . fromIntegral $ tok_class t
  idx (Right _) = -1
  isTerminal :: TokenOrQuad -> Bool -- Filter for filterTop
  isTerminal e = idx e /= -1

  