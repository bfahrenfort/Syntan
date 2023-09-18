-- Avengers, assemble!
-- Assembly.hs

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Assembly where
  import Foreign.C.Types
  import Foreign.C.String
  import Foreign.Ptr
  import Data.List (isPrefixOf)

  import TypeDeclarations

  -- TODO: C interface for writing to assembly file
  foreign import capi "syntan/interface.h asm_write" asmWrite :: CString -> IO ()
  foreign import capi "syntan/interface.h asm_data_head" dataHead :: IO ()
  foreign import capi "syntan/interface.h asm_bss" bss :: IO ()
  foreign import capi "syntan/interface.h asm_io_tail" ioTail :: IO ()
  
  cWrite :: String -> IO ()
  cWrite = flip withCString asmWrite

  -- Look for certain quad combinations and turn them into more efficient versions
  optimizeQuad :: Quad -> Quad
  optimizeQuad (QuadQS _ (QuadQQS op src tf temp) dest) = 
    QuadQQS op src tf dest -- (=, (*, t1, t2, t3), -, a) -> (*, t1, t2, a)
  optimizeQuad (QuadQS _ (QuadSSS op src tf temp) dest) =
    QuadSSS op src tf dest -- (=, (*, X, Y, T1), -, A) -> (*, X, Y, A)
  optimizeQuad quad = quad

  operatorCase :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO ()
  operatorCase (Just op) (Just src) (Just tf) (Just dest)
    | op == "+" = do
      mov src "ax" True False

      cWrite "add ax, ["
      cWrite tf
      cWrite "]\n"

      mov "ax" dest False True
    | op == "-" = do
      mov src "ax" True False

      cWrite "sub ax, ["
      cWrite tf
      cWrite "]\n"

      mov "ax" dest False True
    | op == "*" = do
      mov src "ax" True False

      mov tf "bx" True False

      cWrite "mul bx\n"

      mov "ax" dest False True
    | op == "/" = do
      cWrite "mov dx, 0\n"

      cWrite "mov ax, ["
      cWrite src
      cWrite "]\n"

      cWrite "mov bx, ["
      cWrite tf
      cWrite "]\n"

      cWrite "div bx\n"

      cWrite "mov ["
      cWrite dest
      cWrite "], ax\n"
  operatorCase (Just op) (Just src) Nothing Nothing 
    | op == "PRINT" = do
      mov src "ax" True False
      cWrite "call ConvertIntegerToString\n"
      mov "4" "eax" False False
      mov "1" "ebx" False False
      mov "Result" "ecx" False False
      mov "ResultEnd" "edx" False False
      cWrite "int 80h\n"
      
  operatorCase _ _ _ _ = putStrLn "Writing a quad (not really)"

  -- Write the assembly for each type of quad
  patternMatch :: Quad -> IO ()
  patternMatch (QuadQQS op src tf dest) = do
    putStrLn "QuadQQS"
    patternMatch src
    patternMatch tf
    op_str <- peekCString $ tname op
    src_str <- qname src
    tf_str <- qname tf
    dest_str <- peekCString $ sname dest
    operatorCase (Just op_str) 
                 (Just src_str) 
                 (Just tf_str) 
                 (Just dest_str)
  patternMatch (QuadSSS op src tf dest) = do
    putStrLn "QuadSSS"
    op_str <- peekCString $ tname op
    src_str <- peekCString $ sname src
    tf_str <- peekCString $ sname tf
    dest_str <- peekCString $ sname dest
    operatorCase (Just op_str) (Just src_str) (Just tf_str) (Just dest_str)
  patternMatch (QuadS op src) = do
    putStrLn "QuadS"
    op_str <- peekCString $ tname op
    src_str <- peekCString $ sname src
    operatorCase (Just op_str) (Just src_str) Nothing Nothing


  generateASM :: [TokenOrQuad] -> IO ()
  generateASM [Right quad] = do
    let optimized = optimizeQuad quad
    patternMatch optimized
  generateASM (Right quad:rest) = do
    let optimized = optimizeQuad quad
    patternMatch optimized
    generateASM rest


  mov :: String -> String -> Bool -> Bool -> IO ()
  mov src dest isrc idest = do
    cWrite "mov "

    if idest then cWrite ("[" ++ dest ++ "], ") else cWrite $ dest ++ ", "

    if "INT" `isPrefixOf` src then
      cWrite $ drop 3 src -- Literal instead of memory
    else if isrc then cWrite ("[" ++ src ++ "]") else cWrite src

    cWrite "\n"


  qname :: Quad -> IO String
  qname (QuadQQS _ _ _ dest) = peekCString $ sname dest
  qname (QuadSSS _ _ _ dest) = peekCString $ sname dest
  qname (QuadSQS _ _ _ dest) = peekCString $ sname dest
  qname (QuadQSS _ _ _ dest) = peekCString $ sname dest
  qname (QuadSS _ _ dest) = peekCString $ sname dest
  qname (QuadQS _ _ dest) = peekCString $ sname dest
  qname (QuadIW op _ _) = peekCString $ tname op
  qname (QuadB block) = peekCString $ sname block


  asmSetup :: [Symbol] -> IO ()
  asmSetup symbols = do
    dataHead
    writeSymbols symbols
    putStrLn "setup symbol table"

    bss
    putStrLn "added bss"

  writeSymbols :: [Symbol] -> IO ()
  writeSymbols [] = cWrite "\n"
  writeSymbols (symbol:rest) 
    | fromIntegral (sym_class symbol) == 2 = do -- SVAR
      cWrite "\n"
      --sym_name <- peekCString $ sname symbol
      --sym_value <- peekCString $ value symbol
      asmWrite $ sname symbol
      cWrite " DW "
      asmWrite $ value symbol
      -- putStr sym_name
      -- putStr " "
      -- putStrLn sym_value
      
      writeSymbols rest
    | otherwise = writeSymbols rest
  
  asmFinalize :: IO ()
  asmFinalize = ioTail