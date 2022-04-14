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

  foreign import capi "syntan/interface.h asm_write" asmWrite :: CString -> IO ()
  foreign import capi "syntan/interface.h asm_data_head" dataHead :: IO ()
  foreign import capi "syntan/interface.h asm_bss" bss :: IO ()
  foreign import capi "syntan/interface.h asm_io_tail" ioTail :: IO ()
  
  cWrite :: String -> IO ()
  cWrite = flip withCString asmWrite

  -- TODO: fixups

  -- Look for certain quad combinations and turn them into more efficient versions
  optimizeQuad :: Quad -> Quad
  optimizeQuad (QuadQS _ (QuadQQS op src tf temp) dest) = 
    QuadQQS op src tf dest -- (=, (*, t1, t2, t3), -, a) -> (*, t1, t2, a)
  optimizeQuad (QuadQS _ (QuadSSS op src tf temp) dest) =
    QuadSSS op src tf dest -- (=, (*, X, Y, T1), -, A) -> (*, X, Y, A)
  optimizeQuad (QuadQS _ (QuadQSS op src tf temp) dest) =
    QuadQSS op src tf dest
  optimizeQuad quad = quad


  -- Write assembly for each arithmetic operator
  operatorCase :: (String -> IO ()) -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO ()
  operatorCase printer (Just op) (Just src) (Just tf) (Just dest)
    | op == "+" = do
      asmAction printer "mov" "ax" src False True
      asmAction printer "add" "ax" tf False True
      asmAction printer "mov" dest "ax" True False
    | op == "-" = do
      asmAction printer "mov" "ax" src False True
      asmAction printer "sub" "ax" tf False True
      asmAction printer "mov" dest "ax" True False
    | op == "*" = do
      asmAction printer "mov" "ax" src False True
      asmAction printer "mov" "bx" tf False True
      printer "mul bx\n"
      asmAction printer "mov" dest "ax" True False
    | op == "/" = do
      asmAction printer "mov" "dx" "0" False False
      asmAction printer "mov" "ax" src False True
      asmAction printer "mov" "bx" tf False True
      printer "div bx\n"
      asmAction printer "mov" dest "ax" True False
  operatorCase printer (Just op) (Just src) Nothing (Just dest)
    | op == "=" = do
      asmAction printer "mov" "ax" src False True
      asmAction printer "mov" dest "ax" True False
  operatorCase printer (Just op) (Just src) Nothing Nothing 
    | op == "PRINT" = do
      asmAction printer "mov" "ax" src False True
      printer "call ConvertIntegerToString\n"
      asmAction printer "mov" "eax" "4" False False
      asmAction printer "mov" "ebx" "1" False False
      asmAction printer "mov" "ecx" "Result" False False
      asmAction printer "mov" "edx" "ResultEnd" False False
      printer "int 80h\n"
    | op == "CALL" = do
      printer $ "call " ++ src ++ "\n"
    | op == "GET" = do
      printer "call PrintString\ncall GetAnInteger\n"
      asmAction printer "mov" src "ax" True False


  -- Perform operations on the assembly file based on quad type
  patternMatch :: Quad -> (String -> IO ()) -> IO ()
  patternMatch (QuadQQS op src tf dest) printer = do
    putStrLn "QuadQQS"
    patternMatch src printer
    patternMatch tf printer
    op_str <- peekCString $ tname op
    src_str <- qname src
    tf_str <- qname tf
    dest_str <- peekCString $ sname dest
    operatorCase printer
                 (Just op_str) 
                 (Just src_str) 
                 (Just tf_str) 
                 (Just dest_str)
  patternMatch (QuadSSS op src tf dest) printer = do
    putStrLn "QuadSSS"
    op_str <- peekCString $ tname op
    src_str <- peekCString $ sname src
    tf_str <- peekCString $ sname tf
    dest_str <- peekCString $ sname dest
    operatorCase printer (Just op_str) (Just src_str) (Just tf_str) (Just dest_str)
  patternMatch (QuadSQS op src tf dest) printer = do
    putStrLn "QuadSQS"
    patternMatch tf printer
    op_str <- peekCString $ tname op
    src_str <- peekCString $ sname src
    tf_str <- qname tf
    dest_str <- peekCString $ sname dest
    operatorCase printer (Just op_str) (Just src_str) (Just tf_str) (Just dest_str)
  patternMatch (QuadQSS op src tf dest) printer = do
    putStrLn "QuadQSS"
    patternMatch src printer
    op_str <- peekCString $ tname op
    src_str <- qname src
    tf_str <- peekCString $ sname tf
    dest_str <- peekCString $ sname dest
    operatorCase printer (Just op_str) (Just src_str) (Just tf_str) (Just dest_str)
  patternMatch (QuadSS op src dest) printer = do
    putStrLn "QuadSS"
    op_str <- peekCString $ tname op
    src_str <- peekCString $ sname src
    dest_str <- peekCString $ sname dest
    operatorCase printer (Just op_str) (Just src_str) Nothing (Just dest_str)
  patternMatch (QuadQS op src dest) printer = do
    putStrLn "QuadQS"
    patternMatch src printer
    op_str <- peekCString $ tname op
    src_str <- qname src
    dest_str <- peekCString $ sname dest
    operatorCase printer (Just op_str) (Just src_str) Nothing (Just dest_str)
  patternMatch (QuadS op src) printer = do
    putStrLn "QuadS"
    op_str <- peekCString $ tname op
    src_str <- peekCString $ sname src
    operatorCase printer (Just op_str) (Just src_str) Nothing Nothing
  patternMatch any printer = do
    putStrLn "Writing a Quad (not really)"
    printer "; there's a quad here, I know it\n"


  --FIXME: re-enable optimization after debugging if and while
  generateASM :: [TokenOrQuad] -> IO ()
  generateASM [Right quad] = do
    --let optimized = optimizeQuad quad
    --patternMatch optimized cWrite
    patternMatch quad cWrite
  generateASM (Right quad:rest) = do
    --let optimized = optimizeQuad quad
    --patternMatch optimized cWrite
    patternMatch quad cWrite
    generateASM rest


  asmAction :: (String -> IO ()) -> String -> String -> String -> Bool -> Bool -> IO ()
  asmAction printer action dest src idest isrc = do
    printer $ action ++ " " 

    if idest then printer ("[" ++ dest ++ "], ") else printer $ dest ++ ", "

    if "INT" `isPrefixOf` src then
      printer $ drop 3 src -- Literal instead of memory
    else if isrc then printer ("[" ++ src ++ "]") else printer src

    printer "\n"


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
    |  fromIntegral (sym_class symbol) == 2 -- SVAR
    || fromIntegral (sym_class symbol) == 6 = do -- SCONST
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
    | otherwise = do
      x <- peekCString $ sname symbol
      putStrLn x
      let y = fromIntegral $ sym_class symbol
      print y
      writeSymbols rest
  
  asmFinalize :: IO ()
  asmFinalize = ioTail