-- Avengers, assemble!
-- Assembly.hs

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Assembly where
  import Foreign.C.Types
  import Foreign.C.String
  import Foreign.Storable
  import Foreign.Ptr
  import Data.List (isPrefixOf)

  import TypeDeclarations

  -- Writing and appending intermediate blocks to the main file
  foreign import capi "syntan/interface.h asm_write" asmWrite :: CString -> IO ()
  foreign import capi "syntan/interface.h asm_f_append" asmFAppend :: CString -> IO ()
  foreign import capi "syntan/interface.h asm_data_head" dataHead :: IO ()
  foreign import capi "syntan/interface.h asm_bss" bss :: IO ()
  foreign import capi "syntan/interface.h asm_io_tail" ioTail :: IO ()
  foreign import capi "syntan/interface.h asm_exit" asmExit :: IO ()

  -- Cheating the system to avoid fixups
  foreign import capi "syntan/interface.h block_add" blockAdd :: IO (Ptr Symbol)
  foreign import capi "syntan/interface.h temp_add" tempAdd :: IO (Ptr Symbol)

  -- More cheating, but for procedures
  foreign import capi "syntan/interface.h asm_append_procedures" appendProcedures :: IO ()
  foreign import capi "syntan/interface.h procedure_add" procedureAdd :: CString -> IO ()

  -- Creating and writing to an intermediate file for a block
  foreign import capi "syntan/interface.h blockfile_open" blockfileOpen :: CString -> IO ()
  foreign import capi "syntan/interface.h blockfile_write" blockAsmWrite :: CString -> IO ()
  foreign import capi "syntan/interface.h blockfile_close" blockfileClose :: IO ()

  mainWrite :: String -> IO ()
  mainWrite = flip withCString asmWrite
  blockfileWrite :: String -> IO ()
  blockfileWrite = flip withCString blockAsmWrite

  -- Look for certain quad combinations and turn them into more efficient versions
  optimizeQuad :: Quad -> Quad
  optimizeQuad (QuadQS _ (QuadQQS op src tf temp) dest) = 
    QuadQQS op src tf dest -- (=, (*, t1, t2, t3), -, a) -> (*, t1, t2, a)
  optimizeQuad (QuadQS _ (QuadSSS op src tf temp) dest) =
    QuadSSS op src tf dest -- (=, (*, X, Y, T1), -, A) -> (*, X, Y, A)
  optimizeQuad (QuadQS _ (QuadQSS op src tf temp) dest) =
    QuadQSS op src tf dest
  optimizeQuad quad = quad


  -- Write assembly for operators
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
    | op == "<" = do
      asmAction printer "mov" "ax" src False True
      asmAction printer "cmp" "ax" tf False True
      printer "jge "
    | op == ">" = do
      asmAction printer "mov" "ax" src False True
      asmAction printer "cmp" "ax" tf False True
      printer "jle "
    | op == ">=" = do
      asmAction printer "mov" "ax" src False True
      asmAction printer "cmp" "ax" tf False True
      printer "jl "
    | op == "<=" = do
      asmAction printer "mov" "ax" src False True
      asmAction printer "cmp" "ax" tf False True
      printer "jg "
    | op == "==" = do
      asmAction printer "mov" "ax" src False True
      asmAction printer "cmp" "ax" tf False True
      printer "jne "
    | op == "!=" = do
      asmAction printer "mov" "ax" src False True
      asmAction printer "cmp" "ax" tf False True
      printer "jeq "
    | op == "LOAD" = do
      asmAction printer "mov" "ax" tf False True -- Load offset
      asmAction printer "mov" "ebx" "2" False False -- doubleword is 2 bytes
      printer "mul bx\n"
      asmAction printer "mov" "ecx" src False False -- Load base address
      asmAction printer "add" "ecx" "eax" False False -- Offset the base
      asmAction printer "mov" "bx" "ecx" False True -- Indirect
      asmAction printer "mov" dest "bx" True False -- Store
    | op == "STORE" = do
      asmAction printer "mov" "ax" tf False True -- Load offset
      asmAction printer "mov" "ebx" "2" False False -- Mul 2
      printer "mul bx\n"
      asmAction printer "mov" "ecx" src False False -- Load base
      asmAction printer "add" "ecx" "eax" False False -- Offset base
      asmAction printer "mov" "ax" dest False True -- Load value
      asmAction printer "mov" "ecx" "ax" True False -- Store value

  operatorCase printer (Just op) (Just src) Nothing (Just dest)
    | op == "=" = do
      asmAction printer "mov" "ax" src False True
      asmAction printer "mov" dest "ax" True False
  operatorCase printer (Just op) (Just src) Nothing Nothing 
    | op == "ODD" = do
      asmAction printer "mov" "ax" src False True
      asmAction printer "test" "ax" "1" False False 
      printer "jnz "
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
  -- For operators/most reserved words 
  patternMatch :: Quad -> (String -> IO ()) -> IO ()
  patternMatch (QuadQQS op src tf dest) printer = do
    patternMatch src printer
    patternMatch tf printer
    op_str <- peekTname op
    src_str <- qname src
    tf_str <- qname tf
    dest_str <- peekSname dest
    operatorCase printer
                 (Just op_str) 
                 (Just src_str) 
                 (Just tf_str) 
                 (Just dest_str)
  patternMatch (QuadSSS op src tf dest) printer = do
    op_str <- peekTname op
    src_str <- peekSname src
    tf_str <- peekSname tf
    dest_str <- peekSname dest
    operatorCase printer (Just op_str) (Just src_str) (Just tf_str) (Just dest_str)
  patternMatch (QuadSQS op src tf dest) printer = do
    patternMatch tf printer
    op_str <- peekTname op
    src_str <- peekSname src
    tf_str <- qname tf
    dest_str <- peekSname dest
    operatorCase printer (Just op_str) (Just src_str) (Just tf_str) (Just dest_str)
  patternMatch (QuadQSS op src tf dest) printer = do
    patternMatch src printer
    op_str <- peekTname op
    src_str <- qname src
    tf_str <- peekSname tf
    dest_str <- peekSname dest
    operatorCase printer (Just op_str) (Just src_str) (Just tf_str) (Just dest_str)
  patternMatch (QuadSS op src dest) printer = do
    op_str <- peekTname op
    src_str <- peekSname src
    dest_str <- peekSname dest
    operatorCase printer (Just op_str) (Just src_str) Nothing (Just dest_str)
  patternMatch (QuadQS op src dest) printer = do
    patternMatch src printer
    op_str <- peekTname op
    src_str <- qname src
    dest_str <- peekSname dest
    operatorCase printer (Just op_str) (Just src_str) Nothing (Just dest_str)
  patternMatch (QuadIW op cond (QuadB block)) printer = do
    op_name <- peekTname op
    end_of_block <- peekSname block
    if op_name == "WHILE" then do -- NO FIXUP REQUIRED
      let start_of_block = "W" ++ drop 1 end_of_block
      printer (start_of_block ++ ":\n") -- label
      patternMatch cond printer         -- mov, cmp, jne/jeq/jg/jl/jle/jge 
      printer (end_of_block ++ "\n")    -- turns "jxx " into "jxx BX\n"
      patternMatch (QuadB block) printer
      printer ("jmp " ++ start_of_block ++ "\n" 
              ++ end_of_block ++ ": nop\n")
    else if op_name == "IF" then do -- No fixup either!!
      patternMatch cond printer
      printer (end_of_block ++ "\n")
      patternMatch (QuadB block) printer
      printer (end_of_block ++ ": nop\n")
    else return ()
  patternMatch (QuadIW op cond quad) printer = do
    op_name <- peekTname op
    block <- blockAdd >>= peek
    end_of_block <- peekSname block
    if op_name == "WHILE" then do
      let start_of_block = "W" ++ drop 1 end_of_block
      printer (start_of_block ++ ":\n")
      patternMatch cond printer
      printer (end_of_block ++ "\n")
      patternMatch quad printer
      printer ("jmp " ++ start_of_block ++ "\n" 
              ++ end_of_block ++ ": nop\n")
    else if op_name == "IF" then do
      patternMatch cond printer
      printer (end_of_block ++ "\n")
      patternMatch quad printer
      printer (end_of_block ++ ": nop\n")
    else return ()

  patternMatch (QuadS op src) printer = do
    op_str <- peekTname op
    src_str <- peekSname src
    operatorCase printer (Just op_str) (Just src_str) Nothing Nothing
  patternMatch (QuadQ op src) printer = do
    op_str <- peekTname op
    patternMatch src printer
    src_str <- qname src
    operatorCase printer (Just op_str) (Just src_str) Nothing Nothing
  patternMatch (QuadB block) printer = do
    block_name <- peekSname block
    withCString ("syntan_blocks_temp/" ++ block_name ++ ".qblock") asmFAppend
  patternMatch (QuadP xproc procedure (QuadB block)) printer = do
    proc_name <- peekSname procedure
    block_name <- peekSname block
    withCString ("./syntan_blocks_temp/" ++ proc_name ++ ".proc") blockfileOpen
    blockfileWrite $ proc_name ++ ":\n"
    withCString ("syntan_blocks_temp/" ++ block_name ++ ".qblock") asmFAppend
    blockfileWrite "ret\n"
    blockfileClose
    withCString proc_name procedureAdd
  patternMatch (QuadIdS src off dest) printer = do
    src_str <- peekSname src
    off_str <- peekSname off
    dest_str <- peekSname dest
    operatorCase printer (Just "LOAD") (Just src_str) (Just off_str) (Just dest_str)
  patternMatch (QuadIdQ src off dest) printer = do
    src_str <- peekSname src
    patternMatch off printer
    off_str <- qname off
    dest_str <- peekSname dest
    operatorCase printer (Just "LOAD") (Just src_str) (Just off_str) (Just dest_str)
  patternMatch (QuadSQ op src (QuadIdS arr off _)) printer = do
    src_str <- peekSname src
    arr_str <- peekSname arr
    off_str <- peekSname off
    operatorCase printer (Just "STORE") (Just arr_str) (Just off_str) (Just src_str)
  patternMatch (QuadSQ op src (QuadIdQ arr off _)) printer = do
    src_str <- peekSname src
    arr_str <- peekSname arr
    patternMatch off printer
    off_str <- qname off
    operatorCase printer (Just "STORE") (Just arr_str) (Just off_str) (Just src_str)
  patternMatch (QuadQQ op src (QuadIdS arr off _)) printer = do
    patternMatch src printer
    src_str <- qname src
    arr_str <- peekSname arr
    off_str <- peekSname off
    operatorCase printer (Just "STORE") (Just arr_str) (Just off_str) (Just src_str)
  patternMatch (QuadQQ op src (QuadIdQ arr off _)) printer = do
    patternMatch src printer
    src_str <- qname src
    arr_str <- peekSname arr
    patternMatch off printer
    off_str <- qname off
    operatorCase printer (Just "STORE") (Just arr_str) (Just off_str) (Just src_str)
  patternMatch any printer = do
    putStrLn "Writing a Quad (not really)"
    printer "; there's a quad here, I know it\n"

  quadLoop :: [TokenOrQuad] -> (String -> IO ()) -> IO ()
  quadLoop [Right quad] printer = do
    let optimized = optimizeQuad quad
    patternMatch optimized printer
    --patternMatch quad printer
    printer "\n"
  quadLoop (Right quad:rest) printer = do
    let optimized = optimizeQuad quad
    patternMatch optimized printer
    --patternMatch quad printer
    printer "\n"
    quadLoop rest printer
  
  generateASM :: [TokenOrQuad] -> Maybe Symbol -> IO ()
  generateASM quads Nothing = quadLoop quads mainWrite
  generateASM quads (Just block) = do
    block_name <- peekSname block
    withCString ("./syntan_blocks_temp/" ++ block_name ++ ".qblock") blockfileOpen
    quadLoop quads blockfileWrite
    blockfileClose

  asmAction :: (String -> IO ()) -> String -> String -> String -> Bool -> Bool -> IO ()
  asmAction printer action dest src idest isrc = do
    printer $ action ++ " " 

    if idest then 
      printer ("[" ++ dest ++ "], ") 
    else 
      printer $ dest ++ ", "

    if "INT" `isPrefixOf` src then
      printer $ drop 3 src  -- OPTIMIZATION: literal instead of memory
    else if isrc then
      printer ("[" ++ src ++ "]")
    else
      printer src

    printer "\n"


  qname :: Quad -> IO String
  qname (QuadQQS _ _ _ dest) = peekSname dest
  qname (QuadSSS _ _ _ dest) = peekSname dest
  qname (QuadSQS _ _ _ dest) = peekSname dest
  qname (QuadQSS _ _ _ dest) = peekSname dest
  qname (QuadSS _ _ dest) = peekSname dest
  qname (QuadQS _ _ dest) = peekSname dest
  qname (QuadIW op _ _) = peekTname op
  qname (QuadIdS _ _ dest) = peekSname dest
  qname (QuadB block) = peekSname block


  asmSetup :: [Symbol] -> IO ()
  asmSetup symbols = do
    dataHead
    writeSymbols symbols
    bss

  writeSymbols :: [Symbol] -> IO ()
  writeSymbols [] = mainWrite "\n"
  writeSymbols (symbol:rest) 
    |  fromIntegral (sym_class symbol) == 2 -- SVAR
    || fromIntegral (sym_class symbol) == 6 = do -- SCONST
      mainWrite "\n"
      asmWrite $ sname symbol
      mainWrite " DW "
      asmWrite $ value symbol
      writeSymbols rest
    | fromIntegral (sym_class symbol) == 8 = do -- SARR
      mainWrite "\n"
      asmWrite $ sname symbol
      mainWrite " TIMES "
      asmWrite $ value symbol
      mainWrite " DW 0"
      writeSymbols rest
    | otherwise = writeSymbols rest
  
  asmFinalize :: IO ()
  asmFinalize = do
    asmExit
    appendProcedures
    ioTail