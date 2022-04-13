-- Avengers, assemble!
-- Assembly.hs

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Assembly where
  import Foreign.C.Types
  import Foreign.C.String
  import Foreign.Ptr

  import TypeDeclarations

  -- TODO: C interface for writing to assembly file
  --foreign import capi "syntan/interface.h asm_write" asmWrite :: CString -> IO ()
  
  -- Look for certain quad combinations and turn them into more efficient versions
  optimizeQuad :: Quad -> Quad
  optimizeQuad (QuadQS _ (QuadQQS op src tf temp) dest) = 
    QuadQQS op src tf dest -- (=, (*, t1, t2, t3), -, a) -> (*, t1, t2, a)
  optimizeQuad quad = quad

  -- Write the assembly for each type of quad
  patternMatchWrite :: Quad -> IO ()
  patternMatchWrite quad = putStrLn "Writing a quad (not really)"

  -- TODO: Program pattern match
  generateASM :: [TokenOrQuad] -> IO ()
  generateASM [Right quad] = do
    let optimized = optimizeQuad quad
    patternMatchWrite optimized
  generateASM (Right quad:rest) = do
    let optimized = optimizeQuad quad
    patternMatchWrite optimized

    generateASM rest

