-- CGets.hs
-- Wrappers for receiving data and cleaning up

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module CGets where
  import Foreign.C.Types
  import Foreign.C.String
  import Foreign.Ptr
  import Control.Applicative ((<$>), (<*>))
  import Foreign.Storable

  import TypeDeclarations

  foreign import capi "syntan/interface.h symbol_free" symFree :: Ptr Symbol -> IO ()
  foreign import capi "syntan/interface.h token_free" tokFree :: Ptr Token -> IO ()
  foreign import capi "syntan/interface.h next_symbol" nextSymbol :: IO (Ptr Symbol)
  foreign import capi "syntan/interface.h next_token" nextToken :: IO (Ptr Token)

  -- Take list of symbols from file and put them in a format I can use
  populateSymbolList :: [Ptr Symbol] -> IO [Ptr Symbol]
  populateSymbolList slist = do
    sym_ptr <- nextSymbol
    sym <- peek sym_ptr
    let cls = fromIntegral $ sym_class  sym
    if cls == 0 then do
      return slist
    else do populateSymbolList (slist ++ [sym_ptr])
  -- Do the same with tokens
  populateTokenList :: [Ptr Token] -> IO [Ptr Token]
  populateTokenList tlist = do
    tok_ptr <- nextToken
    tok <- peek tok_ptr
    let cls = fromIntegral $ tok_class tok
    if cls == 0 then do
      return tlist
    else do
      n <- peekCString (tname tok)
      populateTokenList (tlist ++ [tok_ptr])

  -- Cleanup each list
  symbolsFinalize :: [Ptr Symbol] -> IO ()
  symbolsFinalize []         = do
    putStrLn "Syntan: Finalized Symbol List"
  symbolsFinalize (sym_ptr:rest) = do
    symFree sym_ptr
    symbolsFinalize rest
  tokensFinalize :: [Ptr Token] -> IO ()
  tokensFinalize []         = do
    putStrLn "Syntan: Finalized Token List"
  tokensFinalize (tok_ptr:rest) = do
    tokFree tok_ptr
    tokensFinalize rest