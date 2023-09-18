-- Parser.hs
-- Syntax analysis automaton

-- Compiler macros to allow exporting and importing symbols
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-} -- Allows better imports with capi as opposed to ccall



module Syntan.Parser where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import Data.Either
import Data.List (elemIndex, find)
import Data.Tuple (swap)
import Data.Maybe (fromJust, isJust)
import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Text.Read (readMaybe)

import Syntan.PDA
import Syntan.Stack
import Syntan.TypeDeclarations
import Syntan.Helpers

foreign import capi "syntan/interface.h symbol_free" symFree :: Ptr Symbol -> IO ()
foreign import capi "syntan/interface.h token_free" tokFree :: Ptr Token -> IO ()
foreign import capi "syntan/interface.h next_symbol" nextSymbol :: IO (Ptr Symbol)
foreign import capi "syntan/interface.h next_token" nextToken :: IO (Ptr Token)
foreign import capi "syntan/interface.h temp_add" tempAdd :: IO (Ptr Symbol)

-- C extension helpers
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
-- Cleanup functions
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

                    -- The main focus of the program
                    --      ;,      =,    +/-,      (,      ),    *//,     IF,   THEN,    ODD,  relop,      {,      },   CALL,  WHILE,     DO,      ,,  CLASS,   VAR,    PROC,  CONST
precedence_matrix = [[  Error, Yields,  Error,  Error,  Error,  Error, Yields,  Error,  Error,  Error,  Error,  Takes, Yields, Yields,  Error,  Error, Yields, Yields, Yields, Yields ], -- ; (interaction w }?)
                     [  Takes, Yields, Yields, Yields,  Error, Yields,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error ], -- =
                     [  Takes,  Error,  Takes, Yields,  Takes, Yields,  Error,  Takes,  Error,  Takes,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error ], -- +/-
                     [  Error,  Error, Yields, Yields,  Equal, Yields,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- (
                     [  Takes,  Error,  Takes,  Error,  Takes,  Takes,  Error,  Takes,  Error,  Takes, Yields,  Takes,  Error,  Error,  Takes,  Takes,  Error,  Error,  Error,  Error ], -- ) (Interaction with {?)
                     [  Takes,  Error,  Takes, Yields,  Takes,  Takes,  Error,  Takes,  Error,  Takes,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error ], -- / / *
                     [  Error,  Error, Yields, Yields,  Error, Yields,  Error,  Equal, Yields, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- IF
                     [  Error, Yields, Yields,  Error,  Error, Yields, Yields,  Error,  Error,  Error, Yields,  Error, Yields, Yields,  Error,  Error,  Error, Yields,  Error, Yields ], -- THEN
                     [  Error,  Error,  Error, Yields,  Takes,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error ], -- ODD (maybe yield to +-*/?)
                     [  Error,  Error, Yields, Yields,  Takes, Yields,  Error,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error ], -- relop
                     [  Error, Yields,  Error,  Error,  Error,  Error, Yields,  Error,  Error,  Error, Yields,  Equal, Yields, Yields,  Error,  Error,  Error, Yields, Yields, Yields ], -- {
                     [  Error,  Takes,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Takes,  Takes,  Takes,  Error,  Error,  Takes,  Takes,  Takes,  Takes ], -- }
                     [  Error,  Error,  Error, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- CALL
                     [  Error,  Error, Yields, Yields,  Error, Yields,  Error,  Error, Yields, Yields, Yields,  Error,  Error,  Error,  Equal,  Error,  Error,  Error,  Error,  Error ], -- WHILE
                     [  Error, Yields,  Error,  Error,  Error,  Error, Yields,  Error,  Error,  Error, Yields,  Error, Yields, Yields,  Error,  Error,  Error, Yields,  Error, Yields ], -- DO
                     [  Takes,  Takes,  Takes,  Takes,  Takes,  Takes,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error ], -- ,
                     [  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- CLASS
                     [  Takes, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error ], -- VAR
                     [  Error,  Error,  Error, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- PROCEDURE
                     [  Takes, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error ]] -- CONST


-- FIXME: symbolFromToken looks up and pattern matches names before calling lookupSymbol
lookupSymbol :: String -> [Symbol] -> IO (Maybe Symbol)
lookupSymbol target [] = do return Nothing
lookupSymbol target (sym:rest) 
  | isDigitString target = do
    name <- peekCString $ sname sym
    if name == "INT" ++ target then do
      return $ Just sym
    else lookupSymbol target rest
  | otherwise            = do
    name <- peekCString $ sname sym
    if name == target then return $ Just sym
    else lookupSymbol target rest

-- Analyze a stack of quads/tokens to turn it into a real quad
-- Also placeholder for semantic analysis
-- TODO: test
toQuad :: Stack TokenOrQuad -> [Symbol] -> IO Quad
toQuad [Left dest, Left op, Left src] symbols
  | enumInt (tok_class op) == ASSIGN = do
    dest_tname <- peekCString $ tname dest
    m_dest_sym <- lookupSymbol dest_tname symbols -- TODO: error check
    let dest_sym = fromJust m_dest_sym
    src_tname <- peekCString $ tname src
    m_src_sym <- lookupSymbol src_tname symbols
    let src_sym = fromJust m_src_sym
    return $ QuadSS op src_sym dest_sym                -- =, src_sym, dest_sym
  | otherwise = do
    src_tname <- peekCString $ tname dest
    m_src_sym <- lookupSymbol src_tname symbols
    let src_sym = fromJust m_src_sym
    tf_tname <- peekCString $ tname src
    m_tf_sym <- lookupSymbol tf_tname symbols
    let tf_sym = fromJust m_tf_sym
    ptr_temp <- tempAdd 
    temp <- peek ptr_temp
    return $ QuadSSS op src_sym tf_sym temp            -- +, src_sym, tf_sym, temp
toQuad [Left dest, Left op, Right src] symbols
  | enumInt (tok_class op) == ASSIGN = do
    dest_tname <- peekCString $ tname dest
    m_dest_sym <- lookupSymbol dest_tname symbols
    let dest_sym = fromJust m_dest_sym
    return $ QuadQS op src dest_sym                    -- =, src, dest_sym
  | otherwise = do
    src_tname <- peekCString $ tname dest
    m_src_sym <- lookupSymbol src_tname symbols
    let src_sym = fromJust m_src_sym
    ptr_temp <- tempAdd 
    temp <- peek ptr_temp
    return $ QuadSQS op src_sym src temp               -- +, src_sym, src (actually tf), temp
toQuad [Right src, Left op, Left tf] symbols = do
  tf_tname <- peekCString $ tname tf
  m_tf_sym <- lookupSymbol tf_tname symbols
  let tf_sym = fromJust m_tf_sym
  ptr_temp <- tempAdd 
  temp <- peek ptr_temp
  return $ QuadQSS op src tf_sym temp                  -- +, src, tf_sym, temp
toQuad [Right src, Left op, Right tf] symbols = do
  ptr_temp <- tempAdd
  temp <- peek ptr_temp
  return $ QuadQQS op src tf temp                      -- +, src, tf, temp
toQuad [Left op, Left src] symbols = do
  src_tname <- peekCString $ tname src
  m_src_sym <- lookupSymbol src_tname symbols
  let src_sym = fromJust m_src_sym
  return $ QuadS op src_sym                            -- ODD, src
toQuad [Left op, Right src] symbols = return $ QuadQ op src
toQuad _ _ = return Invalid

-- Look for tail of handle and push finished quad to the new stack
generateQuad :: Stack TokenOrQuad -> Stack TokenOrQuad -> [Symbol] -> Integer -> IO (Stack TokenOrQuad)
generateQuad (tok_or_quad:rest) [] symbols counter = generateQuad rest [tok_or_quad] symbols $ counter + 1
generateQuad (tok_or_quad:rest) quad_stk symbols counter 
  | idx (top quad_stk) == 17 || idx (top quad_stk) == 19 = do
    putStrLn "case 1"
    return (tok_or_quad:rest) -- declaration statement
  | idx tok_or_quad == -1 && counter /= 3 = do
    putStrLn "case 2"
    generateQuad rest 
                                                         (push quad_stk tok_or_quad) 
                                                         symbols 
                                                         (counter + 1)
  | counter == 3 = do
    putStrLn "case 3"
    quad <- toQuad quad_stk symbols
    putStrLn "Found head, new stack:"
    printTokenOrQuad $ Right quad
    putStrLn ""
    printTokensAndQuads $ tok_or_quad:rest

    return $ push 
      (tok_or_quad:rest) 
      (Right quad)
  | otherwise = do
    putStrLn "case 4"
    let stk_idx = idx . fromJust $ filterTop (tok_or_quad:rest) isTerminal
    let quad_idx = maybe (-1) idx $ filterTop quad_stk isTerminal

    if quad_idx == -1 then
      generateQuad rest (push quad_stk tok_or_quad) symbols $ counter + 1
    else do
    putStrLn ""
    putStrLn "Pop a quad, current quad_stk:"
    printTokensAndQuads quad_stk
    putStrLn ""
    putStr "Checking row "
    printTokenOrQuad tok_or_quad
    putStr " against col "
    printTokenOrQuad . fromJust $ filterTop quad_stk isTerminal
    putStrLn ""

    let action = precedence_matrix!!fromIntegral stk_idx!!fromIntegral quad_idx
    
    -- If stack operator yields to quad operator, we found the head
    if action == Yields then do
      quad <- toQuad quad_stk symbols
      putStrLn "Found head, stack:"
      printTokenOrQuad $ Right quad
      putStrLn ""
      printTokensAndQuads $ tok_or_quad:rest

      return $ push 
       (tok_or_quad:rest) 
       (Right quad)
    else generateQuad rest (push quad_stk tok_or_quad) symbols $ counter + 1 -- Equal precedence


-- Lookup and push/pop logic
stateFunc :: Token -> Stack TokenOrQuad -> [Symbol] -> IO (Stack TokenOrQuad)
stateFunc tok stk symbols = do
  -- Index in table of current token
  let cur_idx = fromIntegral . idx $ Left tok

  -- Index in table of top stack operator
  let stk_idx = fromIntegral . maybe (-1) idx $ filterTop stk isTerminal

  if (stk_idx == 17 || stk_idx == 19) && cur_idx /= 0 -- Declaration statement
     then do 
    putStrLn "no push"
    putStrLn ""
    return stk
  else if cur_idx == -1 || stk_idx == -1 then do -- tok is nonterminal or there are no operators in stk
    putStrLn "unconditional push"
    putStrLn ""
    return $ push stk $ Left tok
  else do
    print $ fromIndex stk_idx
    print $ fromIndex cur_idx
    
    let action = precedence_matrix!!fromIntegral stk_idx!!fromIntegral cur_idx

    if action == Yields || action == Equal then do
      putStrLn "conditioned push"
      putStrLn ""
      return (push stk (Left tok))
    else if action == Takes then do
      putStrLn "Starting quad gen"
      generateQuad stk [] symbols 0 -- TODO: semi collapse?
    else do
      putStrLn "tinvalid"
      return $ push stk (Left $ Token { tname = nullPtr, tok_class = intEnum TINVALID }) -- to be caught by F


-- TODO: weirdness
checkEnd :: Stack TokenOrQuad -> (Bool, Stack TokenOrQuad)
checkEnd (Left tok:rest) | enumInt (tok_class tok) == TINVALID = (True, Left tok:rest)
                         | otherwise = (False, Left tok:rest)
checkEnd any = (False, any) 
-- checkEnd [] = (False, [])
-- checkEnd [tok_or_quad] = (False, [tok_or_quad])
-- checkEnd (Left tok:rest) | enumInt (tok_class tok) == TINVALID = (True, Left tok:rest)
--                          | tok_class tok == intEnum SEMI || 
--                            tok_class tok == intEnum RB = (True, Left tok:rest)
-- checkEnd (Right quad:rest) = (False, Right quad:rest)
-- checkEnd (t:y:rest) = (False, t:y:rest)

-- The bees have returned, and not in the good way that logically deletes them
-- They've ingrained themselves in the head node of the linked stack,
--  and taken over the info field
-- Send beekeepers and debuggers
pushDown :: P_D_Automaton symbol token stack_el -> [symbol] -> (token -> IO ()) -> (Stack stack_el -> IO ()) -> [token] -> IO (Bool, Stack stack_el)
pushDown (delta, stk, f) symbols printer lprinter (tok:rest) = do
  printer tok
  putStrLn ""
  new_stack <- delta tok stk symbols -- Perform precedence checking and quad generation
  --lprinter new_stack
  let (end, err_stack) = f new_stack
  
  if end then return (end, err_stack)
  else do pushDown (delta, new_stack, f) symbols printer lprinter rest -- Recurse

runParser :: IO ()
runParser = do
  -- Populate lists
  symbol_list <- populateSymbolList []
  token_list <- populateTokenList []
  putStrLn "Syntan: Lexeme and Symbol Import Success"
  tokens <- mapM peek token_list
  symbols <- mapM peek symbol_list
  
  -- Pass and begin parse
  semi_str <- withCString ";" $ \ x -> do -- Stack-memory C string
    let semi_tok = Token { tname = x, tok_class = intEnum SEMI }
    pushDown (stateFunc, [Left semi_tok], checkEnd) symbols printToken printTokensAndQuads tokens
  
  -- Clean up environment (lots of memory was thrown around during imports)
  -- NOTE clean up signal token symbol and quad
  symbolsFinalize symbol_list
  tokensFinalize token_list
  
foreign export ccall runParser :: IO () -- Generate prototypes to call from C