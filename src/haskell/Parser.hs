-- Parser.hs
-- Syntax analysis automaton
-- Requirements for A option:
-- Writing blocks to temp file (bool flag passed to generateASM)
-- IF assembly gen and block appending
-- PROCEDURE assembly gen and block appending
-- [] assembly gen modifications (load+store)
-- Error checking
-- Make wrapper that makes the temp dir

-- Allow exporting and importing symbols
{-# LANGUAGE ForeignFunctionInterface #-}

module Parser where

import Foreign.Ptr
import Foreign.C.String
import Data.List (elemIndex, find)
import Data.Maybe (fromJust)
import Foreign.Storable

import PDA
import Stack
import TypeDeclarations
import Helpers
import CGets
import Assembly

-- The main focus of the program
-- It should be noted that I use 'Skip' to denote when to collapse
-- Semicolons are worthless and thus are thrown out
                    --      ;,      =,    +/-,      (,      ),    *//,     IF,   THEN,    ODD,  relop,      {,      },   CALL,  WHILE,     DO,      ,,  CLASS,   VAR,    PROC,  CONST,  PRINT,    GET,   XARR,     LS,     RS
precedence_matrix = [[   Skip, Yields,  Error,  Error,  Error,  Error, Yields,  Error,  Error,  Error,  Error,  Takes, Yields, Yields,  Error,  Error, Yields, Yields, Yields, Yields, Yields, Yields, Yields, Yields,  Error ], -- ;
                     [  Takes,  Error, Yields, Yields,  Error, Yields,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error ], -- =
                     [  Takes,  Error,  Takes, Yields,  Takes, Yields,  Error,  Takes,  Error,  Takes,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Takes ], -- +/-
                     [  Error,  Error, Yields, Yields,  Equal, Yields,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error ], -- (
                     [  Takes,  Error,  Takes,  Takes,  Takes,  Takes,  Error,  Takes,  Error,  Takes, Yields,  Takes,  Error,  Error,  Takes,  Takes,  Error,  Error,  Takes,  Error, Yields, Yields,  Error,  Error,  Takes ], -- ) (Interaction with {?)
                     [  Takes,  Error,  Takes, Yields,  Takes,  Takes,  Error,  Takes,  Error,  Takes,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Takes ], -- / / *
                     [  Error,  Error, Yields, Yields,  Error, Yields,  Error,  Equal, Yields, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error ], -- IF
                     [  Takes, Yields, Yields,  Error,  Error, Yields, Yields,  Error,  Error,  Error, Yields,  Takes, Yields, Yields,  Error,  Error,  Error, Yields,  Error, Yields, Yields, Yields, Yields, Yields,  Error ], -- THEN
                     [  Error,  Error,  Error, Yields,  Takes,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error ], -- ODD (maybe yield to +-*/?)
                     [  Error,  Error, Yields, Yields,  Takes, Yields,  Error,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error ], -- < <= > >= == !=
                     [   Skip, Yields,  Error,  Error,  Error,  Error, Yields,  Error,  Error,  Error, Yields,  Equal, Yields, Yields,  Error,  Error,  Error, Yields, Yields, Yields, Yields, Yields, Yields, Yields,  Error ], -- {
                     [  Error,  Takes,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Takes,  Takes,  Takes,  Error,  Error,  Takes,  Takes,  Takes,  Takes,  Takes,  Takes,  Takes,  Takes,  Error ], -- }
                     [  Error,  Error,  Error,  Equal,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- CALL
                     [  Error,  Error, Yields, Yields,  Error, Yields,  Error,  Error, Yields, Yields,  Error,  Error,  Error,  Error,  Equal,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error ], -- WHILE
                     [  Takes, Yields, Yields,  Error,  Error, Yields, Yields,  Error,  Error,  Error, Yields,  Takes, Yields, Yields,  Error,  Error,  Error, Yields,  Error, Yields, Yields, Yields, Yields, Yields,  Error ], -- DO
                     [  Takes,  Takes,  Takes,  Takes,  Takes,  Takes,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- ,
                     [  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- CLASS
                     [  Takes, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- VAR
                     [  Error,  Error,  Error,  Equal,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- PROCEDURE
                     [  Takes, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- CONST
                     [  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error ], -- PRINT
                     [  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error ], -- GET
                     [  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error ], -- ARRAY
                     [  Error,  Error, Yields, Yields,  Error, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Equal ], -- [
                     [  Takes,  Takes,  Takes,  Error,  Takes,  Takes,  Error,  Takes,  Error,  Takes,  Error,  Takes,  Error,  Error,  Takes,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Takes,  Takes ]] -- ]


  -- Pop logic, Look for tail of handle and generate quad
generateQuad :: Stack TokenOrQuad -> Stack TokenOrQuad -> [Symbol] -> Integer -> IO (Stack TokenOrQuad, [Symbol])
generateQuad stk@(tok_or_quad:rest) quad_stk symbols counter 
  |  idx (top (push quad_stk tok_or_quad)) == toIndex XVAR 
  || idx (top (push quad_stk tok_or_quad)) == toIndex XCONST
  || idx (top (push quad_stk tok_or_quad)) == toIndex XARR = do
    putStr "case declaration "
    printTokenOrQuad tok_or_quad
    putStrLn ""
    return (rest, symbols) -- declaration statement
  | otherwise = do
    putStr "case comparison "
    printTokenOrQuad tok_or_quad
    putStrLn ""
    let new_quad = push quad_stk tok_or_quad

    let top_term = fromJust $ filterTop rest isTerminal
    let stk_idx = idx top_term
    let top_of_quad = top new_quad
    let quad_idx = idx top_of_quad

    if idx tok_or_quad < 0 then do
      putStrLn "Add the above to quad_stk"
      generateQuad rest new_quad symbols $ counter + 1
    else do
      putStrLn ""
      putStrLn "Current quad_stk:"
      printTokensAndQuads new_quad
      putStr "Checking row "
      printTokenOrQuad top_term
      putStr " against col "
      printTokenOrQuad top_of_quad
      putStrLn ""

      let action = precedence_matrix!!fromIntegral stk_idx!!fromIntegral quad_idx
      
      -- If stack operator yields to quad operator, we found the head
      -- FIXME: if there are any bugs in this entire program they're in these 25 lines
      if action == Yields then do
        if idx tok_or_quad == toIndex ODD 
          || idx tok_or_quad == toIndex IF 
          || idx tok_or_quad == toIndex WHILE
          || idx tok_or_quad == toIndex LB 
          || idx tok_or_quad == toIndex LP
          || idx tok_or_quad == toIndex XPROC 
          || idx tok_or_quad == toIndex CALL
          || idx tok_or_quad == toIndex PRINT
          || idx tok_or_quad == toIndex GET then do -- Rules that start with a terminal
          (quad, new_symbols) <- toQuad new_quad symbols
          putStrLn "Found head, stack:"
          printTokenOrQuad $ Right quad
          putStrLn ""
          printTokensAndQuads rest
          return (push rest (Right quad), new_symbols)
        else do -- Rules that start with a nonterminal
          (quad, new_symbols) <- toQuad (top rest:new_quad) symbols
          putStrLn "Found head, stack:"
          printTokenOrQuad $ Right quad
          putStrLn ""
          printTokensAndQuads $ pop rest
          return (push (pop rest) (Right quad), new_symbols)        
      else generateQuad rest new_quad symbols $ counter + 1 -- Equal precedence

-- Lookup and push
-- TODO: when PROCEDURE pushed, add to stack to assign to next block
-- TODO: fixups
stateFunc :: [Token] -> Stack TokenOrQuad -> [Symbol] -> IO (Bool, Stack TokenOrQuad, [Symbol])
stateFunc (tok:_) [Left start] symbols = return (False, [Left tok, Left start], symbols) -- Only a semi in the stack
stateFunc tokens@(tok:_) stk symbols = do
  -- Index in table of current token
  --let cur_idx = fromIntegral . idx . Left . fromJust $ filterTop tokens isTerminalToken
  let cur_idx = idx $ Left tok

  -- Index in table of top stack operator
  let stk_idx = fromIntegral . maybe (-1) idx $ filterTop stk isTerminal

  if stk_idx == toIndex RB || isBlock (top stk) then do
    putStrLn "Unconditional pop"
    (new_stk, new_symbols) <- generateQuad stk [] symbols 0
    return (True, new_stk, new_symbols)
  else do

    if (stk_idx == toIndex XVAR || stk_idx == toIndex XCONST || stk_idx == toIndex XARR) && cur_idx /= 0 -- Declaration statement
      then do 
      putStrLn "no push"
      putStrLn ""
      return (False, stk, symbols)
    else if cur_idx < 0 || stk_idx < 0 then do -- tok is nonterminal or there are no operators in stk
      putStrLn "unconditional push"
      putStrLn ""
      return (False, push stk $ Left tok, symbols)
    else do
      print $ fromIndex stk_idx
      print $ fromIndex cur_idx
      
      let action = precedence_matrix!!fromIntegral stk_idx!!fromIntegral cur_idx

      if action == Yields || action == Equal then do
        -- TODO: fixup
        putStrLn "conditioned push"
        putStrLn ""
        return (False, push stk (Left tok), symbols)
      else if action == Takes then do
        putStrLn "Starting quad gen"
        (new_stk, new_symbols) <- generateQuad stk [] symbols 0
        if tok_class tok /= intEnum XVAR
          && tok_class tok /= intEnum XCONST
          && tok_class tok /= intEnum XARR then do -- Skip assignment statements
          putStrLn "Retry closer tok "
          return (True, new_stk, new_symbols)
        else return (False, new_stk, new_symbols)
      else if action == Skip then do
        putStrLn "Skippin"
        putStrLn ""
        return (False, stk, symbols)
      else do
        putStrLn "tinvalid"
        return (False, push stk (Left $ Token { tname = nullPtr, tok_class = intEnum TINVALID }), symbols) -- to be caught by F


-- Check if stack contains a valid program or errored
checkEnd :: Stack TokenOrQuad -> (Bool, Bool, Stack TokenOrQuad)
checkEnd (Program (Left semi) (Left xclass) (Left cname) (Left lb) quads (Left rb))
  |  tok_class rb     == intEnum RB
  && tok_class lb     == intEnum LB
  && tok_class cname  == intEnum IDENT
  && tok_class xclass == intEnum XCLASS
  && tok_class semi   == intEnum SEMI = (True, True, quads) -- Valid program
checkEnd stk@(Left tok:rest) | tok_class tok == intEnum TINVALID = (True, False, stk)
                             | otherwise = (False, True, stk)
checkEnd stk@(Right Invalid:rest) = (True, False, stk)
checkEnd any = (False, True, any) 

-- The bees have returned, and I don't mean they popped themselves from the call stack
-- They've ingrained themselves in the head node of the linked stack,
--  and taken over the info field
-- Computer completely filled with bees
-- Send beekeepers and debuggers
pushDown :: P_D_Automaton symbol token stack_el -> [symbol] -> (token -> IO ()) -> [token] -> IO (Bool, Bool, Stack stack_el, [symbol])
pushDown (_, stk, f) symbols _ [] = do
  let (end, valid, err_stack) = f $ reverse stk
  return (end, valid, err_stack, symbols)
pushDown (delta, stk, f) symbols printer tokens@(tok:rest) = do
  printer tok
  putStrLn ""

  (retry, new_stack, new_symbols) <- delta tokens stk symbols 
  let (end, valid, err_stack) = f new_stack

  if end then return (end, valid, err_stack, new_symbols)
  else if retry then pushDown (delta, new_stack, f) new_symbols printer tokens
  else pushDown (delta, new_stack, f) new_symbols printer rest

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
    (_, valid, err_stack, new_symbols) <- pushDown (stateFunc, [Left semi_tok], checkEnd) symbols printToken tokens
    if valid then do
      putStrLn "Syntan: Parse Success"
      putStrLn "Syntan: Starting Main Assembly Generation"
      printSymbols new_symbols
      asmSetup new_symbols
      generateASM err_stack Nothing
      
    else putStrLn "Syntan: Parse Fail"
  
  -- Clean up environment (lots of memory was thrown around during imports)
  symbolsFinalize symbol_list
  tokensFinalize token_list

  -- Add end of file
  asmFinalize
  
foreign export ccall runParser :: IO () -- Generate prototypes to call from C