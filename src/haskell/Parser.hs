-- Parser.hs
-- Syntax analysis automaton
{-- 
  Name: Brandon Fahrenfort
  Option: A
  Featues: Compound statements, 
           nested IF/WHILE, 
           PROCEDURE calls, 
           subscription with [],
           error checking.
  Notes:
    - Uses a slightly modified approach on assembly gen:
     * When popping to generate a quad, it does not immediately generate the assembly.
     * Instead, it waits until a list of quads are popped enclosed in { }, 
     *  called a 'Block' pattern in my code (See the pattern synonyms in Helpers.hs).
     * Then, it generates the assembly (optimizing as needed), writes it to a .qblock file,
     *  and discards the quads, pushing a QuadB (see TypeDeclarations.hs) with the name
     *  of the file to the stack.
     * When it pops the enclosing structure, it writes the preliminaries (a label
     *  and a compare for WHILE, just a compare for IF, a label for PROCEDURE...),
     *  appends the block file of the same name, and writes the closing ('jmp',
     *  'ret', end stack labels etc). This means three things:
     *   * NO FIXUP STACKS REQUIRED. Pretty cool, right.
     *   * Better optimization potential. I have access to all quads in a block
     *      instead of just one - That opens up many more possibilities.
     *   * Effortless nesting. A block can contain other control structures,
     *      which have their own blocks, which have their own control structures
     *     with blocks, etc.
     * What I'm saying is that I'm very proud of this design.
     * The one downside is that the nested block naming is a bit inconsistent inside 
        the assembly file (you might have b17 inside of b3 instead of b2 inside b3),
        but who's looking at that?
--}
-- Requirements for A option:
-- Error checking
-- Make wrapper that makes the temp dir

-- Allow exporting and importing symbols
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.List (elemIndex, find)
import Data.Maybe (fromJust)
import Foreign.Storable
import Control.Monad (when)

import PDA
import Stack
import TypeDeclarations
import Helpers
import CGets
import Assembly
import ErrorCheck

-- The main focus of the program
-- It should be noted that I use 'Skip' to denote when to collapse
-- Semicolons are worthless and thus are thrown out, etc
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

-- Error checking
printErrorMessage :: Stack TokenOrQuad -> IO ()
printErrorMessage (Right (Invalid lst):_) = do
  putStr "No pattern match for the expression: \n\t"
  printFailedQuad lst 0
printErrorMessage err_stack = do
  putStr "In expression: \n\t"
  printExpression err_stack
  putStrLn "\ESC[0m"
  putStr "Expected: \n\t"
  let top_idx = maybe (-1) idx $ filterTop err_stack isTerminal
  when (top_idx == toIndex ASSIGN
     || top_idx == toIndex ADDOP
     || top_idx == toIndex MOP
     || top_idx == toIndex RELOP)
    $ putStr "identifier or literal followed by:\n\t"
  if top_idx >= 0 then printExpected (precedence_matrix!!fromIntegral top_idx) 0
  else putStr "(something's not right)"
  putStrLn ""
  putStr "Found: \n\t\ESC[31m"
  printTokenOrQuad $ top err_stack
  putStrLn "\ESC[0m"

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
      -- ...if there are any bugs in this entire program they're in these 25 lines
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
        return (False, push stk (Left $ Token { tname = (tname tok), tok_class = intEnum TINVALID }), symbols) -- to be caught by F


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
checkEnd stk@(Right (Invalid _):rest) = (True, False, stk)
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

runParser :: IO CInt
runParser = do
  -- Populate lists
  symbol_list <- populateSymbolList []
  token_list <- populateTokenList []
  putStrLn "Syntan: Lexeme and Symbol Import Success"
  tokens <- mapM peek token_list
  symbols <- mapM peek symbol_list
  
  -- Pass and begin parse
  retval <- withCString ";" $ \ x -> do -- Stack-memory C string
    let semi_tok = Token { tname = x, tok_class = intEnum SEMI } -- To start the stack
    (_, valid, err_stack, new_symbols) <- pushDown (stateFunc, [Left semi_tok], checkEnd) symbols printToken tokens
    if valid then do
      putStrLn "Syntan: Parse Success"
      putStrLn "Syntan: Starting Main Assembly Generation"
      asmSetup new_symbols
      generateASM err_stack Nothing
      return $ fromIntegral 0
    else do 
      putStrLn "\ESC[31mSyntan: Parse Fail\ESC[0m"
      printErrorMessage err_stack
      return $ fromIntegral 1
  
  -- Clean up environment (lots of memory was thrown around during imports)
  symbolsFinalize symbol_list
  tokensFinalize token_list

  -- Add end of file
  asmFinalize

  return retval
  
foreign export ccall runParser :: IO CInt -- Generate prototypes to call from Cghc -c -O src/haskell/** -outputdir tmp -stubdir include/stubs -Iinclude -itmp