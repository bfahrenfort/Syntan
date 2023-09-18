-- Parser.hs
-- Syntax analysis automaton

-- Compiler macros to allow exporting and importing symbols
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-} -- Allows better imports with capi as opposed to ccall



module Parser where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import Data.Either
import Data.List (elemIndex, find)
import Data.Tuple (swap)
import Data.Maybe (fromJust, isJust)
import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Control.Applicative ((<$>), (<*>))
import Foreign.Storable

import PDA
import Stack
import TypeDeclarations
import Helpers
import CGets
import Assembly
import Helpers (symbolFromToken)

foreign import capi "syntan/interface.h temp_add" tempAdd :: IO (Ptr Symbol)
foreign import capi "syntan/interface.h block_add" blockAdd :: IO (Ptr Symbol)

-- The main focus of the program
-- It should be noted that I use 'Skip' to denote a guaranteed collapse
-- Semicolons are worthless and thus are thrown out
                    --      ;,      =,    +/-,      (,      ),    *//,     IF,   THEN,    ODD,  relop,      {,      },   CALL,  WHILE,     DO,      ,,  CLASS,   VAR,    PROC,  CONST
precedence_matrix = [[   Skip, Yields,  Error,  Error,  Error,  Error, Yields,  Error,  Error,  Error,  Error,  Takes, Yields, Yields,  Error,  Error, Yields, Yields, Yields, Yields ], -- ; (interaction w }?)
                     [  Takes, Yields, Yields, Yields,  Error, Yields,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error ], -- =
                     [  Takes,  Error,  Takes, Yields,  Takes, Yields,  Error,  Takes,  Error,  Takes,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error ], -- +/-
                     [  Error,  Error, Yields, Yields,  Equal, Yields,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- (
                     [  Takes,  Error,  Takes,  Error,  Takes,  Takes,  Error,  Takes,  Error,  Takes, Yields,  Takes,  Error,  Error,  Takes,  Takes,  Error,  Error,  Takes,  Error ], -- ) (Interaction with {?)
                     [  Takes,  Error,  Takes, Yields,  Takes,  Takes,  Error,  Takes,  Error,  Takes,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error ], -- / / *
                     [  Error,  Error, Yields, Yields,  Error, Yields,  Error,  Equal, Yields, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- IF
                     [  Takes, Yields, Yields,  Error,  Error, Yields, Yields,  Error,  Error,  Error, Yields,  Takes, Yields, Yields,  Error,  Error,  Error, Yields,  Error, Yields ], -- THEN
                     [  Error,  Error,  Error, Yields,  Takes,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error ], -- ODD (maybe yield to +-*/?)
                     [  Error,  Error, Yields, Yields,  Takes, Yields,  Error,  Takes,  Error,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error,  Error ], -- relop
                     [   Skip, Yields,  Error,  Error,  Error,  Error, Yields,  Error,  Error,  Error, Yields,  Equal, Yields, Yields,  Error,  Error,  Error, Yields, Yields, Yields ], -- {
                     [  Error,  Takes,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Takes,  Takes,  Takes,  Error,  Error,  Takes,  Takes,  Takes,  Takes ], -- }
                     [  Error,  Error,  Error, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- CALL
                     [  Error,  Error, Yields, Yields,  Error, Yields,  Error,  Error, Yields, Yields,  Error,  Error,  Error,  Error,  Equal,  Error,  Error,  Error,  Error,  Error ], -- WHILE
                     [  Takes, Yields, Yields,  Error,  Error, Yields, Yields,  Error,  Error,  Error, Yields,  Takes, Yields, Yields,  Error,  Error,  Error, Yields,  Error, Yields ], -- DO
                     [  Takes,  Takes,  Takes,  Takes,  Takes,  Takes,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error ], -- ,
                     [  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- CLASS
                     [  Takes, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error ], -- VAR
                     [  Error,  Error,  Error,  Equal,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error ], -- PROCEDURE
                     [  Takes, Yields,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Takes,  Error,  Error,  Error,  Error ]] -- CONST
  
-- Analyze a stack of quads/tokens to turn it into a real quad
toQuad :: Stack TokenOrQuad -> [Symbol] -> IO Quad
toQuad [Left dest, Left op, Left src] symbols
  | tok_class op == intEnum ASSIGN = do
    dest_sym <- symbolFromToken dest symbols
    src_sym <- symbolFromToken src symbols
    return $ QuadSS op src_sym dest_sym                -- =, src_sym, -, dest_sym
  | otherwise = do
    src_sym <- symbolFromToken dest symbols
    tf_sym <- symbolFromToken src symbols
    temp <- tempAdd >>= peek
    return $ QuadSSS op src_sym tf_sym temp            -- +, src_sym, tf_sym, temp
toQuad [Left dest, Left op, Right src] symbols
  | tok_class op == intEnum ASSIGN = do
    dest_sym <- symbolFromToken dest symbols
    return $ QuadQS op src dest_sym                    -- =, src, -, dest_sym,
  | otherwise = do
    src_sym <- symbolFromToken dest symbols
    temp <- tempAdd >>= peek
    return $ QuadSQS op src_sym src temp               -- +, src_sym, src (actually tf), temp
toQuad [Right src, Left op, Left tf] symbols = do
  tf_sym <- symbolFromToken tf symbols
  temp <- tempAdd >>= peek
  return $ QuadQSS op src tf_sym temp                  -- +, src, tf_sym, temp
toQuad [Right src, Left op, Right tf] symbols = do
  temp <- tempAdd >>= peek
  return $ QuadQQS op src tf temp                      -- +, src, tf, temp
toQuad [Left op, Left src] symbols = do
  src_sym <- symbolFromToken src symbols
  temp <- tempAdd >>= peek
  return $ QuadSS op src_sym temp                      -- ODD, src, -, temp
toQuad [Left op, Right src] symbols = do
  temp <- tempAdd >>= peek
  return $ QuadQS op src temp                          -- ODD, src, -, temp
toQuad [Left iwop, Right cond, Left thenop, Right stmt] symbols = do
  return $ QuadIW iwop cond stmt
-- TODO: block pattern match, generateASM for all quads inside
toQuad (Block (Left lb) quads (Left rb)) symbols |  tok_class lb == intEnum LB
                                                 && tok_class rb == intEnum RB = do
  generateASM quads
  block <- blockAdd >>= peek
  return $ QuadB block
toQuad [Left xproc, Left tproc, Left lp, Left rp, Right block] symbols = do
  proc_sym <- symbolFromToken tproc symbols
  return $ QuadP xproc proc_sym block
toQuad _ _ = return Invalid

-- Look for tail of handle and push finished quad to the new stack
generateQuad :: Stack TokenOrQuad -> Stack TokenOrQuad -> [Symbol] -> Integer -> IO (Stack TokenOrQuad)
generateQuad stk@(tok_or_quad:rest) [] symbols counter = do
  putStr "Empty quad stack "
  printTokenOrQuad tok_or_quad
  putStrLn ""
  generateQuad rest [tok_or_quad] symbols $ counter + 1
generateQuad stk@(tok_or_quad:rest) quad_stk symbols counter 
  | idx tok_or_quad == toIndex SEMI = do
    putStrLn "skip semi"
    generateQuad rest quad_stk symbols counter -- semi collapse
  | idx (top quad_stk) == 17 || idx (top quad_stk) == 19 = do
    putStr "case 1 "
    printTokenOrQuad tok_or_quad
    putStrLn ""
    return stk -- declaration statement
  | idx tok_or_quad < 0 && counter < 3 = do
    putStr "case 2 "
    printTokenOrQuad tok_or_quad
    putStrLn ""
    generateQuad rest 
                                                         (push quad_stk tok_or_quad) 
                                                         symbols 
                                                         (counter + 1)
  | otherwise = do
    putStr "case 4 "
    printTokenOrQuad tok_or_quad
    putStrLn ""
    let top_term = fromJust $ filterTop stk isTerminal
    let stk_idx = idx top_term
    let quad_idx = maybe (-1) idx $ filterTop quad_stk isTerminal

    if quad_idx == -1 then do
      generateQuad rest (push quad_stk tok_or_quad) symbols $ counter + 1
    else do
      putStrLn ""
      putStrLn "Current quad_stk:"
      printTokensAndQuads quad_stk
      putStr "Checking row "
      printTokenOrQuad top_term
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
-- TODO: when PROCEDURE pushed, add to stack to assign to next block
stateFunc :: Token -> Stack TokenOrQuad -> [Symbol] -> IO (Bool, Stack TokenOrQuad)
stateFunc tok [Left start] symbols = return (False, [Left tok, Left start]) -- Only a semi in the stack
stateFunc tok stk symbols = do
  -- Index in table of current token
  let cur_idx = fromIntegral . idx $ Left tok

  -- Index in table of top stack operator
  let stk_idx = fromIntegral . maybe (-1) idx $ filterTop stk isTerminal

  if (stk_idx == 17 || stk_idx == 19) && cur_idx /= 0 -- Declaration statement
     then do 
    putStrLn "no push"
    putStrLn ""
    return (False, stk)
  else if cur_idx < 0 || stk_idx < 0 then do -- tok is nonterminal or there are no operators in stk
    putStrLn "unconditional push"
    putStrLn ""
    return (False, push stk $ Left tok)
  else do
    print $ fromIndex stk_idx
    print $ fromIndex cur_idx
    
    let action = precedence_matrix!!fromIntegral stk_idx!!fromIntegral cur_idx

    if action == Yields || action == Equal then do
      -- TODO: fixup
      putStrLn "conditioned push"
      putStrLn ""
      return (False, push stk (Left tok))
    else if action == Takes then do
      putStrLn "Starting quad gen"
      new_stk <- generateQuad stk [] symbols 0
      if   tok_class tok == intEnum SEMI
        || tok_class tok == intEnum DO
        || tok_class tok == intEnum THEN
        || tok_class tok == intEnum XPROC
        || tok_class tok == intEnum RP 
        || tok_class tok == intEnum RB then do -- Closing/collapsible token
        putStrLn "Retry closer tok "
        return (True, new_stk)
      else return (False, new_stk)
    else if action == Skip then do
      putStrLn "Skippin"
      return (False, stk)
    else do
      putStrLn "tinvalid"
      return (False, push stk (Left $ Token { tname = nullPtr, tok_class = intEnum TINVALID })) -- to be caught by F


-- Check if stack contains a valid program or errored
checkEnd :: Stack TokenOrQuad -> (Bool, Stack TokenOrQuad)
checkEnd (Left tok:rest) | tok_class tok == intEnum TINVALID = (True, Left tok:rest)
                         | otherwise = (False, Left tok:rest)
checkEnd (Right Invalid:rest) = (True, Right Invalid:rest)
checkEnd any = (False, any) 

-- The bees have returned, and not in the good way that logically deletes them
-- They've ingrained themselves in the head node of the linked stack,
--  and taken over the info field
-- Send beekeepers and debuggers
pushDown :: P_D_Automaton symbol token stack_el -> [symbol] -> (token -> IO ()) -> [token] -> IO (Bool, Stack stack_el)
pushDown (delta, stk, f) symbols printer tokens@(tok:rest) = do
  printer tok
  putStrLn ""

  (retry, new_stack) <- delta tok stk symbols 
  let (end, err_stack) = f new_stack

  if end then return (end, err_stack)
  else if retry then pushDown (delta, new_stack, f) symbols printer tokens
  else pushDown (delta, new_stack, f) symbols printer rest

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
    pushDown (stateFunc, [Left semi_tok], checkEnd) symbols printToken tokens
  
  -- Clean up environment (lots of memory was thrown around during imports)
  -- NOTE clean up signal token symbol and quad
  symbolsFinalize symbol_list
  tokensFinalize token_list
  
foreign export ccall runParser :: IO () -- Generate prototypes to call from C