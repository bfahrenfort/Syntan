module TypeDeclarations where

  import Foreign.C.Types
  import Foreign.Ptr
  import Foreign.C.String
  import Data.Either
  import Control.Applicative ((<$>), (<*>))
  import Foreign.Storable
  import Data.Tuple (swap)
  import Data.Maybe (fromJust)

  -- Mirror C types
  data Symbol = Symbol
              { sname     :: CString -- 8 byte
              , sym_class :: CInt -- 4 byte
              , value     :: CString
              , address   :: CInt
              , segment   :: CInt }
  data Token = Token
              { tname     :: CString
              , tok_class :: CInt }
              
  -- Explicitly tell Haskell how to store and retrieve the above types
  instance Storable Symbol where
    alignment _ = 8 -- LCM of the sizes of the struct elements
    sizeOf _    = 32 -- Sum of sizes of struct elementss
    peek ptr    = Symbol
      <$> peekByteOff ptr 0 -- Offset of first element
      <*> peekByteOff ptr 8 -- Offset of second element
      <*> peekByteOff ptr 16 -- Padding bits + offset of the third element
      <*> peekByteOff ptr 24 -- Next time check sizeof(Symbol) before you spend 6 hours debugging
      <*> peekByteOff ptr 28
    poke ptr (Symbol sname sym_class value address segment) = do
      pokeByteOff ptr 0 sname
      pokeByteOff ptr 8 sym_class
      pokeByteOff ptr 16 value
      pokeByteOff ptr 24 address
      pokeByteOff ptr 28 segment
  instance Storable Token where
    alignment _ = 8
    sizeOf _    = 12
    peek ptr    = Token
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr 8
    poke ptr (Token tname tok_class) = do
      pokeByteOff ptr 0 tname
      pokeByteOff ptr 8 tok_class

  peekTname :: Token -> IO String 
  peekTname = peekCString . tname

  peekSname :: Symbol -> IO String 
  peekSname = peekCString . sname

  data Label = Label Integer Integer -- # address

  --                     op,   src,    tf,  dest 
  data Quad = QuadQQS  Token   Quad   Quad Symbol --   +, T1, T2, T3
            | QuadSSS  Token Symbol Symbol Symbol --   +.  X,  Y, T2
            | QuadSQS  Token Symbol   Quad Symbol --   +,  X, T1, T2
            | QuadQSS  Token   Quad Symbol Symbol --   +, T1,  X, T2
            | QuadSS   Token Symbol        Symbol --   =,  Y,  -,  X
            | QuadQS   Token   Quad        Symbol --   =, T1,  -,  X
            | QuadIW   Token   Quad   Quad        --  IF, T1, T2,  -
            | QuadS    Token Symbol               -- ODD,  X
            | QuadQ    Token Quad                 -- ODD, T1
            | QuadB                        Symbol -- (asm)
            | QuadP    Token Symbol   Quad        -- PROCEDURE, Multiply, B1
            | QuadIdS        Symbol Symbol Symbol -- array, X
            | QuadIdQ        Symbol   Quad Symbol -- array, T1
            | QuadSQ   Token Symbol          Quad --   =,  X,  -, arr[0]
            | QuadQQ   Token   Quad          Quad --   =, T1,  -  arr[0]
            | Invalid [TokenOrQuad]

  -- The coolest constructed type monad in all of Haskell: Either
  type TokenOrQuad  = (Either Token Quad)

  data Action = Equal | Yields | Takes | Skip | Error deriving (Eq, Enum) -- Shorthand (Equal <-> 0, Yields <-> 1...)
  -- Looks nice for sure, really impractical in terms of distance from left margin
  -- If it makes the code more readable then so be it

  -- Oh look here's a more custom one
  -- Longhand enumeration begin
  data TokenClass = TINVALID
                  | XCLASS | XVAR | XCONST | XPROC | XARR
                  | IDENT | INTEGER
                  | IF | THEN | WHILE | DO | CALL | ODD
                  | PRINT | GET
                  | ASSIGN | ADDOP | MOP | RELOP
                  | LB | RB | LP | RP | LS | RS
                  | COMMA | SEMI
                    deriving (Eq, Show)

  -- Turning integers in memory locations into enums and back again
  token_pairs_enum  = [ (TINVALID, 0), (XCLASS, -1), (XVAR, -2), (XCONST, -3), (IDENT, 6), 
                        (IF, -4), (THEN, -5), (XPROC, -6), (WHILE, -7), (DO, -8), (CALL, -9), (ODD, -10),
                        (PRINT, -11), (GET, -12), (XARR, -13),
                        (INTEGER, 4),
                        (ASSIGN, 12), (ADDOP, 21), (MOP, 10), (RELOP, 13), 
                        (LB, 25), (RB, 27), (COMMA, 29), (SEMI, 17), (LP, 31), (RP, 33), (LS, 42), (RS, 44) ]

  -- Turning enums into indexes in the lookup table and back again
  token_pairs_index = [ (TINVALID, -1),
                        (SEMI, 0), (ASSIGN, 1), (ADDOP, 2), (LP, 3), (RP, 4), (MOP, 5), 
                        (IF, 6), (THEN, 7), (ODD, 8), (RELOP, 9),
                        (LB, 10), (RB, 11), (CALL, 12), (WHILE, 13), (DO, 14), (COMMA, 15),
                        (XCLASS, 16), (XVAR, 17), (XPROC, 18), (XCONST, 19), (PRINT, 20), (GET, 21), (XARR, 22), (LS, 23), (RS, 24) ]
                        
  instance Enum TokenClass where
    fromEnum x = fromJust (lookup x token_pairs_enum) -- Corresponding entry in the pairs list
    toEnum x   = fromJust (lookup x (map swap token_pairs_enum)) -- Reversed corresponding entry
  -- Longhand enumeration end
  -- I don't like enumerations in haskell
