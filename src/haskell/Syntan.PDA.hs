-- PDA.hs
-- Generic Pushdown Automaton

module Syntan.PDA where
  import Syntan.Stack
  -- Defines a Pushdown Automaton with delta, Z, and F
  -- Deviations from formula: 
  --  Needs a symbol generic type
  --  Alphabets (Q/Sigma/Gamma) aren't needed
  --  State isn't needed since the stack controls the entire state, 
  --    q_0 removed and state removed from delta and F
  --  Z is a Stack structure instead of just the initial stack element
  --  push_down returns a tuple of the validity and the stack for viewing after completion
  type P_D_Automaton symbol token stack_el = (token -> Stack stack_el -> [symbol] -> IO (Stack stack_el), 
                                             Stack stack_el, 
                                             Stack stack_el -> (Bool, Stack stack_el))
