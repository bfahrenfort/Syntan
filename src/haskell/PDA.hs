-- PDA.hs
-- Generic Pushdown Automaton

module PDA where
  import Stack
  -- Defines a Pushdown Automaton with delta, Z, and F
  -- Doesn't really look like the formula with how many modifications I've made
  -- Deviations from formula: 
  --  delta must return a symbol list as well
  --  F needs a validity and an end boolean
  --  delta needs a list of tokens as opposed to the next token
  --  Needs a symbol generic type
  --  Needs a bool in delta to let parse know to recheck the last token
  --  Alphabets (Q/Sigma/Gamma) aren't needed
  --  State isn't needed since the stack controls the entire state, 
  --    q_0 removed and state removed from delta and F
  --  Z is a Stack structure instead of just the initial stack element
  --  push_down returns a tuple of the validity and the stack for viewing after completion
  type P_D_Automaton symbol token stack_el = ([token] -> Stack stack_el -> [symbol] -> IO (Bool, Stack stack_el, [symbol]), 
                                             Stack stack_el, 
                                             Stack stack_el -> (Bool, Bool, Stack stack_el))
