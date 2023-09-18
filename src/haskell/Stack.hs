-- Stack.hs
-- Generic functional linked stack
-- Really just a wrapper for a list, but much more intuitive to read than 
--  Haskell's list syntax when used in state transition functions

module Stack where
  type Stack d = [d]
  
  push :: Stack d -> d -> Stack d
  push s e = e:s -- Construct list from existing list 
  
  pop :: Stack d -> Stack d
  pop [] = []
  pop (e:s) = s
  
  top :: Stack d -> d
  top [] = error "Stack.top: empty stack"
  top (e:s) = e
  
  -- Top element matching some criterion decided by a passed function
  filterTop :: Stack d -> (d -> Bool) -> Maybe d
  filterTop [] f = Nothing
  filterTop (t:rest) f | f t       = Just t
                        | otherwise = filterTop rest f