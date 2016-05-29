-- | Original author: Kit Fredurra
-- | Some refactorings to "haskellify" the code

import Control.Monad
import Control.Parallel.Strategies

data DFA symbol state = DFA {
    states   :: [state] 
  , start    :: state
  , delta    :: state -> symbol -> Maybe state 
  , accept   :: Elem state -> Bool -- g in the paper
}

type Elem state = state -> Maybe state

createElem :: symbol -> DFA symbol state -> Elem state
createElem s dfa = flip (delta dfa) s

mmul :: Elem state -> Elem state -> Elem state
f `mmul` g = f >=> g

fword :: [symbol] -> DFA symbol state -> [Elem state]
fword xs dfa = map ((flip createElem) dfa) xs

mparse :: [Elem state] -> DFA symbol state -> Bool
mparse [x] dfa = (accept dfa) x
mparse (x:y:xs) dfa = mparse ((x `mmul` y):xs) dfa


parse :: [symbol] -> DFA symbol state -> Bool
parse s dfa = mparse (fword s dfa) dfa

