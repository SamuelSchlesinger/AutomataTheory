-- | Original author: Kit Fredurra
-- | Some refactorings to "haskellify" the code
-- | From the paper by Armando B. Matos
-- |     Monoid machines: a O(log n) parser for regular languages.
-- |    (http://www.dcc.fc.up.pt/~acm/semigr.pdf)

import Control.Monad
import Control.Parallel.Strategies

data DFA symbol state = DFA {
    states   :: [state]
  , start    :: state
  , delta    :: symbol -> state -> Maybe state
  , accept   :: Elem state -> Bool -- g in the paper
}

type Elem state = state -> Maybe state

-- | Creates a monoid element corresponding to the given symbol
createElem :: DFA symbol state -> symbol -> Elem state
createElem dfa s = (delta dfa) s

-- | Composition of elements in the generated monoid
mmul :: Elem state -> Elem state -> Elem state
f `mmul` g = f >=> g

-- | Converts a word to its monoidal representation
fword :: [symbol] -> DFA symbol state -> [Elem state]
fword xs dfa = map (createElem dfa) xs

mparse :: [Elem state] -> DFA symbol state -> Bool
mparse [x] dfa = (accept dfa) x
mparse (x:y:xs) dfa = mparse ((x `mmul` y):xs) dfa

parse :: [symbol] -> DFA symbol state -> Bool
parse s dfa = mparse (fword s dfa) dfa
