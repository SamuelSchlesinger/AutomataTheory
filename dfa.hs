data DFA state symbol val = DFA {
    dtransition :: symbol -> state -> state
  , dvalue      :: state  -> val
  , dstarting   :: state
}

evalDFA :: DFA state symbol val -> [symbol] -> val
evalDFA dfa s = f dfa s (dstarting dfa) where
    f dfa [] state = (dvalue dfa) state
    f dfa (x:xs) state = f dfa xs (dtransition dfa x state)
