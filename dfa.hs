{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ComputationalMachines where

type DFA seq state symbol val = (symbol -> state -> state) -> (state -> val) -> state -> seq symbol -> val

dfa :: forall symbol state val.(symbol -> state -> state) -> (state -> val) -> state -> [symbol] -> val
dfa transition value starting s = f s starting where
    f :: [symbol] -> state -> val
    f [] state = value state
    f (x:xs) state = f xs (transition x state)

ndfa :: forall state symbol val n. 
    Monad n
 => (symbol -> state -> n state)
 -> (state -> val) 
 -> state 
 -> [symbol]
 -> n val
ndfa transition value starting s = f s starting where
  f :: [symbol] -> state -> n val
  f [] state = pure (value state)
  f (x:xs) state = do
    state' <- transition x state
    f xs state'

tm :: forall tape state symbol val.
    (state -> symbol -> (forall x. tape x -> (tape x, state)))
 -> (forall x. tape x -> x)
 -> (state -> val)
 -> (state -> Bool)
 -> state
 -> tape symbol
 -> val
tm transition read value finished state tape = f tape state where
  f :: tape symbol -> state -> val
  f tape state = if finished state then value state else uncurry f (transition state (read tape) tape)

ntm :: forall n tape state symbol val.
    Monad n
 => (state -> symbol -> (forall x. tape x -> n (tape x, state)))
 -> (forall x. tape x -> x)
 -> (state -> val)
 -> (state -> Bool)
 -> state
 -> tape symbol
 -> n val
ntm transition read value finished state tape = f tape state where
  f :: tape symbol -> state -> n val
  f tape state =
    if finished state
      then pure (value state)
      else do
        (tape', state') <- transition state (read tape) tape
        f tape' state'
