import Control.Monad
import Data.List (intersect)

-- | A nondeterministic finite state automata
type NFA q c = ([(q, c, q)], q, [q]) -- (transition, start, accepting states)

-- | Maps a transition relation, a current state, and a current character
-- | into a set of new states.
transit :: (Eq q, Eq c) => [(q, c, q)] -> q -> c -> [q] 
transit d q0 c = [ q' | (q, c', q') <- d, q == q0, c' == c]

-- | Maps a transition relation, a list of current states, and a current character
-- | into a set of new states.
totaltransit :: (Eq q, Eq c) => [(q, c, q)] -> [q] -> c -> [q]
totaltransit d q0s c = rmdup $ join [transit d q0 c | q0 <- q0s]
    
-- | Removes duplicates from a list
rmdup :: (Eq q) => [q] -> [q]
rmdup xs = rmdup' [] xs where
    rmdup' ss [] = ss
    rmdup' ss (x:xs) = rmdup' (if x `elem` ss then ss else (x:ss)) xs

-- | Does the NFA accept the given string?
accepts :: (Eq q, Eq c) => NFA q c -> [c] -> Bool
accepts (d, q0, qF) cs = run' d [q0] qF cs where
    run' :: (Eq q, Eq c) => [(q, c, q)] -> [q] -> [q] -> [c] -> Bool
    run' _   q0s qF [] = (q0s`intersect` qF) /= []
    run' d q0s qF (c:cs) = run' d (totaltransit d q0s c) qF cs

