-- | Some marvelous redditor wrote these functions

type NFA q c = (c -> q -> [q], q, q -> Bool)
accepts :: (Eq q) => NFA q c -> Bool
accepts (d, i, f) = any f . foldl' (flip $ (nub .) . concatMap . d) [i]

type DFA q c = (c -> q -> q, q, q -> Bool)
accepts (d, i, f) = f . foldl' d i
