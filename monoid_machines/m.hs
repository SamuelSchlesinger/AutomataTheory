{-# LANGUAGE KindSignatures, MultiParamTypeClasses, ScopedTypeVariables#-}

class (Monoid monoid) => MonoidMachine monoid (machine ::  * -> * -> *) where
    mmap     :: machine monoid symbol -> symbol -> monoid
    accepts  :: machine monoid symbol -> monoid -> Bool

parses :: (MonoidMachine monoid machine) => machine monoid symbol -> [symbol] -> Bool
machine `parses` string = machine `accepts` (mconcat $ map (mmap machine) string)
