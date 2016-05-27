data List a = List a (List a)
data Tape alphabet = Tape (List alphabet) alphabet (List alphabet)

symbol :: Tape alphabet -> alphabet
symbol (Tape _ a _) = a

shiftLeft :: Tape alphabet -> Tape alphabet
shiftLeft (Tape (List a as) b cs) = Tape as a (List b cs)
shiftRight (Tape as b (List c cs)) = Tape (List b as) c cs

data Binary  = Blank | Zero | One deriving (Show, Eq)
data Direction = GoLeft | GoRight | StayHere deriving (Show, Eq)
data Decision  = Accept | Reject deriving (Show, Eq)

blanks = List Blank blanks
blankTape = Tape blanks Blank blanks

list :: alphabet -> [alphabet] -> List alphabet
list b [] = blanks where blanks = List b blanks
list b (x:xs) = List x (list b xs)

data TM state alphabet 
  = TM { transition :: state -> alphabet -> Either Decision (state, alphabet, Direction), 
         tape       :: Tape alphabet, 
         state      :: state }

run :: TM state alphabet -> Either Decision (TM state alphabet)
run tm = do 
    (state, sym, direction) <- transition tm (state tm) (symbol $ tape tm)
    let newtape = if      direction == GoLeft  then shiftLeft $ tape tm
                  else if direction == GoRight then shiftRight $ tape tm
                  else tape tm
    run tm

failingTM :: TM Int Binary
failingTM = TM (\state symbol -> Left Reject) blankTape 0


