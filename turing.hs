data List a = List a (List a)
data Tape alphabet = Tape (List alphabet) alphabet (List alphabet)

symbol :: Tape alphabet -> alphabet
symbol (Tape _ a _) = a

shiftLeft :: Tape alphabet -> Tape alphabet
shiftLeft (Tape (List a as) b cs) = Tape as a (List b cs)
shiftRight (Tape as b (List c cs)) = Tape (List b as) c cs

writeAndShift :: Direction -> alphabet -> Tape alphabet -> Tape alphabet
writeAndShift GoLeft b' (Tape (List a as) b cs) = Tape as a (List b' cs)
writeAndShift GoRight b' (Tape as b (List c cs)) = Tape (List b' as) c cs

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
    let newtape = writeAndShift direction sym $ tape tm
    run (tm { tape = newtape, state = state })

decide :: TM state alphabet -> Decision
decide tm = case run tm of
              Left decision -> decision
              _             -> undefined

failingTM :: TM Int Binary
failingTM = TM (\state symbol -> Left Reject) blankTape 0

haltlessTM = TM (\state symbol -> Right (state + 1, symbol, GoRight)) blankTape 0

evenZeroTransition 0 Blank = Left Accept
evenZeroTransition 1 Zero = Right (0, Zero, GoRight)
evenZeroTransition 0 Zero = Right (1, Zero, GoRight)
evenZeroTransition _ _ = Left Reject

evenZeroTape = Tape blanks Zero (list Blank [Zero | x <- [1..100]])

evenZeroTM :: TM Int Binary
evenZeroTM = TM evenZeroTransition evenZeroTape 0


