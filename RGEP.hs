import System.Random.MWC

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Word
import Data.Monoid
import Data.List
import Data.Bits

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Primitive


{- Operators -}
type Sym = String

data Arity = Arity { inputs :: Int
                   , outputs :: Int
                   } deriving (Show, Eq)

instance Monoid Arity where
  (Arity ins outs) `mappend` (Arity ins' outs') =
    Arity (ins   + max 0 (ins' - outs))
          (outs' + max 0 (outs - ins'))
  mempty = Arity 0 0
 
type StackProg a = [a] -> [a]

data Op a = Op { name :: Sym
               , arity :: Arity
               , program :: StackProg a
               }

instance Show (Op a) where
  show = name

mkOp1 name f = Op name (Arity 1 1) (onHead f) where onHead f (a:as) = f a : as
mkOp2 name f = Op name (Arity 2 1) (firstTwo f) where firstTwo f (a:a':as) = a `f` a' : as
mkTerm name v = Op name (Arity 0 1) (v:)

-- Plain arithmatic operations
plusOp = mkOp2 "+" (+)
minusOp = mkOp2 "-" (-)
timesOp = mkOp2 "*" (*)
divOp = mkOp2 "/" (/)
incOp = mkOp1 "inc" (1+)
zeroTerm = mkTerm "0" (0)
oneTerm = mkTerm "1" (1)
twoTerm = mkTerm "2" (2)

-- Stack operations
dup = Op "dup" (Arity 1 2) (uncurry (:) . (head &&& id))
drop = Op "drop" (Arity 1 0) tail
swap = Op "swap" (Arity 2 2) $ \ (a:a':as) -> a':a:as
tuck = Op "tuck" (Arity 2 3) $ \ (a:a':as) -> (a:a':a:as)

-- Polynomials
-- Expressions
--

filterUnderflows ops = map fst . filter (snd) . zip ops . snd . mapAccumL composeEffects mempty $ ops
composeEffects arr op = if outputs arr < inputs (arity op) then (arr, False) else (arr <> arity op, True)

progArity = Arity 0 1 --from [] to [a]

dec a = a-1

onlyRunnable = uncurry take . (lastFullProg &&& id) where
  lastFullProg = (1+) . last . findIndices (== progArity) . scanl1 (<>) . map arity

cleanProg = onlyRunnable . filterUnderflows

stackEffect :: [Op a] -> Arity
stackEffect = mconcat . map arity

runOpsUnsafe ops = foldl (.) id (map program . reverse $ ops) []
runOps = runOpsUnsafe . cleanProg

runProgram prog = let prog' = cleanProg prog in
  case prog' of
    [] -> Nothing
    as -> Just . head . runOpsUnsafe $ prog'

runProgramWithDefault def prog = maybe def id (runProgram prog)

testStacks = do
      let prog = [oneTerm, twoTerm, plusOp, dup, timesOp, twoTerm] 
      let progUnderflow = [oneTerm, twoTerm, plusOp, timesOp] 
      print $ "safe program"
      print $ map name prog
      print $ "safe program, filtered"
      print $ map name $ filterUnderflows prog
      print $ "safe program, cleaned"
      print $ map name $ cleanProg prog
      print $ "safe program, run unsafe"
      print $ runOpsUnsafe prog
      print $ "safe program, run safe"
      print $ runOps prog
      print $ "unsafe program"
      print $ map name progUnderflow
      print $ "unsafe program, filtered"
      print $ map name $ filterUnderflows progUnderflow
      print $ "unsafe program, cleaned"
      print $ map name $ cleanProg progUnderflow
      print $ "unsafe program, safely"
      print $ runOps progUnderflow
      print $ "unsafe program, unsafely"
      print $ runOpsUnsafe progUnderflow

{- Express raw bits -}

bitsUsed ops = let (terms, nonterms) = splitSymbols ops in
  if length terms > 0 && length nonterms > 0
    then 1 + max (bitsRequired (length terms)) (bitsRequired (length nonterms))
    else error "There must be at least one terminal symbol and one nonterminal symbol"

bitsRequired n = ceiling $ logBase 2 (fromIntegral n)

splitSymbols ops = (filter ((==0) . inputs . arity) ops, filter ((/=0) . inputs . arity) ops)

decode :: [Op a] -> (Word32 -> Op a)
decode ops = uncurry decodeSymbols $ splitSymbols ops

decodeSymbols :: [Op a] -> [Op a] -> (Word32 -> Op a)
decodeSymbols terms nonterms = let
  termsV = V.fromList terms
  nontermsV = V.fromList nonterms
  in \ w -> let index = fromIntegral $ w `shiftR` 1
                symV = if testBit w 0 then nontermsV else termsV
                in symV V.! (index `mod` V.length symV)

testDecode = do
  let ops = [plusOp, timesOp, oneTerm, twoTerm, zeroTerm]
  let termIndices = map (`shiftL` 1) [0..5]
  let nontermIndices = map ((`setBit` 0) . (`shiftL` 1)) [0..5]
  print termIndices
  print nontermIndices
  putStrLn ""
  print $ (map (decode ops) termIndices :: [Op Int])
  print $ (map (decode ops) nontermIndices :: [Op Int])

{- Generate Random population -}
randomIndividual :: (PrimMonad m) => Int -> Word32 -> Gen (PrimState m) -> m [Word32]
randomIndividual is bits g = replicateM is $ uniformR (0, (2^bits)-1) g 

randomPopulation :: (PrimMonad m) => Int -> Int -> Word32 -> Gen (PrimState m) -> m [[Word32]]
randomPopulation ps is bits g = replicateM ps $ randomIndividual is bits g

