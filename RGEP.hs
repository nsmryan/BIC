
import System.Random.MWC.Monad
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Word
import Data.Monoid
import Data.List
import Data.List.Split
import Data.Bits
import Data.Function
import qualified Data.Traversable as T
import qualified Data.Foldable as F

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Primitive.Class


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

onlyRunnable ops = take (lastFullProg ops) ops where
  incrProgs = scanl1 (<>) . map arity
  fullProg = findIndices (== progArity) 
  getLength as = 1 + last as
  lastFullProg = getLength . fullProg . incrProgs

cleanProg = onlyRunnable . filterUnderflows

stackEffect :: [Op a] -> Arity
stackEffect = mconcat . map arity

runOpsUnsafe ops = foldl (.) id (map program . reverse $ ops) []
runOps = runOpsUnsafe . cleanProg

runProgram prog = do
  let filtered = filterUnderflows prog 
  guard . not . null $ filtered
  let runnable = onlyRunnable filtered
  guard . not . null $ runnable
  return $ runOpsUnsafe runnable

runProgramWithDefault def prog = maybe def head (runProgram prog)

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

type Decoder a = Word32 -> Op a

decode :: [Op a] -> Decoder a
decode ops = uncurry decodeSymbols $ splitSymbols ops

decodeSymbols :: [Op a] -> [Op a] -> Decoder a
decodeSymbols terms nonterms = let
  termsV = V.fromList terms
  nontermsV = V.fromList nonterms
  in \ w -> let index = fromIntegral $ w `shiftR` 1
                symV = if testBit w 0 then nontermsV else termsV
                in symV V.! (index `mod` V.length symV)

testDecode = do
  let ops = [plusOp, timesOp, oneTerm, twoTerm, zeroTerm] :: [Op Int]
  let termIndices = map (`shiftL` 1) [0..5]
  let nontermIndices = map ((`setBit` 0) . (`shiftL` 1)) [0..5]
  print termIndices
  print nontermIndices
  putStrLn ""
  print $ map (decode ops) termIndices
  print $ map (decode ops) nontermIndices

{- Generate Random population -}
type Ind32 = S.Seq Word32
type Pop32 = S.Seq Ind32
type IndR = S.Seq Double
type PopR = S.Seq IndR

ind32 :: (MonadPrim m) =>
  Int -> Word32 -> Rand m Ind32
ind32 is bits = S.replicateM is $ uniformR (0, (2^bits)-1)

pop32 :: (MonadPrim m) =>
  Int -> Int -> Word32 -> Rand m Pop32
pop32 ps is bits = S.replicateM ps $ ind32 is bits

indR :: (MonadPrim m) =>
  Int -> Rand m IndR
indR is = S.replicateM is $ uniformR (0, 1)

popR :: (MonadPrim m) =>
  Int -> Int -> Rand m PopR
popR ps is = S.replicateM ps $ indR is

{- Incremental Population Based Learning -}
minBy f a b = if f a < f b then a else b
maxBy f a b = if f a > f b then a else b

timesM :: (Monad m) => Int -> a -> (a -> m a) -> m a
timesM i a m = foldl (>=>) return (replicate i m) $ a

genInd :: (MonadPrim m, T.Traversable t) =>
  t Double -> Rand m (t Bool)
genInd = T.mapM $ \ p -> do
  p' <- uniformR (0, 1)
  return $ p' > p

b2d :: Bool -> Double
b2d True = 1.0
b2d False = 0.0

adjustProb learn neglearn p minBit maxBit =
  if minBit == maxBit
    then (p * (1 - learn))  + (b2d minBit * learn)
    else (p * (1 - learn2)) + (b2d minBit * learn2) where
      learn2 = learn + neglearn

mutIBPL probs' mutRate mutShift = S.zipWith3 mut probs' <$> bs <*> ps where
  len = S.length probs'
  ps = S.replicateM len (uniformR (0.0 :: Double, 1.0))
  bs = S.replicateM len (uniformR (0.0 :: Double, 1.0))
  mut p b p' = let b' = fromIntegral . round $ b in
    if p' < mutRate
      then (p * (1 - mutShift)) + (b' * mutShift) 
      else p

pbil' ps learn neglearn mutRate mutShift express evaluate (best, probs) = do
  inds <- S.replicateM ps $ genInd probs
  let inds' = express <$> inds
  let evaled = S.zip inds $ evaluate <$> inds'
  let minInd = F.minimumBy (compare `on` snd) evaled
  let maxInd = F.maximumBy (compare `on` snd) evaled
  let best' = minBy snd best minInd
  let probs' = S.zipWith3 (adjustProb learn neglearn) probs (fst minInd) (fst maxInd)
  probs'' <- mutIBPL probs' mutRate mutShift
  return (best', probs'')

pbil ps is gens learn neglearn mutRate mutShift express eval = do
  let probs = S.replicate is 0.5
  initialBest <- genInd probs
  ((finalBest, fit), probs) <- timesM gens ((initialBest, eval $ express initialBest), probs) $ pbil' ps learn neglearn mutRate mutShift express eval
  return (express finalBest, fit, probs)

maxValue = ((0 -) . F.sum . fmap fromEnum)
testPBIL = do
  let ps = 20
  let is = 100
  let gens = 1000
  (ind, fit,  probs) <- runWithSystemRandom . asRandIO $ pbil ps is gens 0.05 0.01 0.2 0.05 id maxValue
  print $ negate fit

{- RGEP PBIL -}
b2i True = 1
b2i False = 0

pack :: [Bool] -> Word32
pack bs = foldl (\ w b -> (w `shiftL` 1) .|. b2i b) 0 bs

collect ::  Int -> S.Seq Bool -> [Word32]
collect n bs = pack <$> (chunksOf n $ F.toList bs)

rgepPBIL ops ps is gens learn neglearn mutRate mutShift eval =
  pbil ps bs gens learn neglearn mutRate mutShift (collect bits) eval where
    bs = bits * is
    bits = bitsUsed ops

testRGEP = do
  let ps = 50
  let is = 20
  let gens = 1000
  let ops = [zeroTerm, oneTerm, twoTerm, plusOp, timesOp, dup]
  let decoder = decode ops
  let eval = (0-) . runProgramWithDefault 0 . fmap decoder
  (ind, fit, probs) <- runWithSystemRandom . asRandIO $ rgepPBIL ops ps is gens 0.1 0.075 0.02 0.05 eval
  print probs
  putStrLn ""
  print $ fmap name . cleanProg . fmap decoder $ ind
  putStrLn ""
  print $ negate fit

{- Random Mutation Hill Climbing RGEP -}

type Mutator p = [Int] -> p -> p

rgepMutator :: Int -> Int -> Int -> [Int] -> [Mutator Pop32]
rgepMutator ps is bits ixs = undefined 

rmhc :: (MonadPrim m) =>
  Int -> Int -> Double -> (Ind32 -> m Ind32) -> ([Word32] -> Double) -> m (Ind32, Pop32)
rmhc is gens mutRate mutFunction eval = undefined

pmIndividual = undefined

indexFromSource :: (a -> a) -> Int -> Seq a -> State [Int] (Seq a)
indexFromSource f n s = do
  skips <- skip n
  let (top, bottom) = S.split skips s

testRMHC = do
  let ops = [zeroTerm, oneTerm, twoTerm, plusOp, timesOp, dup]
  let decoder = decode ops
  let eval = (0.0 -) . fromIntegral . runProgramWithDefault 0 . fmap decoder
  (ind, fit) <- runWithSystemRandom . asRandIO $ rmhc 10 100 0.1 pmIndividual eval
  print ind
  print fit

{- GA RGEP -}

{- Original RGEP -}

