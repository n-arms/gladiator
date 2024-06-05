module Live (
  findLiveRanges,
  Line,
  StackSlot,
  ActiveAlloc,
  LiveRange(..),
  LiveRanges(..),
  rangeStart,
  rangeEnd
) where 

import IR
import Program(Name)
import Lower(index)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict(HashMap)
import qualified Data.HashSet as HashSet
import Data.HashSet(HashSet)
import Data.Functor

type Line = Int
type StackSlot = Int
type ActiveAlloc = Int

rangeStart :: LiveRange -> Line
rangeStart (LiveRange line _) = line

rangeEnd :: LiveRange -> Line
rangeEnd (LiveRange _ line) = line

data LiveRange = LiveRange Line Line

instance Semigroup LiveRange where
  (LiveRange start end) <> (LiveRange start' end') = LiveRange (start `min` start') (end `min` end')

instance Monoid LiveRange where
  mempty = LiveRange maxBound minBound

-- liveRanges[i].start is the line of which i is defined, or 0 if it is a block argument
-- liveRanges[i].end is the line of which i is last used, or minBound if it is never used
data LiveRanges = LiveRanges (HashMap Id LiveRange)

isLive :: LiveRanges -> Line -> Id -> Bool
isLive (LiveRanges ranges) line id =
  let LiveRange start end = ranges HashMap.! id in
  line >= start && line < end

instance Semigroup LiveRanges where
  (LiveRanges ranges) <> (LiveRanges ranges') = LiveRanges $ HashMap.unionWith (<>) ranges ranges'

instance Monoid LiveRanges where
  mempty = LiveRanges HashMap.empty

liveRange :: Line -> Line -> Id -> LiveRanges
liveRange start end id = LiveRanges $ HashMap.singleton id $ LiveRange start end

startRange :: Line -> Id -> LiveRanges
startRange start id = LiveRanges $ HashMap.singleton id $ LiveRange start minBound

endRange :: Line -> Id -> LiveRanges
endRange end id = LiveRanges $ HashMap.singleton id $ LiveRange maxBound end

findLiveRanges :: Block -> LiveRanges
findLiveRanges (Block args stmts result) = let
  argRanges = mconcat $ [0..(length args) - 1] <&> \arg -> liveRange 0 0 arg
  stmtRanges = mconcat $ (index stmts) <&> \(stmt, index) -> stmtLiveRanges index stmt
  resultRanges = resultLiveRanges (length args) result in
  argRanges <> stmtRanges <> resultRanges

stmtLiveRanges :: Int -> Statement -> LiveRanges
stmtLiveRanges index (Let name _ expr) = (startRange index name) <> (mconcat $ (exprVariables expr) <&> endRange index)
   
exprVariables :: Expr -> [Id]
exprVariables (Atom atom) = atomVariables atom
exprVariables (Primitive _ args) = args >>= atomVariables
exprVariables (Call _ args) = args >>= atomVariables

atomVariables :: Atom -> [Id]
atomVariables (Variable var) = [var]
atomVariables (Literal _) = []

resultLiveRanges :: Int -> Result -> LiveRanges
resultLiveRanges index (Return result) = mconcat $ (atomVariables result) <&> endRange index 
resultLiveRanges index (Jump j) = blockJumpLiveRanges index j
resultLiveRanges index (JumpIf p j1 j2) = (blockJumpLiveRanges index j1) <> (blockJumpLiveRanges index j2)

blockJumpLiveRanges :: Int -> BlockJump -> LiveRanges
blockJumpLiveRanges index (BlockJump _ args) = mconcat $ do
  arg <- args
  var <- (atomVariables arg)
  return $ endRange index var

