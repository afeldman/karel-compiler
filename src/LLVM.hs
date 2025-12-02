{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.AST.Instruction
import LLVM.AST.Operand
import LLVM.AST.Type (i32, i64, i8, double, void, ptr)
import LLVM.Context
import LLVM.Module
import qualified LLVM.Pretty as Pretty

import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map
import Data.Text.Lazy (unpack)
import Data.String (fromString)

-- | Codegen State
data CodegenState = CodegenState
  { currentBlock :: AST.Name                              -- Current basic block
  , blocks       :: Map.Map AST.Name BlockState           -- All basic blocks
  , blockCount   :: Int                                   -- Block counter
  , count        :: Word                                  -- Unnamed value counter
  , names        :: Map.Map String Int                    -- Name counter
  } deriving Show

data BlockState = BlockState
  { idx   :: Int                                          -- Block index
  , stack :: [AST.Named AST.Instruction]                  -- Stack of instructions
  , term  :: Maybe (AST.Named AST.Terminator)             -- Block terminator
  } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

-- | Empty codegen state
emptyCodegen :: CodegenState
emptyCodegen = CodegenState (AST.Name "entry") Map.empty 1 0 Map.empty

-- | Empty block state
emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

-- | Create new basic block
createBlock :: AST.Name -> Codegen AST.Name
createBlock name = do
  bls <- gets blocks
  ix <- gets blockCount
  modify $ \s -> s { blocks = Map.insert name (emptyBlock ix) bls
                   , blockCount = ix + 1
                   }
  return name

-- | Set current block
setBlock :: AST.Name -> Codegen ()
setBlock name = modify $ \s -> s { currentBlock = name }

-- | Get current block
getBlock :: Codegen AST.Name
getBlock = gets currentBlock

-- | Add instruction to current block
instr :: AST.Instruction -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = LocalReference T.double n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (n := ins) : i })
  return ref

-- | Add terminator to current block
terminator :: AST.Named AST.Terminator -> Codegen ()
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })

-- | Get current block state
current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-- | Modify current block
modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

-- | Generate fresh unnamed variable
fresh :: Codegen AST.Name
fresh = do
  i <- gets count
  modify $ \s -> s { count = i + 1 }
  return $ AST.UnName i

-- | Generate unique name
uniqueName :: String -> Codegen AST.Name
uniqueName prefix = do
  ns <- gets names
  let count = Map.findWithDefault 0 prefix ns
  modify $ \s -> s { names = Map.insert prefix (count + 1) ns }
  return $ AST.Name $ fromString $ prefix ++ show count

-- | Create basic blocks from state
createBlocks :: Codegen a -> [AST.BasicBlock]
createBlocks m = map makeBlock $ Map.toList $ blocks $ execState (runCodegen m) emptyCodegen

makeBlock :: (AST.Name, BlockState) -> AST.BasicBlock
makeBlock (l, BlockState _ s t) = AST.BasicBlock l (reverse s) (makeTerm t)
  where
    makeTerm (Just x) = x
    makeTerm Nothing = error $ "Block has no terminator: " ++ show l

-- | Integer constant
int :: Integer -> Operand
int n = ConstantOperand $ C.Int 32 n

-- | Double constant
double :: Double -> Operand
double n = ConstantOperand $ C.Float (F.Double n)

-- | Floating point operations
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

-- | Integer operations
iadd :: Operand -> Operand -> Codegen Operand
iadd a b = instr $ Add False False a b []

isub :: Operand -> Operand -> Codegen Operand
isub a b = instr $ Sub False False a b []

imul :: Operand -> Operand -> Codegen Operand
imul a b = instr $ Mul False False a b []

idiv :: Operand -> Operand -> Codegen Operand
idiv a b = instr $ SDiv False a b []

-- | Comparison operations
fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp cond a b = instr $ ICmp cond a b []

-- | Memory operations
alloca :: AST.Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store ptr val = do
  instr $ Store False ptr val Nothing 0 []
  return ()

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- | Control flow
br :: AST.Name -> Codegen ()
br label = terminator $ Do $ Br label []

cbr :: Operand -> AST.Name -> AST.Name -> Codegen ()
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Maybe Operand -> Codegen ()
ret val = terminator $ Do $ Ret val []

-- | Function calls
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (map (\a -> (a, [])) args) [] []

-- | Helper: call without return value
callVoid :: Operand -> [Operand] -> Codegen ()
callVoid fn args = do
  instr $ Call Nothing CC.C [] (Right fn) (map (\a -> (a, [])) args) [] []
  return ()

-- | Type conversions
sitofp :: AST.Type -> Operand -> Codegen Operand
sitofp ty val = instr $ SIToFP val ty []

fptosi :: AST.Type -> Operand -> Codegen Operand
fptosi ty val = instr $ FPToSI val ty []

-- | Compile LLVM AST to LLVM IR string
toLLVMIR :: AST.Module -> String
toLLVMIR mod = unpack $ Pretty.ppllvm mod

-- | Compile LLVM AST to bitcode file
toBitcode :: FilePath -> AST.Module -> IO ()
toBitcode file mod = do
  withContext $ \ctx ->
    withModuleFromAST ctx mod $ \m ->
      writeBitcodeToFile (File file) m

-- | Compile LLVM AST to object file
toObjectFile :: FilePath -> AST.Module -> IO ()
toObjectFile file mod = do
  withContext $ \ctx ->
    withModuleFromAST ctx mod $ \m ->
      writeObjectToFile (File file) m

-- | Pretty print LLVM IR to stdout
printLLVMIR :: AST.Module -> IO ()
printLLVMIR mod = putStrLn $ toLLVMIR mod

-- | Write LLVM IR to file
writeLLVMIR :: FilePath -> AST.Module -> IO ()
writeLLVMIR file mod = writeFile file $ toLLVMIR mod
