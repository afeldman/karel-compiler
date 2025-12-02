{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.CallingConvention as CC
import LLVM.AST.Instruction
import LLVM.AST.Operand
import LLVM.AST.Type (i32, i64, i8, double, void, ptr)

import AbsKarel
import LLVM
import Control.Monad.State
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Maybe (fromMaybe)

-- | Module-level state
data ModuleState = ModuleState
  { variables :: Map.Map String Operand  -- Global and local variables
  , labels :: Map.Map String AST.Name    -- Labels for GOTO
  } deriving Show

emptyModule :: ModuleState
emptyModule = ModuleState Map.empty Map.empty

type ModuleGen = State ModuleState

-- | Generate LLVM module from Karel AST
codegenModule :: Karel -> AST.Module
codegenModule (KarelGrammer (Ident progName) _ _ decls stms _ routines) =
  AST.defaultModule
    { AST.moduleName = fromString progName
    , AST.moduleDefinitions = concat
        [ [mainFunction]
        , builtInDecls
        , map genRoutine routines
        ]
    }
  where
    mainFunction = AST.GlobalDefinition $ G.functionDefaults
      { G.name = AST.Name "main"
      , G.returnType = T.i32
      , G.basicBlocks = createBlocks $ do
          entry <- createBlock (AST.Name "entry")
          setBlock entry
          
          -- Declare variables from declarations
          mapM_ declareFromDeclBlock decls
          
          -- Generate code for statements
          mapM_ cgenStmt stms
          
          -- Return 0
          ret (Just $ int 0)
      }

-- | Built-in function declarations
builtInDecls :: [AST.Definition]
builtInDecls =
  [ -- printf
    AST.GlobalDefinition $ G.functionDefaults
      { G.name = AST.Name "printf"
      , G.returnType = T.i32
      , G.parameters = ([G.Parameter (T.ptr T.i8) (AST.Name "fmt") []], True)
      , G.basicBlocks = []
      }
  , -- puts
    AST.GlobalDefinition $ G.functionDefaults
      { G.name = AST.Name "puts"
      , G.returnType = T.i32
      , G.parameters = ([G.Parameter (T.ptr T.i8) (AST.Name "s") []], False)
      , G.basicBlocks = []
      }
  , -- scanf
    AST.GlobalDefinition $ G.functionDefaults
      { G.name = AST.Name "scanf"
      , G.returnType = T.i32
      , G.parameters = ([G.Parameter (T.ptr T.i8) (AST.Name "fmt") []], True)
      , G.basicBlocks = []
      }
  , -- Math functions
    AST.GlobalDefinition $ G.functionDefaults
      { G.name = AST.Name "sqrt"
      , G.returnType = T.double
      , G.parameters = ([G.Parameter T.double (AST.Name "x") []], False)
      , G.basicBlocks = []
      }
  , AST.GlobalDefinition $ G.functionDefaults
      { G.name = AST.Name "sin"
      , G.returnType = T.double
      , G.parameters = ([G.Parameter T.double (AST.Name "x") []], False)
      , G.basicBlocks = []
      }
  , AST.GlobalDefinition $ G.functionDefaults
      { G.name = AST.Name "cos"
      , G.returnType = T.double
      , G.parameters = ([G.Parameter T.double (AST.Name "x") []], False)
      , G.basicBlocks = []
      }
  ]

-- | Declare variables from declaration blocks
declareFromDeclBlock :: DeclBlock -> Codegen ()
declareFromDeclBlock (DeclVar (VDB _ vars)) = mapM_ declareVar vars
  where
    declareVar (VarDDtype (Ident name) _ _ dataType) = do
      let ty = karelTypeToLLVM dataType
      ptr <- alloca ty
      -- Store in symbol table (simplified - would need state monad)
      return ()
    declareVar _ = return ()
declareFromDeclBlock _ = return ()

-- | Convert Karel data types to LLVM types
karelTypeToLLVM :: DataTypes -> AST.Type
karelTypeToLLVM (DTParam PDTInt) = T.i32
karelTypeToLLVM (DTParam PDTReal) = T.double
karelTypeToLLVM (DTParam PDTBool) = T.i1
karelTypeToLLVM (DTString n) = T.ArrayType (fromIntegral n) T.i8
karelTypeToLLVM _ = T.double  -- Default to double

-- | Generate code for statements
cgenStmt :: Stm -> Codegen ()

-- Assignment: var := expression
cgenStmt (SAssign (Ident name) expr) = do
  val <- cgenExpr expr
  -- Simplified: would need symbol table lookup
  ptr <- alloca T.double
  store ptr val
  return ()

-- IF-THEN
cgenStmt (SIfThen cond thenStmts) = do
  condVal <- cgenExpr cond
  
  thenBB <- createBlock (AST.Name "if.then")
  mergeBB <- createBlock (AST.Name "if.merge")
  
  cbr condVal thenBB mergeBB
  
  setBlock thenBB
  mapM_ cgenStmt thenStmts
  br mergeBB
  
  setBlock mergeBB

-- IF-THEN-ELSE
cgenStmt (SIfThenElse cond thenStmts elseStmts) = do
  condVal <- cgenExpr cond
  
  thenBB <- createBlock (AST.Name "if.then")
  elseBB <- createBlock (AST.Name "if.else")
  mergeBB <- createBlock (AST.Name "if.merge")
  
  cbr condVal thenBB elseBB
  
  setBlock thenBB
  mapM_ cgenStmt thenStmts
  br mergeBB
  
  setBlock elseBB
  mapM_ cgenStmt elseStmts
  br mergeBB
  
  setBlock mergeBB

-- WHILE loop
cgenStmt (SWhile cond body) = do
  loopBB <- createBlock (AST.Name "while.loop")
  bodyBB <- createBlock (AST.Name "while.body")
  endBB <- createBlock (AST.Name "while.end")
  
  br loopBB
  
  setBlock loopBB
  condVal <- cgenExpr cond
  cbr condVal bodyBB endBB
  
  setBlock bodyBB
  mapM_ cgenStmt body
  br loopBB
  
  setBlock endBB

-- FOR loop: FOR i := start TO end DO ... ENDFOR
cgenStmt (SForTo (Ident var) start end body) = do
  -- Allocate loop variable
  loopVar <- alloca T.i32
  store loopVar (int start)
  
  loopBB <- createBlock (AST.Name "for.loop")
  bodyBB <- createBlock (AST.Name "for.body")
  endBB <- createBlock (AST.Name "for.end")
  
  br loopBB
  
  setBlock loopBB
  i <- load loopVar
  cond <- icmp IP.SLE i (int end)
  cbr cond bodyBB endBB
  
  setBlock bodyBB
  mapM_ cgenStmt body
  
  -- Increment
  iVal <- load loopVar
  next <- iadd iVal (int 1)
  store loopVar next
  br loopBB
  
  setBlock endBB

-- RETURN
cgenStmt SReturn = do
  ret Nothing
  return ()

-- Function call: CALL func(args)
cgenStmt (SCallparamExp (Ident name) args) = do
  argVals <- mapM cgenExpr args
  let fn = ConstantOperand $ C.GlobalReference (T.ptr $ T.FunctionType T.void [] False) (AST.Name $ fromString name)
  callVoid fn argVals
  return ()

-- WRITE statement (simplified)
cgenStmt (SWrite (Ident dev) items) = do
  -- Simplified: just ignore for now
  return ()

-- Default: ignore unimplemented statements
cgenStmt _ = return ()

-- | Generate code for expressions
cgenExpr :: Expression -> Codegen Operand

-- Integer literal
cgenExpr (EInt n) = return $ int n

-- Real/Double literal
cgenExpr (EDouble d) = return $ double d

-- Variable reference
cgenExpr (EIdent (Ident name)) = do
  -- Simplified: would need symbol table lookup
  ptr <- alloca T.double
  load ptr

-- Binary operations
cgenExpr (EAdd e1 e2) = do
  v1 <- cgenExpr e1
  v2 <- cgenExpr e2
  fadd v1 v2

cgenExpr (ESub e1 e2) = do
  v1 <- cgenExpr e1
  v2 <- cgenExpr e2
  fsub v1 v2

cgenExpr (EMul e1 e2) = do
  v1 <- cgenExpr e1
  v2 <- cgenExpr e2
  fmul v1 v2

cgenExpr (EDiv e1 e2) = do
  v1 <- cgenExpr e1
  v2 <- cgenExpr e2
  fdiv v1 v2

-- Comparison operations
cgenExpr (EEqual e1 e2) = do
  v1 <- cgenExpr e1
  v2 <- cgenExpr e2
  fcmp FP.OEQ v1 v2

cgenExpr (ENEqual e1 e2) = do
  v1 <- cgenExpr e1
  v2 <- cgenExpr e2
  fcmp FP.ONE v1 v2

cgenExpr (ELess e1 e2) = do
  v1 <- cgenExpr e1
  v2 <- cgenExpr e2
  fcmp FP.OLT v1 v2

cgenExpr (Egret e1 e2) = do
  v1 <- cgenExpr e1
  v2 <- cgenExpr e2
  fcmp FP.OGT v1 v2

cgenExpr (ELeq e1 e2) = do
  v1 <- cgenExpr e1
  v2 <- cgenExpr e2
  fcmp FP.OLE v1 v2

cgenExpr (Egeq e1 e2) = do
  v1 <- cgenExpr e1
  v2 <- cgenExpr e2
  fcmp FP.OGE v1 v2

-- Logical operations
cgenExpr (EAnd e1 e2) = do
  v1 <- cgenExpr e1
  v2 <- cgenExpr e2
  instr $ And v1 v2 []

cgenExpr (EOR e1 e2) = do
  v1 <- cgenExpr e1
  v2 <- cgenExpr e2
  instr $ Or v1 v2 []

cgenExpr (ENot e1 e2) = do
  v <- cgenExpr e1
  instr $ Xor v (ConstantOperand $ C.Int 1 1) []

-- Default
cgenExpr _ = return $ double 0.0

-- | Generate routine definition
genRoutine :: RoutineDefinitionBlock -> AST.Definition
genRoutine (RoutineDefineBlock (Ident name) _ decls stms _) =
  AST.GlobalDefinition $ G.functionDefaults
    { G.name = AST.Name (fromString name)
    , G.returnType = T.void
    , G.parameters = ([], False)  -- TODO: Extract parameters
    , G.basicBlocks = createBlocks $ do
        entry <- createBlock (AST.Name "entry")
        setBlock entry
        
        -- Declare variables
        mapM_ declareFromDeclBlock decls
        
        -- Generate statements
        mapM_ cgenStmt stms
        
        -- Return
        ret Nothing
    }
