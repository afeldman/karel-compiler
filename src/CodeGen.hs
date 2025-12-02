{-# LANGUAGE OverloadedStrings #-}

module CodeGen where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Global as G
import AbsKarel
import BuiltIns
import qualified Data.Map as Map

-- | Generate LLVM module from Karel AST
codegenModule :: Karel -> AST.Module
codegenModule karel = AST.defaultModule
  { AST.moduleName = "karel"
  , AST.moduleDefinitions = generateDefinitions karel
  }

-- | Generate LLVM definitions
generateDefinitions :: Karel -> [AST.Definition]
generateDefinitions (KarelGrammer (Ident progName) _ _ decls _ stms _ _ routines) =
  -- Main function
  mainDef : 
  -- Built-in function declarations
  builtInDefs ++
  -- User-defined routines
  routineDefs
  where
    mainDef = AST.GlobalDefinition $ G.functionDefaults
      { G.name = AST.Name "main"
      , G.returnType = T.i32
      , G.basicBlocks = []  -- TODO: Implement
      }
    
    builtInDefs = generateBuiltInDecls
    routineDefs = map generateRoutine routines

-- | Generate built-in function declarations
generateBuiltInDecls :: [AST.Definition]
generateBuiltInDecls =
  [ AST.GlobalDefinition $ G.functionDefaults
      { G.name = AST.Name "printf"
      , G.returnType = T.i32
      , G.parameters = ([G.Parameter (T.ptr T.i8) (AST.Name "fmt") []], True)
      , G.basicBlocks = []
      }
  , AST.GlobalDefinition $ G.functionDefaults
      { G.name = AST.Name "strlen"
      , G.returnType = T.i64
      , G.parameters = ([G.Parameter (T.ptr T.i8) (AST.Name "s") []], False)
      , G.basicBlocks = []
      }
  , AST.GlobalDefinition $ G.functionDefaults
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

-- | Generate routine definition
generateRoutine :: RoutineDefinitionBlock -> AST.Definition
generateRoutine (RoutineDefBlock (Ident name) _ _ _ stms _) =
  AST.GlobalDefinition $ G.functionDefaults
    { G.name = AST.Name (fromString name)
    , G.returnType = T.VoidType
    , G.basicBlocks = []  -- TODO: Implement statement generation
    }

fromString :: String -> AST.Name
fromString = AST.Name . fromString
