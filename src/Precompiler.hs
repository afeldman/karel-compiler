{-# LANGUAGE OverloadedStrings #-}

module Precompiler where

import AbsKarel
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List (isPrefixOf)
import System.FilePath ((</>), takeDirectory, replaceExtension)
import System.Directory (doesFileExist)

-- | Precompiler state
data PrecompilerState = PrecompilerState
  { includePaths :: [FilePath]
  , resolvedModules :: Map.Map String Karel
  , builtInTypes :: Map.Map String Type
  , builtInRoutines :: Map.Map String RoutineDecl
  , definedMacros :: Map.Map String MacroValue
  , conditionalStack :: [Bool]  -- Track %IF/%IFDEF nesting
  } deriving (Show)

-- | Macro value (constant or expression)
data MacroValue 
  = MacroInt Integer
  | MacroReal Double
  | MacroString String
  | MacroExpr Expression
  deriving (Show, Eq)

-- | Initialize precompiler with AKU library path
initPrecompiler :: [FilePath] -> PrecompilerState
initPrecompiler paths = PrecompilerState
  { includePaths = paths
  , resolvedModules = Map.empty
  , builtInTypes = initBuiltInTypes
  , builtInRoutines = initBuiltInRoutines
  , definedMacros = Map.empty
  , conditionalStack = []
  }

-- | Initialize built-in types (INTEGER, REAL, STRING, etc.)
initBuiltInTypes :: Map.Map String Type
initBuiltInTypes = Map.fromList
  [ ("INTEGER", TInt)
  , ("REAL", TReal)
  , ("BOOLEAN", TBool)
  , ("STRING", TString Nothing)
  , ("BYTE", TByte)
  , ("SHORT", TShort)
  ]

-- | Initialize built-in routines (WRITE, READ, etc.)
initBuiltInRoutines :: Map.Map String RoutineDecl
initBuiltInRoutines = Map.fromList
  [ ("WRITE", createBuiltInRoutine "WRITE" [("msg", TString Nothing)])
  , ("WRITELN", createBuiltInRoutine "WRITELN" [("msg", TString Nothing)])
  , ("STR_LEN", createBuiltInRoutine "STR_LEN" [("s", TString Nothing)])
  , ("SUB_STR", createBuiltInRoutine "SUB_STR" 
      [("s", TString Nothing), ("pos", TInt), ("len", TInt)])
  , ("ARRAY_LEN", createBuiltInRoutine "ARRAY_LEN" [("arr", TInt)])
  ]
  where
    createBuiltInRoutine name params = 
      RoutineDeclWithParam (Ident name) (map toParam params) [] []
    toParam (n, t) = Param (Ident n) t

-- | Preprocess Karel program - resolve includes and expand modules
preprocess :: PrecompilerState -> Karel -> IO (PrecompilerState, Karel)
preprocess state (KarelGrammer ident optEnd directives decls begin stms end endIdent routines) = do
  -- Process all preprocessor directives
  (state', filteredDirectives, expandedDecls) <- processDirectives state directives decls
  
  -- Expand macros in declarations
  let expandedDeclsWithMacros = map (expandMacrosInDecl state') expandedDecls
  
  -- Expand built-in function calls and macros in statements
  let expandedStms = map (expandMacrosInStm state' . expandBuiltIns state') stms
  let expandedRoutines = map (expandRoutineBuiltIns state') routines
  
  return (state', KarelGrammer ident optEnd filteredDirectives 
          expandedDeclsWithMacros begin expandedStms end endIdent expandedRoutines)

-- | Process all preprocessor directives (%DEFINE, %IF, %INCLUDE, etc.)
processDirectives :: PrecompilerState -> [DirectiveBlock] -> [DeclBlock] 
                  -> IO (PrecompilerState, [DirectiveBlock], [DeclBlock])
processDirectives state directives decls = do
  (state', filteredDirs, expandedDecls) <- foldM processDirective (state, [], decls) directives
  return (state', reverse filteredDirs, expandedDecls)

-- | Process single preprocessor directive
processDirective :: (PrecompilerState, [DirectiveBlock], [DeclBlock]) -> DirectiveBlock 
                 -> IO (PrecompilerState, [DirectiveBlock], [DeclBlock])
processDirective (state, dirs, decls) dir = case dir of
  -- %DEFINE macro value
  DirDefineConst ident@(Ident name) intVal -> do
    let state' = state { definedMacros = Map.insert name (MacroInt intVal) (definedMacros state) }
    putStrLn $ "Precompiler: Defined " ++ name ++ " = " ++ show intVal
    return (state', dirs, decls)
  
  DirDefine ident@(Ident name) expr -> do
    let state' = state { definedMacros = Map.insert name (MacroExpr expr) (definedMacros state) }
    putStrLn $ "Precompiler: Defined " ++ name ++ " = <expr>"
    return (state', dirs, decls)
  
  -- %UNDEF macro
  DirUndef (Ident name) -> do
    let state' = state { definedMacros = Map.delete name (definedMacros state) }
    putStrLn $ "Precompiler: Undefined " ++ name
    return (state', dirs, decls)
  
  -- %IFDEF name
  DirIfdef (Ident name) -> do
    let isDefined = Map.member name (definedMacros state)
    let state' = state { conditionalStack = isDefined : conditionalStack state }
    putStrLn $ "Precompiler: %IFDEF " ++ name ++ " = " ++ show isDefined
    return (state', dirs, decls)
  
  -- %IFNDEF name
  DirIfndef (Ident name) -> do
    let isDefined = Map.member name (definedMacros state)
    let state' = state { conditionalStack = not isDefined : conditionalStack state }
    putStrLn $ "Precompiler: %IFNDEF " ++ name ++ " = " ++ show (not isDefined)
    return (state', dirs, decls)
  
  -- %IF expr
  DirIf expr -> do
    let result = evaluateCondition state expr
    let state' = state { conditionalStack = result : conditionalStack state }
    putStrLn $ "Precompiler: %IF <expr> = " ++ show result
    return (state', dirs, decls)
  
  -- %ELSE
  DirElse -> do
    case conditionalStack state of
      (cond:rest) -> do
        let state' = state { conditionalStack = not cond : rest }
        putStrLn "Precompiler: %ELSE"
        return (state', dirs, decls)
      [] -> do
        putStrLn "Precompiler: Warning - %ELSE without %IF"
        return (state, dirs, decls)
  
  -- %ENDIF
  DirEndif -> do
    case conditionalStack state of
      (_:rest) -> do
        let state' = state { conditionalStack = rest }
        putStrLn "Precompiler: %ENDIF"
        return (state', dirs, decls)
      [] -> do
        putStrLn "Precompiler: Warning - %ENDIF without %IF"
        return (state, dirs, decls)
  
  -- %INCLUDE file
  DirInclude (Text path) -> do
    if isConditionActive state
      then do
        let cleanPath = filter (/= '"') path
        (state', decls') <- processInclude state decls cleanPath
        return (state', dirs, decls')
      else do
        putStrLn $ "Precompiler: Skipping %INCLUDE (conditional false)"
        return (state, dirs, decls)
  
  -- Other directives pass through
  _ -> return (state, dir : dirs, decls)

-- | Check if current conditional context is active
isConditionActive :: PrecompilerState -> Bool
isConditionActive state = case conditionalStack state of
  [] -> True  -- No conditionals, always active
  (cond:_) -> cond  -- Top of stack determines if active

-- | Process single include file
processInclude :: PrecompilerState -> [DeclBlock] -> String -> IO (PrecompilerState, [DeclBlock])
processInclude state decls includePath = do
  let searchPaths = map (</> includePath) (includePaths state)
  let searchPathsKl = map (`replaceExtension` ".kl") searchPaths
  
  maybeFile <- findFirstExisting (searchPaths ++ searchPathsKl)
  
  case maybeFile of
    Just filePath -> do
      -- TODO: Parse included file and extract declarations
      -- For now, return unchanged
      putStrLn $ "Precompiler: Including " ++ filePath
      return (state, decls)
    Nothing -> do
      putStrLn $ "Precompiler: Warning - Include not found: " ++ includePath
      return (state, decls)

-- | Find first existing file from list
findFirstExisting :: [FilePath] -> IO (Maybe FilePath)
findFirstExisting [] = return Nothing
findFirstExisting (p:ps) = do
  exists <- doesFileExist p
  if exists
    then return (Just p)
    else findFirstExisting ps

-- | Evaluate conditional expression for %IF
evaluateCondition :: PrecompilerState -> Expression -> Bool
evaluateCondition state expr = case expr of
  EInt n -> n /= 0
  EDouble r -> r /= 0.0
  -- ETrue/EFalse don't exist in Expression, use integer comparison
  
  -- Variable reference - check if defined
  EIdent (Ident name) -> Map.member name (definedMacros state)
  
  -- Comparison operators
  EEqual expr1 expr2 -> 
    let v1 = evalExprToInt state expr1
        v2 = evalExprToInt state expr2
    in v1 == v2
  ENEqual expr1 expr2 ->
    let v1 = evalExprToInt state expr1
        v2 = evalExprToInt state expr2
    in v1 /= v2
  ELess expr1 expr2 ->
    let v1 = evalExprToInt state expr1
        v2 = evalExprToInt state expr2
    in v1 < v2
  ELeq expr1 expr2 ->
    let v1 = evalExprToInt state expr1
        v2 = evalExprToInt state expr2
    in v1 <= v2
  Egret expr1 expr2 ->
    let v1 = evalExprToInt state expr1
        v2 = evalExprToInt state expr2
    in v1 > v2
  Egeq expr1 expr2 ->
    let v1 = evalExprToInt state expr1
        v2 = evalExprToInt state expr2
    in v1 >= v2
  
  -- Logical operators
  EAnd expr1 expr2 -> evaluateCondition state expr1 && evaluateCondition state expr2
  EOR expr1 expr2 -> evaluateCondition state expr1 || evaluateCondition state expr2
  ENot expr1 expr2 -> not (evaluateCondition state expr1)  -- Karel's NOT is binary in expression
  
  _ -> False  -- Default to false for unknown expressions

-- | Evaluate expression to integer (for conditions)
evalExprToInt :: PrecompilerState -> Expression -> Integer
evalExprToInt state expr = case expr of
  EInt n -> n
  EDouble r -> round r
  
  EIdent (Ident name) -> case Map.lookup name (definedMacros state) of
    Just (MacroInt n) -> n
    Just (MacroReal r) -> round r
    _ -> 0
  
  EAdd expr1 expr2 -> evalExprToInt state expr1 + evalExprToInt state expr2
  ESub expr1 expr2 -> evalExprToInt state expr1 - evalExprToInt state expr2
  EMul expr1 expr2 -> evalExprToInt state expr1 * evalExprToInt state expr2
  EAdiv expr1 expr2 -> 
    let v2 = evalExprToInt state expr2
    in if v2 /= 0 then evalExprToInt state expr1 `div` v2 else 0
  
  EMOD expr1 expr2 -> 
    let v2 = evalExprToInt state expr2
    in if v2 /= 0 then evalExprToInt state expr1 `mod` v2 else 0
  
  EMinus expr' -> negate (evalExprToInt state expr')
  EPlus expr' -> evalExprToInt state expr'
  
  _ -> 0

-- | Expand macros in declarations
expandMacrosInDecl :: PrecompilerState -> DeclBlock -> DeclBlock
expandMacrosInDecl state decl = case decl of
  DeclConst (CDB consts) ->
    DeclConst (CDB (map expandConstDecl consts))
  -- Variables don't have default expressions in Karel grammar
  _ -> decl
  where
    expandConstDecl (CDIdent ident ident2 optEnd) = CDIdent ident ident2 optEnd
    expandConstDecl (CDLiteral ident lit optEnd) = CDLiteral ident lit optEnd

-- | Expand macros in expressions
expandMacrosInExpr :: PrecompilerState -> Expression -> Expression
expandMacrosInExpr state expr = case expr of
  EIdent (Ident name) -> case Map.lookup name (definedMacros state) of
    Just (MacroInt n) -> EInt n
    Just (MacroReal r) -> EDouble r
    Just (MacroExpr e) -> expandMacrosInExpr state e
    _ -> expr
  
  EAdd e1 e2 -> EAdd (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  ESub e1 e2 -> ESub (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  EMul e1 e2 -> EMul (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  EAdiv e1 e2 -> EAdiv (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  EMOD e1 e2 -> EMOD (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  EDiv e1 e2 -> EDiv (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  EMinus e -> EMinus (expandMacrosInExpr state e)
  EPlus e -> EPlus (expandMacrosInExpr state e)
  
  EEqual e1 e2 -> EEqual (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  ENEqual e1 e2 -> ENEqual (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  ELess e1 e2 -> ELess (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  ELeq e1 e2 -> ELeq (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  Egret e1 e2 -> Egret (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  Egeq e1 e2 -> Egeq (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  
  EAnd e1 e2 -> EAnd (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  EOR e1 e2 -> EOR (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  ENot e1 e2 -> ENot (expandMacrosInExpr state e1) (expandMacrosInExpr state e2)
  
  EBrack e -> EBrack (expandMacrosInExpr state e)
  
  _ -> expr

-- | Expand macros in statements
expandMacrosInStm :: PrecompilerState -> Stm -> Stm
expandMacrosInStm state stm = case stm of
  SAssign ident expr -> SAssign ident (expandMacrosInExpr state expr)
  
  SIfThen cond thenStms ->
    SIfThen (expandMacrosInExpr state cond) (map (expandMacrosInStm state) thenStms)
  
  SIfThenElse cond thenStms elseStms ->
    SIfThenElse (expandMacrosInExpr state cond) 
                (map (expandMacrosInStm state) thenStms)
                (map (expandMacrosInStm state) elseStms)
  
  SForTo ident from to stms ->
    SForTo ident from to (map (expandMacrosInStm state) stms)
  
  SForDownTo ident from to stms ->
    SForDownTo ident from to (map (expandMacrosInStm state) stms)
  
  SWhile cond stms ->
    SWhile (expandMacrosInExpr state cond) (map (expandMacrosInStm state) stms)
  
  SRepeat stms cond ->
    SRepeat (map (expandMacrosInStm state) stms) (expandMacrosInExpr state cond)
  
  SCallparamExp ident exprs ->
    SCallparamExp ident (map (expandMacrosInExpr state) exprs)
  
  _ -> stm

-- | Expand built-in function calls in statements
expandBuiltIns :: PrecompilerState -> Stm -> Stm
expandBuiltIns state stm = case stm of
  -- Expand WRITE/WRITELN to __builtin_WRITE
  SCallparamExp (Ident name) args | name `elem` ["WRITE", "WRITELN"] ->
    SCallparamExp (Ident ("__builtin_" ++ name)) args
  
  -- Expand string functions
  SCallparamExp (Ident name) args | isBuiltInFunction state name ->
    SCallparamExp (Ident ("__builtin_" ++ name)) args
  
  -- Recursively process compound statements
  SIfThen cond thenStms ->
    SIfThen cond (map (expandBuiltIns state) thenStms)
  
  SIfThenElse cond thenStms elseStms ->
    SIfThenElse cond (map (expandBuiltIns state) thenStms) (map (expandBuiltIns state) elseStms)
  
  SForTo ident from to stms ->
    SForTo ident from to (map (expandBuiltIns state) stms)
  
  SForDownTo ident from to stms ->
    SForDownTo ident from to (map (expandBuiltIns state) stms)
  
  SWhile cond stms ->
    SWhile cond (map (expandBuiltIns state) stms)
  
  SRepeat stms cond ->
    SRepeat (map (expandBuiltIns state) stms) cond
  
  -- Default: return unchanged
  _ -> stm

-- | Expand built-ins in routine definitions
expandRoutineBuiltIns :: PrecompilerState -> RoutineDefinitionBlock -> RoutineDefinitionBlock
expandRoutineBuiltIns state (RoutineDefineBlock ident optEnd decls stms endIdent) =
  RoutineDefineBlock ident optEnd 
                     (map (expandMacrosInDecl state) decls)
                     (map (expandMacrosInStm state . expandBuiltIns state) stms) 
                     endIdent

-- | Check if function name is a built-in
isBuiltInFunction :: PrecompilerState -> String -> Bool
isBuiltInFunction state name = Map.member name (builtInRoutines state)

-- | Generate include search paths from AKU project
generateIncludePaths :: FilePath -> [FilePath]
generateIncludePaths akuRoot =
  [ akuRoot
  , akuRoot </> "string"
  , akuRoot </> "string" </> "include"
  , akuRoot </> "math"
  , akuRoot </> "math" </> "include"
  , akuRoot </> "byte"
  , akuRoot </> "byte" </> "include"
  , akuRoot </> "time"
  , akuRoot </> "time" </> "include"
  , akuRoot </> "logic"
  , akuRoot </> "char"
  , akuRoot </> "hex"
  , akuRoot </> "fuzzy"
  , akuRoot </> "gravity"
  , akuRoot </> "filehash"
  ]
