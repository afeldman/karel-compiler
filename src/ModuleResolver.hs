{-# LANGUAGE OverloadedStrings #-}

module ModuleResolver where

import AbsKarel
import qualified Data.Map as Map
import System.FilePath
import System.Directory

-- | Module resolution context
data ModuleContext = ModuleContext
  { moduleMap :: Map.Map String Karel  -- Resolved modules
  , searchPaths :: [FilePath]           -- Search paths for modules
  } deriving (Show)

-- | Initialize module context
initModuleContext :: [FilePath] -> ModuleContext
initModuleContext paths = ModuleContext
  { moduleMap = Map.empty
  , searchPaths = paths
  }

-- | Resolve module by name
resolveModule :: ModuleContext -> String -> IO (Maybe Karel)
resolveModule ctx moduleName = do
  -- Check if already resolved
  case Map.lookup moduleName (moduleMap ctx) of
    Just prog -> return (Just prog)
    Nothing -> searchAndParseModule ctx moduleName

-- | Search for module file and parse it
searchAndParseModule :: ModuleContext -> String -> IO (Maybe Karel)
searchAndParseModule ctx moduleName = do
  let candidates = generateCandidatePaths (searchPaths ctx) moduleName
  findAndParse candidates
  where
    findAndParse [] = return Nothing
    findAndParse (path:paths) = do
      exists <- doesFileExist path
      if exists
        then do
          -- TODO: Parse Karel file
          putStrLn $ "Found module: " ++ path
          return Nothing  -- Placeholder
        else findAndParse paths

-- | Generate candidate file paths for module
generateCandidatePaths :: [FilePath] -> String -> [FilePath]
generateCandidatePaths searchPaths moduleName =
  [ searchPath </> moduleName <.> ext
  | searchPath <- searchPaths
  , ext <- ["kl", "h.kl", "t.kl"]
  ]

-- | Add resolved module to context
addModule :: ModuleContext -> String -> Karel -> ModuleContext
addModule ctx name prog = ctx { moduleMap = Map.insert name prog (moduleMap ctx) }

-- | Get all declared routines from a module
getModuleRoutines :: Karel -> [RoutineDecl]
getModuleRoutines (KarelGrammer _ _ _ decls _ _ _ _ _) = extractRoutineDecls decls
  where
    extractRoutineDecls [] = []
    extractRoutineDecls (DeclRoutine r : rest) = r : extractRoutineDecls rest
    extractRoutineDecls (_ : rest) = extractRoutineDecls rest

-- | Get all types from a module
getModuleTypes :: Karel -> [TypeDecl]
getModuleTypes (KarelGrammer _ _ _ decls _ _ _ _ _) = extractTypeDecls decls
  where
    extractTypeDecls [] = []
    extractTypeDecls (DeclType (TypeDeclBlock types) : rest) = types ++ extractTypeDecls rest
    extractTypeDecls (_ : rest) = extractTypeDecls rest
