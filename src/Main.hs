module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.FilePath (takeDirectory)

import LexKarel
import ParKarel
import AbsKarel
import ErrM

import Precompiler
import ModuleResolver
import CodeGen
import LLVM

-- | Main entry point
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      hPutStrLn stderr "Usage: karelc <file.kl> [options]"
      hPutStrLn stderr ""
      hPutStrLn stderr "Options:"
      hPutStrLn stderr "  -o <file>         Output file"
      hPutStrLn stderr "  -emit-llvm        Generate LLVM IR"
      hPutStrLn stderr "  -S                Generate assembly"
      hPutStrLn stderr "  --aku-path <path> Path to AKU library (default: ../aku)"
      hPutStrLn stderr "  --precompile      Run precompiler only"
      hPutStrLn stderr "  --no-precompile   Skip precompiler"
      exitFailure
    
    (file:flags) -> do
      contents <- readFile file
      case pKarel (myLexer contents) of
        Bad err -> do
          hPutStrLn stderr $ "Parse error: " ++ err
          exitFailure
        
        Ok tree -> do
          let akuPath = getAkuPath flags (takeDirectory file ++ "/../aku")
          let includePaths = generateIncludePaths akuPath
          
          -- Run precompiler if not disabled
          processedTree <- if "--no-precompile" `elem` flags
            then return tree
            else do
              let precompiler = initPrecompiler includePaths
              (_, processed) <- preprocess precompiler tree
              return processed
          
          -- Generate LLVM code
          let llvmMod = codegenModule processedTree
          
          -- Process output flags
          if "--precompile" `elem` flags
            then do
              putStrLn "Precompiler completed successfully"
              exitSuccess
            else do
              processFlags file flags llvmMod
              exitSuccess

-- | Get AKU library path from flags
getAkuPath :: [String] -> FilePath -> FilePath
getAkuPath [] defaultPath = defaultPath
getAkuPath ("--aku-path":path:_) _ = path
getAkuPath (_:rest) defaultPath = getAkuPath rest defaultPath

-- | Process command line flags
processFlags :: FilePath -> [String] -> AST.Module -> IO ()
processFlags inputFile flags mod
  | "-emit-llvm" `elem` flags = do
      let outputFile = getOutputFile flags inputFile ".ll"
      writeLLVMIR outputFile mod
      putStrLn $ "Generated LLVM IR: " ++ outputFile
  
  | "-S" `elem` flags = do
      let outputFile = getOutputFile flags inputFile ".s"
      putStrLn $ "Generating assembly: " ++ outputFile
      writeLLVMIR (replaceExtension outputFile ".ll") mod
  
  | otherwise = do
      let outputFile = getOutputFile flags inputFile ".bc"
      toBitcode outputFile mod
      putStrLn $ "Generated bitcode: " ++ outputFile

-- | Get output file name
getOutputFile :: [String] -> FilePath -> String -> FilePath
getOutputFile flags inputFile defaultExt =
  case getOutput flags of
    Just file -> file
    Nothing -> replaceExtension inputFile defaultExt

-- | Extract -o flag value
getOutput :: [String] -> Maybe FilePath
getOutput [] = Nothing
getOutput ("-o":file:_) = Just file
getOutput (_:rest) = getOutput rest

-- | Replace file extension
replaceExtension :: FilePath -> String -> FilePath
replaceExtension path newExt = 
  let base = takeWhile (/= '.') path
  in base ++ newExt
