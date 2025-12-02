{-# LANGUAGE OverloadedStrings #-}

module LLVM where

import qualified LLVM.AST as AST
import LLVM.Context
import LLVM.Module
import qualified LLVM.Pretty as Pretty

import Data.Text.Lazy (unpack)

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
