{-# LANGUAGE OverloadedStrings #-}

module BuiltIns where

import qualified Data.Map as Map
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Constant as C

-- | Built-in functions for Karel
data BuiltInFunction = BuiltInFunction
  { builtInName :: String
  , builtInParams :: [(String, T.Type)]
  , builtInReturn :: T.Type
  , builtInImpl :: [AST.Operand] -> AST.Operand  -- LLVM implementation
  }

-- | All built-in functions
builtInFunctions :: Map.Map String BuiltInFunction
builtInFunctions = Map.fromList
  [ ("WRITE", writeFunc)
  , ("WRITELN", writelnFunc)
  , ("STR_LEN", strLenFunc)
  , ("SUB_STR", subStrFunc)
  , ("ARRAY_LEN", arrayLenFunc)
  , ("ABS", absFunc)
  , ("SQRT", sqrtFunc)
  , ("SIN", sinFunc)
  , ("COS", cosFunc)
  , ("TAN", tanFunc)
  , ("ATAN2", atan2Func)
  , ("TRUNC", truncFunc)
  , ("ROUND", roundFunc)
  ]

-- String functions
writeFunc :: BuiltInFunction
writeFunc = BuiltInFunction
  { builtInName = "WRITE"
  , builtInParams = [("msg", T.ptr T.i8)]
  , builtInReturn = T.i32
  , builtInImpl = \_ -> undefined  -- Implemented in CodeGen
  }

writelnFunc :: BuiltInFunction
writelnFunc = BuiltInFunction
  { builtInName = "WRITELN"
  , builtInParams = [("msg", T.ptr T.i8)]
  , builtInReturn = T.i32
  , builtInImpl = \_ -> undefined
  }

strLenFunc :: BuiltInFunction
strLenFunc = BuiltInFunction
  { builtInName = "STR_LEN"
  , builtInParams = [("str", T.ptr T.i8)]
  , builtInReturn = T.i32
  , builtInImpl = \_ -> undefined
  }

subStrFunc :: BuiltInFunction
subStrFunc = BuiltInFunction
  { builtInName = "SUB_STR"
  , builtInParams = [("str", T.ptr T.i8), ("pos", T.i32), ("len", T.i32)]
  , builtInReturn = T.ptr T.i8
  , builtInImpl = \_ -> undefined
  }

arrayLenFunc :: BuiltInFunction
arrayLenFunc = BuiltInFunction
  { builtInName = "ARRAY_LEN"
  , builtInParams = [("arr", T.ptr T.i32)]
  , builtInReturn = T.i32
  , builtInImpl = \_ -> undefined
  }

-- Math functions
absFunc :: BuiltInFunction
absFunc = BuiltInFunction
  { builtInName = "ABS"
  , builtInParams = [("x", T.double)]
  , builtInReturn = T.double
  , builtInImpl = \_ -> undefined
  }

sqrtFunc :: BuiltInFunction
sqrtFunc = BuiltInFunction
  { builtInName = "SQRT"
  , builtInParams = [("x", T.double)]
  , builtInReturn = T.double
  , builtInImpl = \_ -> undefined
  }

sinFunc :: BuiltInFunction
sinFunc = BuiltInFunction
  { builtInName = "SIN"
  , builtInParams = [("x", T.double)]
  , builtInReturn = T.double
  , builtInImpl = \_ -> undefined
  }

cosFunc :: BuiltInFunction
cosFunc = BuiltInFunction
  { builtInName = "COS"
  , builtInParams = [("x", T.double)]
  , builtInReturn = T.double
  , builtInImpl = \_ -> undefined
  }

tanFunc :: BuiltInFunction
tanFunc = BuiltInFunction
  { builtInName = "TAN"
  , builtInParams = [("x", T.double)]
  , builtInReturn = T.double
  , builtInImpl = \_ -> undefined
  }

atan2Func :: BuiltInFunction
atan2Func = BuiltInFunction
  { builtInName = "ATAN2"
  , builtInParams = [("y", T.double), ("x", T.double)]
  , builtInReturn = T.double
  , builtInImpl = \_ -> undefined
  }

truncFunc :: BuiltInFunction
truncFunc = BuiltInFunction
  { builtInName = "TRUNC"
  , builtInParams = [("x", T.double)]
  , builtInReturn = T.i32
  , builtInImpl = \_ -> undefined
  }

roundFunc :: BuiltInFunction
roundFunc = BuiltInFunction
  { builtInName = "ROUND"
  , builtInParams = [("x", T.double)]
  , builtInReturn = T.i32
  , builtInImpl = \_ -> undefined
  }

-- | Check if a function is built-in
isBuiltIn :: String -> Bool
isBuiltIn name = Map.member name builtInFunctions

-- | Get built-in function by name
getBuiltIn :: String -> Maybe BuiltInFunction
getBuiltIn name = Map.lookup name builtInFunctions
