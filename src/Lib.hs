module Lib
  ( putError, typecheckExpression
  ) where

import qualified Data.List as L
import Language.Haskell.Interpreter

errorString :: InterpreterError -> String
errorString (WontCompile es) = L.intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

-- TODO: improve this type signature, this is compiler-generated
putError :: MonadIO m => InterpreterError -> m ()
putError err = do
  liftIO . putStrLn $ errorString err

typecheckExpressionHelper :: String -> Interpreter String
typecheckExpressionHelper code = do
  setImports ["Prelude"]
  r <- typeOf code
  return r

-- TODO: improve this type signature, this is compiler-generated
typecheckExpression :: MonadIO m => String -> m (Either InterpreterError String)
typecheckExpression code = do
  liftIO . runInterpreter $ (typecheckExpressionHelper code)
