module Lib
  ( someFunc
  ) where


import Data.List
import Language.Haskell.Interpreter

someFunc :: IO ()
someFunc = do
  r <- runInterpreter testHint
  case r of
    Left err -> putStrLn $ errorString err
    Right () -> return ()

testHint :: Interpreter ()
testHint =
  do
    setImportsQ [("Prelude", Nothing)]
    emptyLine
    let expr1 = "[1,2] ++ [3]"
    say $ "type of " ++ expr1 ++ " is:"
    say =<< typeOf expr1
    emptyLine


errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

say :: String -> Interpreter ()
say = liftIO . putStrLn

emptyLine :: Interpreter ()
emptyLine = say ""
