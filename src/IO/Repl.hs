module IO.Repl where

import Common
import Core.Tableau (isSatisfiable)
import IO.Parser (parseProps)
import System.Console.Haskeline

repl :: InputT IO ()
repl = do
  minput <- getInputLine "tableau> "
  case minput of
    Nothing -> outputStrLn "Bye!" -- Ctrl-D
    Just ":q" -> outputStrLn "Bye!"
    Just input -> do
      case parseProps input of
        Left err -> outputStrLn $ "Parse error: " ++ show err
        Right props -> outputStrLn $ "result: " ++ show (isSatisfiable props)
      repl

runRepl :: IO ()
runRepl = runInputT defaultSettings repl
